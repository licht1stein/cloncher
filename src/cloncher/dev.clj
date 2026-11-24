(ns cloncher.dev
  "Persistent development window for experimenting with launcher sessions."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.cui :as cui]
            [cloncher.main :as app]
            [cloncher.menu :as menu]
            [cloncher.note :as note]
            [cloncher.palette :as palette]
            [cloncher.panel :as panel]
            [cloncher.key-overlay :as key-overlay]
            [cloncher.timer-overlay :as timer-overlay]
            [cloncher.view :as view]
            [jolt.nrepl :as nrepl]))

(defonce ^:private control (atom {}))
(defonce ^:private redraw (atom nil))
(defonce ^:private session (app/session))
(defonce ^:private reload-lock (Object.))
(defonce ^:private configuration (atom nil))
(defonce ^:private note-buffer (delay (cui/input-buffer)))
(defonce ^:private note-entry (atom nil))

(defn state [] (app/state session))

(defn redraw! []
  (when-let [wake @redraw] (wake)))

(defn reload-config!
  "Load the current EDN config and refresh the open preview on its frame thread."
  []
  (reset! configuration (menu/load-configuration!))
  (swap! control assoc :reconfigure true)
  (redraw!))

(defn reload!
  "Reload presentation and session code in the running window."
  []
  (locking reload-lock
    (doseq [namespace '[cloncher.json cloncher.process cloncher.config cloncher.persistence cloncher.socket cloncher.daemon
                        cloncher.cui cloncher.panel cloncher.palette cloncher.search cloncher.icons
                        cloncher.desktop cloncher.menu cloncher.sources cloncher.commands
                        cloncher.files cloncher.preview cloncher.edit cloncher.lisp
                        cloncher.machine cloncher.usage cloncher.view-cache cloncher.view
                        cloncher.note cloncher.main cloncher.dev]]
      (require namespace :reload)))
  (reload-config!))

(def ^:private view-kinds
  #{:off :root :apps :menu :files :file :projects :project :project-add :tasks :dmenu :secret})

(defn show!
  "Queue a view on the CUI thread. Accept a view keyword or a map such as
  {:kind :apps :initial-query \"edit\" :fixture {:rows [{:label \"Editor\"}]}}."
  [view]
  (let [target (if (keyword? view) {:kind view} view)]
    (when-not (contains? view-kinds (:kind target))
      (throw (ex-info "Unknown launcher preview view" {:view (:kind target)})))
    (swap! control assoc :open target)
    (redraw!)))

(defn off! [] (show! :off))

(defn screenshot!
  "Capture the running dev window. Return the absolute PNG path after CUI writes it."
  ([] (screenshot! (str ".dev/screenshots/cloncher-" (System/currentTimeMillis) ".png")))
  ([path]
   (let [path (fs/absolutize path)
         result (promise)
         request {:path (str path) :result result :phase :queued}]
     (when-not @redraw (throw (ex-info "The dev window is closed" {})))
     (fs/create-dirs (fs/parent path))
     (let [[before _] (swap-vals! control #(if (:capture %) % (assoc % :capture request)))]
       (when (:capture before) (throw (ex-info "A dev screenshot is already pending" {}))))
     (redraw!)
     (let [captured (deref result 5000 ::timeout)]
       (cond
         (= ::timeout captured)
         (throw (ex-info "CUI screenshot timed out" {:path (str path)}))
         (instance? Throwable captured) (throw captured)
         :else captured)))))

(defn- finish-screenshot! [{:keys [result]} value]
  (swap! control dissoc :capture)
  (deliver result value))

(defn- screenshot-frame! [ui]
  (when-let [{:keys [path phase] :as request} (:capture @control)]
    (case phase
      :queued
      (if (= :secret (app/active-kind session))
        (finish-screenshot! request (ex-info "Choose a non-secret preview to capture" {}))
        (do
          (cui/capture! path)
          (swap! control assoc-in [:capture :phase] :writing)
          (cui/redraw! ui)))
      :writing
      (case (cui/capture-result)
        1 (finish-screenshot! request path)
        -1 (finish-screenshot! request (ex-info "CUI could not write the screenshot" {:path path}))
        (cui/redraw! ui)))))

(defn- off-panel! [ui]
  (cui/column! ui "launcher-off" {:width view/preview-width :height 468 :surface? true})
  (try
    (cui/heading! ui "Launcher is off" 4)
    (finally (cui/end! ui))))

(defn- open-preview! [ui buffer definition]
  (cui/clear-input! buffer)
  (app/clear-preview! session ui)
  (app/show! session definition)
  (cui/redraw! ui))

(defn- spacer! [ui id]
  (cui/column! ui id {:height 0 :pad 0 :gap 0})
  (cui/end! ui))

(defn- section-heading!
  "A section title with an optional button on the right; return whether it was pressed."
  [ui id title button primary?]
  (cui/bar! ui id)
  (try
    (cui/heading! ui title 3)
    (cui/spacer! ui)
    (when button (cui/button! ui button primary?))
    (finally (cui/end! ui))))

(defn- wallpaper!
  "A rounded backdrop of palette stripes behind `body`, so translucent surfaces show
  what they let through. `height` is :fit, or points for content that fills."
  [ui id height body]
  (panel/begin! ui id {:stack? true :height height :radius 16 :clip? true})
  (try
    (panel/begin! ui (str id "-stripes") {:row? true :height :fill})
    (doseq [[i color] (map-indexed vector [:accent :danger :badge-text :selected-text])]
      (panel/begin! ui (str id "-stripe-" i) {:height :fill :background color})
      (cui/end! ui))
    (cui/end! ui)
    (panel/begin! ui (str id "-content") {:height (if (number? height) :fill :fit)
                                          :pad-x 32 :pad-y 32 :gap 16})
    (try (body) (finally (cui/end! ui)))
    (finally (cui/end! ui))))

(defn- launcher-section! [ui buffer]
  (let [off? (= :off (app/status session))]
    (when (section-heading! ui "launcher-heading" "Launcher" (if off? "Open launcher" "Turn off") off?)
      (open-preview! ui buffer (if off? {:kind :root} {:kind :off})))
    (wallpaper! ui "launcher-wallpaper" :fit
      #(do (cui/bar! ui "launcher-centered")
           (try
             (cui/spacer! ui)
             (if (= :on (app/status session))
               (do
                 (cui/apply-colors! ui (palette/load-colors))
                 (app/draw-frame! session ui buffer {:preview? true})
                 (when (= :complete (app/status session))
                   (open-preview! ui buffer {:kind :off})))
               (off-panel! ui))
             (cui/spacer! ui)
             (finally (cui/end! ui)))))))

(defn- note-section!
  "The quick-note card. Saving shows the entry instead of writing the scratchpad."
  [ui]
  (section-heading! ui "note-heading" "Quick note" nil false)
  (cui/apply-colors! ui (palette/load-colors))
  (let [buffer @note-buffer
        now (java.time.LocalDateTime/now)]
    (wallpaper! ui "note-wallpaper" (+ note/height 64)
      #(case (note/render! ui buffer {:title "Scratchpad" :at now})
         :save (do (reset! note-entry (or (note/entry (cui/input-value buffer) now) "Nothing to append"))
                   (cui/clear-input! buffer)
                   (cui/redraw! ui))
         :cancel (do (reset! note-entry "Cancelled") (cui/clear-input! buffer) (cui/redraw! ui))
         nil))
    (cui/wake-in! ui (- 60.0 (.getSecond now))))
  (when-let [entry @note-entry]
    (cui/caption! ui (str "Would append · " entry))))

(defn- timers-section! [ui]
  (section-heading! ui "timers-heading" "Countdown and stopwatch" nil false)
  (cui/apply-colors! ui (palette/load-colors))
  (wallpaper! ui "timers-wallpaper" :fit #(timer-overlay/preview! ui)))

(defn- keys-section!
  "The key display's card, drawn by its own renderer from a sample of typing and chords."
  [ui]
  (section-heading! ui "keys-heading" "Key display" nil false)
  (cui/apply-colors! ui (palette/load-colors))
  (wallpaper! ui "keys-wallpaper" :fit
    #(do (cui/bar! ui "keys-centered")
         (try
           (cui/spacer! ui)
           (panel/begin! ui "keys-card" {:width key-overlay/width :height key-overlay/height})
           (try (key-overlay/render! ui {:entries [{:text "hello"} {:sequence [[#{:ctrl} 30]] :count 1}
                                                   {:sequence [[#{} 105]] :count 3}
                                                   {:sequence [[#{:ctrl} 46] [#{} 46]] :count 1 :emacs? true}]
                                         :at (System/currentTimeMillis)}
                                     (System/currentTimeMillis))
                (finally (cui/end! ui)))
           (cui/spacer! ui)
           (finally (cui/end! ui))))))

(defn- render-preview!
  "Center the previews: side by side when the window is wide enough, else stacked
  in one column of the launcher's width. The page scrolls when it runs out of height."
  [ui buffer]
  (let [{:keys [open reconfigure]} (first (swap-vals! control dissoc :open :reconfigure))
        _ (cond open (open-preview! ui buffer open)
                reconfigure (app/reconfigure! session))
        launcher-width (+ view/preview-width 112)
        widgets-width (+ note/width 112)
        side? (>= (cui/width ui) (+ launcher-width widgets-width 32))
        column! (fn [id width] (cui/column! ui id {:width width :height 0 :pad 24 :gap 14}))]
    (cui/scroll! ui "development-page")
    (try
      (cui/bar! ui "development-window")
      (try
        (spacer! ui "left-gutter")
        (if side?
          (do (column! "launcher-column" launcher-width)
              (try (launcher-section! ui buffer) (finally (cui/end! ui)))
              (column! "widgets-column" widgets-width)
              (try (note-section! ui) (timers-section! ui) (keys-section! ui) (finally (cui/end! ui))))
          (do (column! "preview-column" (max 1 (min widgets-width (- (cui/width ui) 16))))
              (try (launcher-section! ui buffer)
                   (note-section! ui)
                   (timers-section! ui)
                   (keys-section! ui)
                   (finally (cui/end! ui)))))
        (spacer! ui "right-gutter")
        (finally (cui/end! ui)))
      (finally (cui/end-scroll! ui))))
  (screenshot-frame! ui))

(defn render!
  "Render the development host and the shared launcher panel inside it."
  [ui buffer]
  (binding [menu/*configuration* @configuration]
    (render-preview! ui buffer)))

(defn- running-port
  "The port of a Cloncher Dev nREPL that accepts connections, or nil."
  []
  (when-let [port (when (fs/exists? ".nrepl-port") (some-> (slurp ".nrepl-port") str/trim parse-long))]
    (try
      (with-open [socket (java.net.Socket.)]
        (.connect socket (java.net.InetSocketAddress. "127.0.0.1" (int port)) 500)
        port)
      (catch Exception _ nil))))

(defn -main [& args]
  (when-let [port (running-port)]
    (binding [*out* *err*]
      (println (str "Cloncher Dev is already running (nREPL on 127.0.0.1:" port ")."))
      (println "Reload it with: jolt eval desktop --ns user -e '(cloncher.dev/reload!)'"))
    (System/exit 1))
  (let [request (if (= ["off"] (vec args)) {:kind :off :mode :launcher} (app/parse-args args))]
    (when-not (contains? #{:launcher :dmenu :secret} (:mode request))
      (throw (ex-info "jolt dev accepts off or a launcher view such as apps or menu connect" {})))
    (app/load-cui!)
    (reset! configuration (menu/load-configuration!))
    (binding [menu/*configuration* @configuration]
      (app/show! session (cond-> request
                           (contains? #{:dmenu :secret} (:mode request))
                           (assoc :kind (:mode request)))))
    (let [stop (nrepl/start 0 [] {:port-file ".nrepl-port"})]
      (println (str "Cloncher Dev nREPL on 127.0.0.1:" (:port (meta stop))))
      (flush)
      (try
        (cui/window! {:width 760 :height 610 :title "Cloncher Dev" :redraw redraw}
                     (fn [ui buffer]
                       (locking reload-lock
                         (try (render! ui buffer)
                              (catch Exception error
                                (binding [*out* *err*] (println "Cloncher Dev:" (ex-message error)))
                                (app/close-session! session)
                                (cui/caption! ui (str "Preview error · " (ex-message error)))
                                (cui/wake-in! ui 0.1))))))
        (finally
          (app/close-session! session)
          (when-let [capture (:capture @control)]
            (finish-screenshot! capture (ex-info "The dev window closed during capture" {})))
          (stop)))
      (System/exit 0))))
