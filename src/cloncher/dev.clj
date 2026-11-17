(ns cloncher.dev
  "Persistent development window for experimenting with launcher sessions."
  (:require [babashka.fs :as fs]
            [cloncher.cui :as cui]
            [cloncher.main :as app]
            [cloncher.palette :as palette]
            [cloncher.view :as view]
            [jolt.nrepl :as nrepl]))

(defonce ^:private control (atom {}))
(defonce ^:private redraw (atom nil))
(defonce ^:private session (app/session))
(defonce ^:private reload-lock (Object.))

(defn state [] (app/state session))

(defn redraw! []
  (when-let [wake @redraw] (wake)))

(defn reload!
  "Reload presentation and session code in the running window."
  []
  (locking reload-lock
    (doseq [namespace '[cloncher.json cloncher.process cloncher.persistence cloncher.socket cloncher.daemon
                        cloncher.cui cloncher.panel cloncher.palette cloncher.search cloncher.icons
                        cloncher.desktop cloncher.menu cloncher.sources cloncher.commands
                        cloncher.files cloncher.preview cloncher.edit cloncher.lisp
                        cloncher.machine cloncher.usage cloncher.view-cache cloncher.view
                        cloncher.main cloncher.dev]]
      (require namespace :reload)))
  (redraw!))

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
  (cui/column! ui "launcher-off" {:height 468 :surface? true})
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

(defn render!
  "Render the development host and the shared launcher panel inside it."
  [ui buffer]
  (let [{:keys [open]} (first (swap-vals! control dissoc :open))
        _ (when open (open-preview! ui buffer open))
        off? (= :off (app/status session))
        width (max 1 (min (+ view/preview-width 48) (- (cui/width ui) 16)))]
    (cui/bar! ui "development-window")
    (try
      (spacer! ui "left-gutter")
      (cui/column! ui "preview" {:width width :height 0 :pad 24 :gap 12})
      (try
        (cui/heading! ui "Launcher preview" 3)
        (when (cui/button! ui (if off? "Open launcher" "Turn off") off?)
          (open-preview! ui buffer (if off? {:kind :root} {:kind :off})))
        (if (= :on (app/status session))
          (do
            (cui/apply-colors! ui (palette/load-colors))
            (app/draw-frame! session ui buffer {:preview? true})
            (when (= :complete (app/status session))
              (open-preview! ui buffer {:kind :off})))
          (off-panel! ui))
        (finally (cui/end! ui)))
      (spacer! ui "right-gutter")
      (finally (cui/end! ui))))
  (screenshot-frame! ui))

(defn -main [& args]
  (let [request (if (= ["off"] (vec args)) {:kind :off :mode :launcher} (app/parse-args args))]
    (when-not (contains? #{:launcher :dmenu :secret} (:mode request))
      (throw (ex-info "jolt dev accepts off or a launcher view such as apps or menu connect" {})))
    (app/load-cui!)
    (app/show! session (cond-> request
                         (contains? #{:dmenu :secret} (:mode request))
                         (assoc :kind (:mode request))))
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
