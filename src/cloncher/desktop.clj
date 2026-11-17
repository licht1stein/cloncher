(ns cloncher.desktop
  (:require [cloncher.process :as p]
            [cloncher.json :as json]
            [clojure.string :as str]))

(defn list-windows
  "Niri windows, most recently focused first, with the current one last."
  []
  (let [{:keys [exit out err]} (p/sh {:continue true} "timeout" "2" "niri" "msg" "--json" "windows")]
    (when-not (zero? exit)
      (throw (ex-info (str "Could not list windows: " (str/trim (or err ""))) {})))
    (->> (json/parse-string out true)
         (remove #(contains? #{"cloncher" "cloncher.launcher" "cloncher.launcher.tiled"} (:app_id %)))
         (map #(assoc % :type :window :app-id (:app_id %) :workspace-id (:workspace_id %)))
         (sort-by (fn [w] [(boolean (:is_focused w))
                          (- (get-in w [:focus_timestamp :secs] 0))
                          (- (get-in w [:focus_timestamp :nanos] 0))
                          (:id w)]))
         vec)))

(defn focus
  "Describe a focus action. Only activating its result performs the action."
  [window]
  (when window
    (when-not (and (= :window (:type window)) (pos-int? (:id window)))
      (throw (ex-info "focus expects a window from (list-windows)" {})))
    {:type :focus :window window}))

(defn focus-window! [{:keys [id]}]
  ;; argv, not shell text: window titles and app ids are never executable.
  (when-not (pos-int? id) (throw (ex-info "Invalid window id" {})))
  (let [{:keys [exit err]} (p/sh {:continue true} "timeout" "2" "niri" "msg" "action" "focus-window" "--id" (str id))]
    (when-not (zero? exit)
      (p/sh {:continue true} "notify-send" "Window focus failed" (str/trim (or err "Window may have closed")))))
  nil)

(defn window-item [window]
  {:icon "󰖯" :label (or (not-empty (:title window)) (:app-id window) "Untitled window")
   :subtitle (str (:app-id window) " · workspace " (:workspace-id window)
                  (when (:is_focused window) " · current"))
   :row-id (str "window:" (:id window)) :hint "focus" :volatile true :window true
   :action #(focus-window! window)})

(defn window-items []
  (try
    (let [windows (list-windows)]
      (if (seq windows) (mapv window-item windows)
          [{:label "No open windows" :volatile true}]))
    (catch Exception e [{:label "Could not list windows" :subtitle (ex-message e)
                         :warn true :volatile true}])))

(defn place-launcher!
  "Place the window on Niri when its rules did not already open it floating."
  [pid width]
  (when (and (p/getenv "NIRI_SOCKET") (p/getenv "WAYLAND_DISPLAY")
             (not= "x11" (p/getenv "CUI_BACKEND"))
             (not= "1" (p/getenv "CLONCHER_TILING")))
    (try
      (let [window (some (fn [_]
                           (let [{:keys [out exit]} (p/sh {:continue true}
                                                         "timeout" "1" "niri" "msg" "--json" "windows")]
                             (or (when (zero? exit)
                                   (some #(when (and (= pid (:pid %))
                                                     (= "cloncher.launcher" (:app_id %))) %)
                                         (json/parse-string out true)))
                                 (do (Thread/sleep 25) nil))))
                         (range 6))]
        (when (and window (not (:is_floating window)))
          (let [id (str (:id window))]
            (doseq [args [["move-window-to-floating" "--id" id]
                          ["set-window-width" (str width) "--id" id]
                          ["center-window" "--id" id]
                          ["move-floating-window" "--id" id "--y" "140"]]]
              (apply p/sh {:continue true} "timeout" "1" "niri" "msg" "action" args)))))
      (catch Exception _ nil))))
