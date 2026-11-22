(ns cloncher.timer-overlay
  "One click-through timer per launcher session, owned by a separate CUI process."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.cui :as cui]
            [cloncher.json :as json]
            [cloncher.palette :as palette]
            [cloncher.process :as p]
            [cloncher.socket :as socket]
            [jolt.ffi :as ffi]))

(ffi/defcfn ^:private card! "cloncher_timer_card"
  [:pointer :string :string :string :double :int] :void)

(def ^:private second-ns 1000000000)
(def ^:private alarm-cycle-ns (* 3 second-ns))
(def ^:private alarm-pulse-ns (* 2 second-ns))
(def ^:private unavailable "The timer needs a Wayland compositor with layer-shell support and a current Cloncher CUI host.")

(defn- endpoint [] (str (fs/path (fs/parent (socket/path)) "timer.sock")))

(defn- request! [message timeout]
  (when-let [fd (socket/connect (endpoint))]
    (try
      (when (socket/write! fd message) (socket/read! fd timeout))
      (catch Exception _ nil)
      (finally (socket/close! fd)))))

(defn status [] (request! {:op "status"} 250))

(defn control! [id op]
  (request! {:id id :op op} 1000))

(defn start! [kind seconds label]
  (let [message {:op "start" :kind kind :seconds seconds :label label}]
    (when-not (:id (request! message 1000))
      (let [ready (p/start-helper! ["--timer-overlay" (endpoint) (json/generate-string message)] 6000)]
        (when-not (or (= "ready" ready)
                      (and (= "busy" ready) (:id (request! message 1000))))
          (throw (ex-info unavailable {})))))))

(defn- new-timer [{:keys [kind seconds label]} now]
  (when (or (= kind "stopwatch")
            (and (= kind "countdown") (integer? seconds) (<= 1 seconds 31536000)))
    {:id (str (java.util.UUID/randomUUID)) :kind kind :label (or (not-empty label) (if (= kind "countdown") "Countdown" "Stopwatch"))
     :total (when (= kind "countdown") (* seconds second-ns))
     :phase "running" :start now :elapsed 0}))

(defn- elapsed [{:keys [start elapsed]} now]
  (if start (+ elapsed (- now start)) elapsed))

(defn- digits [seconds]
  (let [h (quot seconds 3600), m (mod (quot seconds 60) 60), s (mod seconds 60)]
    (if (pos? h) (format "%d:%02d:%02d" h m s) (format "%02d:%02d" m s))))

(defn- lap-digits [elapsed]
  (format "%s.%02d" (digits (quot elapsed second-ns)) (mod (quot elapsed 10000000) 100)))

(defn- record-lap [{:keys [laps] :as timer} now]
  (let [at (elapsed timer now)
        split (- at (or (:at (peek laps)) 0))]
    (update timer :laps (fnil conj [])
            {:at at :number (inc (count laps)) :split (lap-digits split) :total (lap-digits at)})))

(defn- reading [{:keys [total phase finished-at laps] :as timer} now]
  (let [spent (elapsed timer now)
        remaining (when total (max 0 (- total spent)))
        alarm-offset (when finished-at (mod (- now finished-at) alarm-cycle-ns))
        seconds (if total (quot (+ remaining (dec second-ns)) second-ns) (quot spent second-ns))]
    (assoc (select-keys timer [:id :kind :label :phase])
           :digits (digits seconds)
           :latest-lap (peek laps)
           :progress (cond alarm-offset (/ (double alarm-offset) alarm-pulse-ns)
                           total (/ (double remaining) total)
                           :else -1.0)
           :delay (cond
                    alarm-offset (if (< alarm-offset alarm-pulse-ns)
                                   (/ 1.0 24)
                                   (/ (double (- alarm-cycle-ns alarm-offset)) second-ns))
                    (= "running" phase)
                    (let [part (mod (or remaining spent) second-ns)]
                      (/ (double (if total (if (zero? part) second-ns part) (- second-ns part))) second-ns))
                    :else 1.0))))

(defn- update-timer! [state {:keys [op id] :as message}]
  (locking state
    (let [now (System/nanoTime)
          {:keys [total phase] :as previous} @state
          finished? (and total (= "running" phase) (>= (elapsed previous now) total))
          timer (if finished? (assoc previous :phase "finished" :elapsed total :start nil :finished-at now) previous)
          active? (= id (:id timer))
          next (cond
                 (= op "start") (or (new-timer message now) timer)
                 (not active?) timer
                 (and (= op "pause") (= "running" (:phase timer)))
                 (assoc timer :phase "paused" :elapsed (elapsed timer now) :start nil)
                 (and (= op "resume") (= "paused" (:phase timer)))
                 (assoc timer :phase "running" :start now)
                 (and (= op "lap") (= "stopwatch" (:kind timer)) (= "running" (:phase timer)))
                 (record-lap timer now)
                 (= op "restart") (assoc (dissoc timer :finished-at :laps) :phase "running" :elapsed 0 :start now)
                 :else timer)]
      (when-not (identical? previous next) (vreset! state next))
      (cond-> (reading next now)
        (and active? (= op "laps")) (assoc :laps (:laps next))
        (and active? (= op "close")) (assoc :close true)))))

(defn- serve-controls! [listener state stop? redraw]
  (loop []
    (when-let [fd (socket/accept! (:fd listener))]
      (let [stop (try
                   (when-let [message (socket/read! fd 1000)]
                     (let [{:keys [close] :as reply} (update-timer! state message)]
                       (socket/write! fd (select-keys reply [:id :kind :label :digits :phase :latest-lap :laps]))
                       (when close (reset! stop? true))
                       (when-not (contains? #{"status" "laps"} (:op message))
                         (when-let [wake @redraw] (wake)))
                       close))
                   (catch Exception _ nil)
                   (finally (socket/close! fd)))]
        (when-not stop (recur))))))

(defn worker! [path initial load-cui!]
  (if-let [listener (socket/listen! path)]
    (try
      (let [initial-timer (new-timer (json/parse-string initial true) 0)
            state (volatile! initial-timer)
            redraw (atom nil)
            stop? (atom false)
            controls (volatile! nil)
            ready? (volatile! false)
            colors (volatile! {})
            reload-at (volatile! 0)]
        (when-not initial-timer (throw (ex-info "Invalid timer" {})))
        (load-cui!)
        (try
          (cui/window! {:app-id "cloncher.timer-overlay" :title "Cloncher Timer"
                        :width 300 :height 108 :redraw redraw}
            (fn [ui _]
              (if @stop?
                (cui/quit!)
                (do
                  (when-not @ready?
                    (vreset! state (assoc initial-timer :start (System/nanoTime))))
                  (let [{:keys [phase digits label progress delay latest-lap]} (update-timer! state nil)
                        lap (when latest-lap (str "Lap " (:number latest-lap) " · " (:split latest-lap)))
                        now (System/currentTimeMillis)]
                    (when (>= now @reload-at)
                      (vreset! colors (palette/load-colors))
                      (vreset! reload-at (+ now 1000)))
                    (cui/apply-colors! ui @colors)
                    (case phase
                      "paused" (card! ui digits label (if lap (str "Paused · " lap) "Paused") progress 1)
                      "finished" (card! ui digits label "Time’s up" progress 2)
                      (card! ui digits label (or lap "") progress 0))
                    (when-not @ready?
                      (vreset! controls (future (serve-controls! listener state stop? redraw)))
                      (vreset! ready? true)
                      (println "ready")
                      (flush))
                    (cui/wake-in! ui (max 0.001 delay)))))))
          (finally (when-let [task @controls] (future-cancel task)))))
      (finally (socket/stop! listener)))
    (do (println "busy") (flush))))
