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

(defn- draw-card!
  "Draw a timer reading as the 300×108 card, with its top left at the drawing origin."
  [ui {:keys [phase digits label progress latest-lap]}]
  (let [lap (when latest-lap (str "Lap " (:number latest-lap) " · " (:split latest-lap)))]
    (case phase
      "paused" (card! ui digits label (if lap (str "Paused · " lap) "Paused") progress 1)
      "finished" (card! ui digits label "Time’s up" progress 2)
      (card! ui digits label (or lap "") progress 0))))

;; ── development preview ─────────────────────────────────────────────────────
;; The card draws at fixed coordinates, so each sits in a CUI canvas: its paint
;; callback runs after layout and moves the drawing origin to the canvas.

(ffi/defcfn ^:private canvas* "cui_canvas"
  [:pointer :string [:by-value [:struct [[:kind :int] [:value :float]]]]
   [:by-value [:struct [[:kind :int] [:value :float]]]] :pointer :pointer :size_t]
  [:by-value [:struct [[:x :float] [:y :float] [:w :float] [:h :float]
                       [:hovered :bool] [:pressed :bool] [:clicked :bool] [:mx :float] [:my :float]]]])
(ffi/defcfn ^:private push-origin! "cui__push_xform" [:pointer :float :float :float :float :float :float] :void)
(ffi/defcfn ^:private pop-origin! "cui__pop_xform" [:pointer] :void)

(def ^:private canvas-result
  (ffi/layout [:struct [[:x :float] [:y :float] [:w :float] [:h :float]
                        [:hovered :bool] [:pressed :bool] [:clicked :bool] [:mx :float] [:my :float]]]))

(defonce ^:private preview-cards (atom {}))
(defonce ^:private preview-started (System/nanoTime))

;; One painter for the process: CUI calls it after the frame function returns.
(defonce ^:private painter
  (delay (ffi/foreign-callable
          (fn [ui user]
            (let [index (ffi/read user :int)
                  {:keys [origin reading]} (get @preview-cards index)
                  [x y] origin]
              (when reading
                (push-origin! ui 1.0 0.0 0.0 1.0 (double x) (double y))
                (try (draw-card! ui reading)
                     (finally (pop-origin! ui))))))
          ;; cui_paint_fn(ui, cui_rect rect, user): the rect travels in SSE registers,
          ;; leaving ui and user as the two integer arguments.
          [:pointer :pointer] :void :collect-safe)))

(defn- sample-timers
  "A running countdown, a stopwatch with laps, and a finished countdown, all live."
  [now]
  (let [elapsed (- now preview-started)
        minute (* 60 second-ns)
        started (- now elapsed (* 2 minute))   ; the stopwatch has run two minutes longer than the preview
        stopwatch {:id "stopwatch" :kind "stopwatch" :label "Stopwatch" :phase "running" :start started :elapsed 0}]
    [{:id "countdown" :kind "countdown" :label "Tea" :total (* 5 minute) :phase "running"
      :start (- now (mod elapsed (* 5 minute))) :elapsed 0}
     (-> stopwatch
         (record-lap (+ started (* 41 second-ns)))
         (record-lap (+ started (* 90 second-ns))))
     {:id "finished" :kind "countdown" :label "Countdown" :total minute :phase "finished"
      :start nil :elapsed minute :finished-at (- now (mod elapsed (* 3 second-ns)))}]))

(defn preview!
  "Development: the running countdown and stopwatch in one row, the finished countdown
  below, ticking and ringing as the overlay does."
  [ui]
  (let [now (System/nanoTime)
        readings (mapv #(reading % now) (sample-timers now))
        size-layout (ffi/layout [:struct [[:kind :int] [:value :float]]])
        width (doto (ffi/alloc (ffi/sizeof size-layout)) (ffi/write size-layout {:kind 2 :value 300.0}))
        height (doto (ffi/alloc (ffi/sizeof size-layout)) (ffi/write size-layout {:kind 2 :value 108.0}))
        result (ffi/alloc (ffi/sizeof canvas-result))
        user (ffi/alloc 4)]
    (try
      (doseq [row [[0 1] [2]]]
        (cui/bar! ui (str "timer-preview-row-" (first row)))
        (try
          (cui/spacer! ui)
          (doseq [index row]
            (ffi/write user :int index)
            #_{:clj-kondo/ignore [:invalid-arity]}
            (canvas* result ui (str "timer-preview-" index) width height @painter user 4)
            ;; The canvas reports last frame's rectangle; layout is stable between frames.
            (let [{:keys [x y]} (ffi/read result canvas-result)]
              (swap! preview-cards assoc index {:origin [x y] :reading (readings index)})
              (when (zero? (+ x y)) (cui/redraw! ui))))
          (cui/spacer! ui)
          (finally (cui/end! ui))))
      (cui/wake-in! ui (reduce min 1.0 (map :delay readings)))
      (finally (ffi/free width) (ffi/free height) (ffi/free result) (ffi/free user)))))

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
                  (let [{:keys [delay] :as reading} (update-timer! state nil)
                        now (System/currentTimeMillis)]
                    (when (>= now @reload-at)
                      (vreset! colors (palette/load-colors))
                      (vreset! reload-at (+ now 1000)))
                    (cui/apply-colors! ui @colors)
                    (draw-card! ui reading)
                    (when-not @ready?
                      (vreset! controls (future (serve-controls! listener state stop? redraw)))
                      (vreset! ready? true)
                      (println "ready")
                      (flush))
                    (cui/wake-in! ui (max 0.001 delay)))))))
          (finally (when-let [task @controls] (future-cancel task)))))
      (finally (socket/stop! listener)))
    (do (println "busy") (flush))))
