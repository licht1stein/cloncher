(ns cloncher.key-overlay
  "Key display for screen recordings: the keys being pressed, in a click-through card
  like the timer's, mirroring it in the bottom right corner.

  Wayland gives no client other windows' keys, so the worker reads the keyboards' evdev
  devices directly. That needs read access to /dev/input/event*, normally membership in
  the `input` group. Names follow the US layout of the physical keys."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.cui :as cui]
            [cloncher.draw :as draw :refer [color in-box text!]]
            [cloncher.palette :as palette]
            [cloncher.process :as p]
            [cloncher.socket :as socket]
            [jolt.ffi :as ffi]))

;; ── keys ────────────────────────────────────────────────────────────────────

(def ^:private modifiers
  {29 :ctrl 97 :ctrl 42 :shift 54 :shift 56 :alt 100 :alt 125 :super 126 :super})

(def ^:private printable
  "Linux key codes of the US layout's character keys: [unshifted shifted]."
  (merge (zipmap (range 2 12) (map vector "1234567890" "!@#$%^&*()"))
         (zipmap (range 16 26) (map vector "qwertyuiop" "QWERTYUIOP"))
         (zipmap (range 30 39) (map vector "asdfghjkl" "ASDFGHJKL"))
         (zipmap (range 44 51) (map vector "zxcvbnm" "ZXCVBNM"))
         {12 [\- \_] 13 [\= \+] 26 [\[ \{] 27 [\] \}] 39 [\; \:] 40 [\' \"] 41 [\` \~]
          43 [\\ \|] 51 [\, \<] 52 [\. \>] 53 [\/ \?] 57 [\space \space]}))

(def ^:private named
  (merge {1 "Esc" 14 "⌫" 15 "Tab" 28 "⏎" 96 "⏎" 57 "Space" 58 "Caps Lock"
          102 "Home" 103 "↑" 104 "Page Up" 105 "←" 106 "→" 107 "End" 108 "↓" 109 "Page Down"
          110 "Insert" 111 "Del" 99 "Print" 119 "Pause" 113 "Mute" 114 "Volume −" 115 "Volume +"
          163 "Next" 164 "Play/Pause" 165 "Previous" 224 "Brightness −" 225 "Brightness +"
          127 "Menu"}
         (zipmap (concat (range 59 69) [87 88]) (map #(str "F" %) (range 1 13)))))

(def ^:private emacs-named
  "Emacs's names for the keys it does not spell as a character."
  (merge {1 "ESC" 14 "DEL" 15 "TAB" 28 "RET" 96 "RET" 57 "SPC" 58 "<capslock>"
          102 "<home>" 103 "<up>" 104 "<prior>" 105 "<left>" 106 "<right>" 107 "<end>" 108 "<down>"
          109 "<next>" 110 "<insert>" 111 "<delete>" 99 "<print>" 119 "<pause>" 127 "<menu>"
          113 "<XF86AudioMute>" 114 "<XF86AudioLowerVolume>" 115 "<XF86AudioRaiseVolume>"
          163 "<XF86AudioNext>" 164 "<XF86AudioPlay>" 165 "<XF86AudioPrev>"
          224 "<XF86MonBrightnessDown>" 225 "<XF86MonBrightnessUp>"}
         (zipmap (concat (range 59 69) [87 88]) (map #(str "<f" % ">") (range 1 13)))))

(def ^:private hyper #{:ctrl :alt :shift :super})
(def ^:private desktop-modifiers [[:ctrl "Ctrl"] [:alt "Alt"] [:shift "Shift"] [:super "Super"]])
(def ^:private emacs-modifiers [[:ctrl "C-"] [:alt "M-"] [:shift "S-"] [:super "s-"]])

(defn- chord-label
  "A chord of `mods` and key `code` (nil for modifiers alone) as the desktop writes it
  (`Ctrl + Shift + V`, `Hyper + Space`) or as Emacs does (`C-S-v`, `H-SPC`).
  Ctrl, Alt, Shift and Super together are Hyper."
  [mods code emacs?]
  (let [[plain shifted] (printable code)
        hyper? (= hyper mods)]
    (if emacs?
      (let [letter? (and plain (Character/isLetter ^Character plain))
            ;; Shift turns punctuation into its other character, as Emacs receives it.
            shifted-character? (and plain (not letter?) (not= \space plain) (not hyper?) (mods :shift))
            mods (if shifted-character? (disj mods :shift) mods)
            key (cond (nil? code) ""
                      (emacs-named code) (emacs-named code)
                      shifted-character? (str shifted)
                      plain (str plain)
                      :else (str "<key-" code ">"))]
        (str (if hyper? "H-" (apply str (keep (fn [[m prefix]] (when (mods m) prefix)) emacs-modifiers)))
             key))
      (str/join " + " (concat (if hyper? ["Hyper"] (keep (fn [[m name]] (when (mods m) name)) desktop-modifiers))
                              (when code [(or (named code) (some-> plain str str/upper-case) (str "Key " code))]))))))

(def ^:private history 4)
(def ^:private run-gap-ms 1500)

(def ^:private combo-gap-ms 1000)

(defn- joins-combo?
  "A guess at Emacs key sequences, without its keymaps: a chord within a second of a chord
  continues the sequence (`C-x C-f`), and so does a plain key after a Ctrl chord (`C-c c`).
  A plain key after Meta starts afresh, since `M-x` is followed by typing."
  [{:keys [emacs? entries at]} mods now]
  (let [{:keys [sequence] :as last-entry} (peek entries)
        [previous-mods previous-code] (peek sequence)]
    (boolean (and emacs? (:emacs? last-entry) previous-code (seq previous-mods)
                  (< (- now (or at 0)) combo-gap-ms)
                  (or (seq (disj mods :shift)) (previous-mods :ctrl))))))

(defn- push [state entry now]
  (-> state
      (update :entries #(vec (take-last history (conj % (assoc entry :emacs? (boolean (:emacs? state)))))))
      (assoc :at now)))

(defn press
  "The display after a key event: `value` 1 press, 2 repeat, 0 release.
  Characters typed with at most Shift join a run of text; other keys show as chords,
  a repeated chord counts up, and a modifier pressed and released alone shows itself.
  While `:emacs?` is set (Emacs has the focus), entries take Emacs notation and keys that
  look like one sequence join it: `C-c c`, `C-x C-f`."
  [{:keys [mods solo entries at] :as state} code value now]
  (let [modifier (modifiers code)
        mods (or mods #{})
        last-entry (peek entries)
        recent? (< (- now (or at 0)) run-gap-ms)]
    (cond
      ;; `:solo` holds the modifiers pressed since none were down, until another key joins them.
      (and modifier (= 1 value))
      (let [fresh? (empty? mods)]
        (-> state
            (assoc :mods (conj mods modifier))
            (assoc :solo (when (or fresh? solo) (conj (if fresh? #{} solo) modifier)))))

      (and modifier (zero? value))
      (let [state (update state :mods disj modifier)]
        (if (empty? (:mods state))
          (cond-> (dissoc state :solo) solo (push {:sequence [[solo nil]] :count 1} now))
          state))

      (or modifier (zero? value)) state

      :else
      (let [state (dissoc state :solo)
            [plain shifted] (printable code)
            text? (and plain (contains? #{#{} #{:shift}} mods) (not= 57 code))
            space? (and (= 57 code) (empty? mods) (:text last-entry) recent?)
            last-index (dec (count entries))]
        (cond
          (and (= 1 value) (joins-combo? state mods now))
          (-> state (update-in [:entries last-index :sequence] conj [mods code]) (assoc :at now))

          (or text? space?)
          (let [character (if (mods :shift) shifted plain)]
            (if (and (:text last-entry) recent?)
              (-> state (update-in [:entries last-index :text] str character) (assoc :at now))
              (push state {:text (str character)} now)))

          (and (= [[mods code]] (:sequence last-entry)) recent?)
          (-> state (update-in [:entries last-index :count] inc) (assoc :at now))

          :else (push state {:sequence [[mods code]] :count 1} now))))))

(defn- label [{:keys [text sequence count emacs?]}]
  (if text
    text
    (str (str/join (if emacs? " " "  ") (map (fn [[mods code]] (chord-label mods code emacs?)) sequence))
         (when (> count 1) (str "  ×" count)))))

;; ── card ────────────────────────────────────────────────────────────────────

(def width 300)
(def height 108)
(def ^:private visible-ms 2500)

(defn render!
  "Draw the card for `state` at time `now`, filling the parent; nothing once the keys
  have been idle for a moment. Return seconds until the card must redraw, or nil."
  [ui {:keys [entries at]} now]
  (draw/font-role! ui 0 3)
  (when (and (seq entries) (< (- now at) visible-ms))
    (let [latest (peek entries)
          earlier (pop entries)
          text (label latest)
          ;; A run of typing shows its end.
          text (if (> (count text) 18) (str "…" (subs text (- (count text) 17))) text)]
      (in-box ui {:width [:fill 1] :height [:fill 1] :radius 16 :clip true :pad-x 18 :pad-y 14 :gap 6
                  :justify :center
                  :bg (color ui :surface 153) :border 1 :border-color (color ui :border-subtle 255)}
        (text! ui (if (seq earlier) (str/join "   " (map label earlier)) "Keys")
               {:size 12 :color (color ui :muted 255) :nowrap? true})
        (text! ui text {:font :mono :size (if (> (count text) 12) 22 30)
                        :color (color ui :foreground 255) :nowrap? true}))
      (/ (- visible-ms (- now at)) 1000.0))))

;; ── evdev ───────────────────────────────────────────────────────────────────

(ffi/defcfn ^:private c-open "open" [:string :int] :int)
(ffi/defcfn ^:private c-read "read" [:int :pointer :size_t] :ssize_t)
(ffi/defcfn ^:private c-close "close" [:int] :int)
(ffi/defcfn ^:private c-poll "poll" [:pointer :size_t :int] :int :blocking)

(def ^:private event-size 24)   ; struct input_event on 64-bit: timeval, type, code, value
(def ^:private batch 64)

(defn- keyboards
  "Event devices that have the A key, which is what a keyboard has: /dev/input/eventN paths.
  The KEY bitmap lists 64-bit words, most significant first, so A (30) is in the low 32
  bits of the last."
  []
  (->> (str/split (slurp "/proc/bus/input/devices") #"\n\n")
       (keep (fn [block]
               (let [handlers (second (re-find #"(?m)^H: Handlers=(.*)$" block))
                     low-keys (some-> (re-find #"(?m)^B: KEY=(.*)$" block) second (str/split #" ") last
                                      (as-> word (subs word (max 0 (- (count word) 8))))
                                      (Long/parseLong 16))]
                 (when (and handlers (re-find #"\bkbd\b" handlers) low-keys (bit-test low-keys 30))
                   (some->> (re-find #"\bevent\d+\b" handlers) (str "/dev/input/"))))))
       vec))

(defn- read-keys!
  "Read every keyboard until `stop?`, calling `on-key` with code and value for each key event.
  Keyboards plugged in later are picked up within two seconds."
  [stop? on-key]
  (let [open (volatile! {})
        polls (ffi/alloc (* 8 64))
        buffer (ffi/alloc (* event-size batch))]
    (try
      (loop [rescan-at 0]
        (when-not @stop?
          (let [now (System/currentTimeMillis)
                rescan-at (if (< now rescan-at)
                            rescan-at
                            (do (doseq [path (keyboards) :when (not (contains? @open path))]
                                  (let [fd (c-open path (bit-or 0x800 0x80000))]   ; O_NONBLOCK | O_CLOEXEC
                                    (when-not (neg? fd) (vswap! open assoc path fd))))
                                (+ now 2000)))
                fds (vec (take 64 @open))]
            (doseq [[i [_ fd]] (map-indexed vector fds)]
              (ffi/write polls :int fd (* 8 i))
              (ffi/write polls :int16 1 (+ 4 (* 8 i)))      ; POLLIN
              (ffi/write polls :int16 0 (+ 6 (* 8 i))))
            (when (pos? (c-poll polls (count fds) 250))
              (doseq [[i [path fd]] (map-indexed vector fds)
                      :let [revents (ffi/read polls :int16 (+ 6 (* 8 i)))]
                      :when (pos? revents)]
                (let [n (c-read fd buffer (* event-size batch))]
                  (if (pos? n)
                    (dotimes [e (quot n event-size)]
                      (let [offset (* e event-size)]
                        (when (= 1 (ffi/read buffer :uint16 (+ offset 16)))   ; EV_KEY
                          (on-key (ffi/read buffer :uint16 (+ offset 18)) (ffi/read buffer :int (+ offset 20))))))
                    ;; POLLERR or POLLHUP: unplugged. Drop it until a rescan finds it again.
                    (when (pos? (bit-and revents 0x18))
                      (c-close fd)
                      (vswap! open dissoc path))))))
            (recur rescan-at))))
      (finally
        (run! c-close (vals @open))
        (ffi/free polls)
        (ffi/free buffer)))))

;; ── overlay process ─────────────────────────────────────────────────────────

(def ^:private unavailable
  "The key display needs a Wayland compositor with layer-shell support and read access to /dev/input (the input group).")

(defn- endpoint [] (str (fs/path (fs/parent (socket/path)) "keys.sock")))

(defn running?
  "Whether the key display is on."
  []
  (if-let [fd (socket/connect (endpoint))]
    (try (socket/write! fd {:op "status"}) true (finally (socket/close! fd)))
    false))

(defn toggle!
  "Turn the key display off when it is on, else start it in its own process."
  []
  (if-let [fd (socket/connect (endpoint))]
    (try (socket/write! fd {:op "stop"}) (finally (socket/close! fd)))
    (when-not (contains? #{"ready" "busy"} (p/start-helper! ["--key-overlay"] 6000))
      (throw (ex-info unavailable {})))))

(defn worker!
  "Show the key display until asked to stop. The socket lock keeps one running."
  [load-cui!]
  (if-let [listener (socket/listen! (endpoint))]
    (try
      (when (empty? (keyboards))
        (throw (ex-info "No keyboard found in /proc/bus/input/devices" {})))
      (load-cui!)
      (let [state (atom {:entries [] :at 0})
            stop? (atom false)
            redraw (atom nil)
            reader (future (read-keys! stop? (fn [code value]
                                                (swap! state press code value (System/currentTimeMillis))
                                                (when-let [wake @redraw] (wake)))))
            ;; Emacs notation while an Emacs frame has the focus, checked a few times a second.
            focus (future (while (not @stop?)
                            (let [{:keys [exit out]} (p/sh {:continue true} "niri" "msg" "--json" "focused-window")
                                  app-id (when (zero? exit) (second (re-find #"\"app_id\":\"([^\"]*)\"" out)))]
                              (swap! state assoc :emacs? (= "emacs" (some-> app-id str/lower-case))))
                            (Thread/sleep 300)))
            ;; `running?` asks for the status; `toggle!` asks it to stop.
            control (future (loop []
                              (when-let [fd (socket/accept! (:fd listener))]
                                (let [stop (try (= "stop" (:op (socket/read! fd 1000)))
                                                (catch Exception _ false)
                                                (finally (socket/close! fd)))]
                                  (if stop
                                    (do (reset! stop? true)
                                        (when-let [wake @redraw] (wake)))
                                    (recur))))))
            ready? (volatile! false)
            colors (volatile! {})
            reload-at (volatile! 0)]
        (try
          ;; Its own app ID gives the timer's click-through card, in the bottom right corner.
          (cui/window! {:app-id "cloncher.keys" :title "Cloncher Keys"
                        :width width :height height :redraw redraw}
            (fn [ui _]
              (if @stop?
                (cui/quit!)
                (let [now (System/currentTimeMillis)]
                  (when (>= now @reload-at)
                    (vreset! colors (palette/load-colors))
                    (vreset! reload-at (+ now 1000)))
                  (cui/apply-colors! ui @colors)
                  (when-let [delay (render! ui @state now)]
                    (cui/wake-in! ui (max 0.05 delay)))
                  (when-not @ready?
                    (vreset! ready? true)
                    (println "ready")
                    (flush))))))
          (finally
            (reset! stop? true)
            (future-cancel control)
            @reader
            @focus)))
      (finally (socket/stop! listener)))
    (do (println "busy") (flush))))
