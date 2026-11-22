(ns cloncher.menu
  "Desktop menu providers and actions ported from the dotfiles launcher."
  (:require [babashka.fs :as fs]
            [cloncher.process :as p]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [cloncher.clipboard :as clipboard]
            [cloncher.config :as config]
            [cloncher.json :as json]
            [cloncher.search :as search]
            [cloncher.note :as note]
            [cloncher.sources :as sources]
            [cloncher.timer-overlay :as overlay]
            [cloncher.desktop :as desktop-repl]))

(defn kitty
  "Open a compositor-owned kitty window.

  Spawn through niri so the window is not a child of the launcher (which would
  keep the keyboard grab) and override `shell` so kitty.conf's tmux
  wrapper cannot swallow sudo's TTY."
  [cmd & {:keys [wait? class title]}]
  (let [script (cond-> cmd
                 wait? (str "; read -p \"Press enter to close...\""))
        opts (->> [(when class (str "--class " class))
                   (when title (str "--title " title))
                   "-o shell=zsh"]
                  (remove nil?)
                  (str/join " "))]
    (format "niri msg action spawn -- kitty %s sh -c '%s'" opts script)))

;; Debug mode: set to true to see picker output in terminal
(def debug? (= (System/getenv "DEBUG") "1"))

(def ^:dynamic *pick!*
  "Open a CUI picker after the calling window has closed."
  nil)

(defn show-menu [prompt items]
  (*pick!* {:mode :dmenu :prompt prompt :lines (vec items) :allow-empty? true}))

(defn prompt-input [prompt]
  (show-menu prompt []))

(defn clock-duration
  "`13:30`, `9:05`, `at 13:30`: seconds until that time today, or tomorrow
  once it has passed. 24h clock; nil for anything else."
  [input]
  (when-let [[_ h m] (re-matches #"(?:at\s*)?([01]?\d|2[0-3])[:.]([0-5]\d)" (str/lower-case (str/trim (str input))))]
    (let [now (java.time.LocalDateTime/now)
          at (.atTime (.toLocalDate now) (parse-long h) (parse-long m))
          at (if (.isAfter at now) at (.plusDays at 1))]
      (str (max 1 (.getSeconds (java.time.Duration/between now at))) "s"))))

(defn timer-duration
  "Positive durations: bare numbers mean minutes; units s, m, h, d combine.
  A clock time (`13:30`) means the next time the clock reads that."
  [input]
  (or (clock-duration input)
  (let [s (some-> input str/trim str/lower-case)
        s (if (and s (re-matches #"[0-9]+(?:\.[0-9]+)?" s)) (str s "m") s)
        parts (when s (re-seq #"([0-9]+(?:\.[0-9]+)?)\s*([smhd])" s))]
    (when (and (seq parts)
               (str/blank? (str/replace s #"[0-9]+(?:\.[0-9]+)?\s*[smhd]" "")))
      (let [seconds (reduce + (map (fn [[_ n unit]]
                                    (* (Double/parseDouble n)
                                       ({"s" 1 "m" 60 "h" 3600 "d" 86400} unit))) parts))]
        (when (and (pos? seconds) (<= seconds 31536000))
          (str (long (Math/ceil seconds)) "s")))))))

(defn schedule-timer! [kind duration message]
  ;; A transient user timer outlives the launcher. Never interpolate user text
  ;; into a shell, and disable systemd's own $VAR expansion too.
  (let [unit (str "launcher-" (name kind) "-" (java.util.UUID/randomUUID))
        command (if (= kind :reminder)
                  ["notify-send" "--urgency=critical" "--expire-time=0"
                   "--" (if (str/blank? message) "Timer" "Reminder")
                   (if (str/blank? message) "Time’s up!"
                       (str/escape message {\& "&amp;" \< "&lt;" \> "&gt;"}))]
                  ["noctalia" "msg" "session" "lock-and-suspend"])]
    (apply p/shell {:out :string :err :string :continue true}
           (into ["systemd-run" "--user" "--collect" "--quiet"
                  "--expand-environment=no" (str "--unit=" unit)
                  (str "--description=" (if (= kind :reminder) (if (str/blank? message) "Timer" (str "Reminder: " message)) "Lock & Suspend"))
                  (str "--on-active=" duration) "--timer-property=AccuracySec=1s"
                  "--"] command))))

(defn timer-action [kind]
  (let [message (if (= kind :reminder)
                  (some-> (prompt-input "Remind me: message (optional; blank = timer)") str/trim)
                  "Lock & Suspend")]
    (when message
      (loop []
        (when-let [input (some-> (prompt-input "When (30m, 1h 30m, 13:30; number = minutes)") str/trim not-empty)]
          (if-let [duration (timer-duration input)]
            (let [result (schedule-timer! kind duration message)]
              (p/shell {:continue true}
                       "notify-send" "--" "Timers"
                       (if (zero? (:exit result))
                         (str (if (= kind :reminder) (if (str/blank? message) "Timer" "Reminder") "Lock & Suspend") " scheduled " (if (clock-duration input) "at " "in ") (str/replace input #"(?i)^at\s*" ""))
                         (str "Could not schedule timer: " (:err result)))))
            (do
              (p/shell {:continue true} "notify-send" "Timers"
                       "Use a duration up to 365d (30m, 1h 30m) or a time (13:30).")
              (recur))))))))

(defn remind-me-action [] (timer-action :reminder))
(defn sleep-in-action [] (timer-action :sleep))

(defn- start-overlay! [kind seconds label]
  (try
    (overlay/start! kind seconds label)
    (catch Exception error
      (binding [*out* *err*] (println "cloncher:" (ex-message error)))
      (try (p/sh {:continue true} "notify-send" "--" "Could not open timer" (ex-message error))
           (catch Exception _ nil)))))

(declare configuration load-configuration!)

(defn scratchpad-note!
  "Open the quick-note editor for the configured `:scratchpad` Org file, by default
  in a helper process; `open!` receives the expanded path. Effects run outside the
  session's configuration binding, so read the current config file."
  ([] (scratchpad-note! note/start!))
  ([open!]
   (try
     (let [path (or (:scratchpad (load-configuration!))
                    (throw (ex-info (str "Set :scratchpad in " (config/path) " to an Org file") {})))]
       (open! (str/replace-first path #"^~(?=/)" (or (p/getenv "HOME") (str (fs/home))))))
     (catch Exception error
       (binding [*out* *err*] (println "cloncher:" (ex-message error)))
       (try (p/sh {:continue true} "notify-send" "--" "Could not open the note editor" (ex-message error))
            (catch Exception _ nil))))))

(defn- countdown-action []
  (loop [prompt "Countdown duration (5m, 1h 30m, 13:30)"]
    (when-let [input (some-> (prompt-input prompt) str/trim not-empty)]
      (if-let [duration (timer-duration input)]
        (when-some [label (prompt-input "Countdown label (optional)")]
          (let [duration (or (clock-duration input) duration)]
            (start-overlay! "countdown" (parse-long (subs duration 0 (dec (count duration)))) (str/trim label))))
        (recur "Use a duration up to 365d, or a time like 13:30")))))

(defn- stopwatch-action [] (start-overlay! "stopwatch" nil ""))

(defn- show-laps! [id]
  (when-let [laps (seq (:laps (overlay/control! id "laps")))]
    (show-menu "Stopwatch laps · split / total"
               (mapv (fn [{:keys [number split total]}] (str "Lap " number "  ·  " split "  /  " total))
                     (rseq (vec laps))))))

(defn- overlay-item []
  (when-let [{:keys [id kind label digits phase latest-lap]} (overlay/status)]
    (let [stopwatch? (= kind "stopwatch")
          paused? (= phase "paused")
          finished? (= phase "finished")
          can-lap? (and stopwatch? (= phase "running"))
          control (fn [title op] {:label title :action #(overlay/control! id op)})
          items (cond-> []
                  (not finished?) (conj (if paused?
                                          (control "Resume" "resume")
                                          (control "Pause" "pause")))
                  can-lap? (conj (control "Record lap" "lap"))
                  latest-lap (conj {:label "Lap history" :action #(show-laps! id)})
                  true (conj (control "Restart" "restart")
                             (control (if finished? "Dismiss" "Close") "close")))]
      (cond-> {:icon (if stopwatch? "󰔛" "󰥔") :label label :row-id "timer-overlay"
               :subtitle (str digits (cond paused? " · paused" finished? " · time’s up")
                              (when latest-lap (str " · lap " (:number latest-lap) ": " (:split latest-lap))))
               :hint "controls" :search (str kind " timer") :action {:prompt label :items items}}
        can-lap? (assoc :alt-hint "record lap" :alt-action #(overlay/control! id "lap"))))))

(defn cancel-timer! [unit]
  (let [result (p/sh {:continue true} "systemctl" "--user" "stop" unit
                     (str/replace unit #"\.timer$" ".service"))]
    (p/shell {:continue true} "notify-send" "--" "Timers"
             (if (zero? (:exit result)) "Timer cancelled"
                 (str "Could not cancel timer: " (:err result))))
    result))

(defn active-timer-items []
  (let [{:keys [exit out]} (p/sh {:continue true} "systemctl" "--user" "list-timers"
                                     "--output=json" "--no-pager"
                                     "launcher-reminder-*.timer" "launcher-sleep-*.timer")]
    (if-not (zero? exit)
      [{:icon "󰥔" :label "Could not read active timers"}]
      (let [now (* 1000 (System/currentTimeMillis))
            timers (->> (json/parse-string out true)
                        (filter #(and (number? (:next %)) (pos? (:next %))))
                        (sort-by :next))]
        (if (seq timers)
          (mapv (fn [{:keys [unit next]}]
                  (let [sleep? (str/starts-with? unit "launcher-sleep-")
                        description (p/sh {:continue true} "systemctl" "--user" "show"
                                          "--property=Description" "--value" unit)
                        label (str/trim (:out description))
                        seconds (max 0 (long (Math/ceil (/ (- next now) 1000000.0))))]
                    {:icon (if sleep? "󰒲" "󰥔")
                     :label (if (or (str/blank? label) (str/starts-with? label "/") (str/starts-with? label "[systemd-run]"))
                              (if sleep? "Lock & Suspend" "Reminder") label)
                     :row-id unit
                     :action #(cancel-timer! unit)
                     :hint "cancel"
                     :subtitle (str "Enter to cancel · in " (quot seconds 3600) "h "
                                (quot (mod seconds 3600) 60) "m " (mod seconds 60) "s")})) timers)
          [{:icon "󰥔" :label "No active timers"}])))))

(defn get-timers-menu-items []
  (into [{:icon "󰥔" :label "Remind me" :subtitle "timer / reminder" :action remind-me-action}
         {:icon "󰒲" :label "Sleep in" :action sleep-in-action}]
        (active-timer-items)))

(def pkg-path (str sources/dotfiles "/bin/menu/pkg.clj"))

(defn install-aur-action
  "Ask for an AUR query in the launcher, then open the fzf TUI in kitty."
  []
  (when-let [q (not-empty (some-> (prompt-input "AUR search") str/trim))]
    (p/shell {:continue true}
             "niri" "msg" "action" "spawn" "--"
             "kitty" "--class" "pkg" "--title" "Install"
             "-o" "shell=zsh"
             pkg-path "install" "--aur" q)))

(def reset-marker "[Reset to default]")
(def reset-all-marker "[Reset ALL to default]")

(defn unset-all-workspace-names
  "Unset names for all workspaces."
  []
  (let [result (p/shell {:out :string} "niri" "msg" "workspaces")
        lines (str/split-lines (:out result))
        ;; Parse workspace names from lines like "   1 \"name\"" or " * 2 \"name\""
        workspace-names (->> lines
                             (map str/trim)
                             (filter #(re-matches #"^\*?\s*\d+\s+\".*\"" %))
                             (map #(second (re-find #"^\*?\s*\d+\s+\"(.*)\"" %)))
                             (filter some?))]
    (doseq [name workspace-names]
      (p/shell "niri" "msg" "action" "unset-workspace-name" name))))

(defn rename-workspace
  "Prompt for a new workspace name and rename the current workspace.
  Select the reset marker to unset the workspace name."
  []
  (let [selection (show-menu "Workspace name" [reset-marker reset-all-marker])]
    (cond
      (nil? selection) nil ; Canceled
      (= selection reset-marker) (p/shell "niri" "msg" "action" "unset-workspace-name")
      (= selection reset-all-marker) (unset-all-workspace-names)
      :else (p/shell "niri" "msg" "action" "set-workspace-name" selection))))

(defn get-dnd-mode
  "Return Noctalia's notification DND state as :on or :off."
  []
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true}
                                    "noctalia" "msg" "notification-dnd-status")]
    (if (and (zero? exit)
             (contains? #{"on" "true" "enabled"} (-> out str/trim str/lower-case)))
      :on
      :off)))

(defn cycle-dnd
  "Toggle Noctalia's notification Do Not Disturb state."
  []
  (p/shell "noctalia" "msg" "notification-dnd-toggle"))

(defn get-dnd-menu-item
  "Get the Noctalia DND menu item with current state."
  []
  (case (get-dnd-mode)
    :on {:icon "󰂛" :label "Do not disturb: On" :action cycle-dnd}
    :off {:icon "󰂚" :label "Do not disturb: Off" :action cycle-dnd}))

(def ^:private ambient (str sources/dotfiles "/bin/menu/ambient.clj"))

(defn get-themes-menu
  "The ambient widget's switches, through the same `ambient.clj`: a manual
  theme change turns ambient auto off and picks a wallpaper for the mode,
  exactly as the widget's buttons do. The toggles read ambient's policy file
  (defaults as in ambient.clj), so no sensor or DDC read is paid here."
  []
  (let [policy (merge {:auto true :brightnessAuto true :wallpaperAuto true}
                      (try (json/parse-string (slurp (str (fs/path (fs/xdg-cache-home) "noctalia-ambient" "policy.json"))) true)
                           (catch Exception _ nil)))
        dark? (not= "light" (some-> (p/sh {:continue true} "noctalia" "msg" "theme-mode-get") :out str/trim str/lower-case))
        toggle (fn [icon label k cmd]
                 (let [on? (boolean (get policy k))]
                   {:icon icon :label (str label ": " (if on? "On" "Off"))
                    :action (str ambient " " cmd (if on? " off" " on"))}))]
    [(if dark?
       {:icon "󰖨" :label "Switch to light" :action (str ambient " set light")}
       {:icon "󰖔" :label "Switch to dark" :action (str ambient " set dark")})
     (toggle "󰖨" "Auto theme" :auto "auto")
     (toggle "󰃠" "Auto brightness" :brightnessAuto "brightness-auto")
     (toggle "󰸉" "Wallpaper follows theme" :wallpaperAuto "wallpaper-auto")
     {:icon "󰔎" :label "Theme settings"
      :action "noctalia msg settings-open appearance"}]))

;; Keybindings, read back out of the niri config. mybinds.kdl comes first,
;; so a personal bind wins over the base one for the same key.
(def niri-bind-files
  [(str sources/dotfiles "/niri/.config/niri/mybinds.kdl")
   (str sources/dotfiles "/niri/.config/niri/base-binds.kdl")])

(defn- bind-description
  "A bind's `hotkey-overlay-title`, else its action in a few words: the
  program names of a spawn, or the niri action itself."
  [title body]
  (or title
      (let [[_ action args] (re-find #"^([a-z-]+)\s*(.*?);?$" (str/trim body))]
        (if (= "spawn" action)
          (->> (re-seq #"\x22([^\x22]*)\x22" (str args))
               (map #(last (str/split (second %) #"/")))
               (remove #{"sh" "-c" "msg" "action" "launch" "--class" "--title"})
               (take 4)
               (str/join " "))
          (str/trim (str/replace (str action " " args) #"[-;]" " "))))))

(defn parse-niri-keybindings
  "Every bind in `niri-bind-files` as {:key :description}, one per key."
  []
  (->> niri-bind-files
       (filter fs/exists?)
       (mapcat #(str/split-lines (slurp %)))
       (keep (fn [line]
               (when-let [[_ key opts body] (re-find #"^\s*([A-Za-z0-9+]+)((?:\s+[a-z-]+=(?:\x22[^\x22]*\x22|\S+))*)\s*\{(.*)\}\s*$" line)]
                 {:key key
                  :description (bind-description
                                (second (re-find #"hotkey-overlay-title=\x22([^\x22]*)\x22" (str opts)))
                                body)})))
       (reduce (fn [acc b] (if (some #(= (:key b) (:key %)) acc) acc (conj acc b))) [])))

(defn format-key
  "Format key combo: Mod→❖, Hyper→✦"
  [key]
  (-> key
      (str/replace "Mod+Shift+Ctrl+Alt+" "✦")
      (str/replace "Mod+" "❖")
      (str/replace "Shift+" "⇧")
      (str/replace "Control+" "⌃")
      (str/replace "Ctrl+" "⌃")
      (str/replace "Alt+" "⌥")
      (str/replace "XF86" "")))

(defn get-keybindings-menu
  "Every niri keybinding, as reference rows."
  []
  (mapv (fn [{:keys [key description]}]
          {:icon "󰌌" :label (format "%-12s %s" (format-key key) description) :action nil})
        (parse-niri-keybindings)))

(def wf-recorder-path (str sources/dotfiles "/bin/menu/wf_recorder.clj"))

(defn get-record-menu-items
  "Get record menu items, with stop at top if recording is active."
  []
  (let [base-items [{:icon "󱂬" :label "Region"
                     :action (str wf-recorder-path " region")}
                      {:icon "󱂬" :label "Region + audio"
                       :action (str wf-recorder-path " region --mic")}
                    {:icon "󰹑" :label "Screen"
                     :action (str wf-recorder-path " screen")}
                      {:icon "󰹑" :label "Screen + audio"
                       :action (str wf-recorder-path " screen --mic")}
                    {:icon "󰖯" :label "Window"
                     :action (str wf-recorder-path " window")}
                    {:icon "󰖯" :label "Window + audio"
                     :action (str wf-recorder-path " window --mic")}
                    {:icon "󰍬" :label "Audio only" :subtitle "mic + speakers"
                     :action (str wf-recorder-path " audio")}]
        status-edn (-> (p/shell {:out :string :continue true} wf-recorder-path "status" "--edn")
                       :out
                       str/trim
                       read-string)
        recording? (:recording status-edn)]
    (if recording?
      (into [{:icon "󰓛" :label "Stop recording"
              :action (str wf-recorder-path " stop")}]
            base-items)
      base-items)))

;; Tailscale

(defn- tailnets []
  (:tailnets (configuration)))

(defn current-tailnet
  "Current tailnet name from `tailscale status --json`, or nil if unavailable.
  Does not need root."
  []
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true}
                                    "tailscale" "status" "--json")]
    (when (zero? exit)
      (try
        (-> out (json/parse-string true) :CurrentTailnet :Name)
        (catch Exception _ nil)))))

(defn notify-tailscale
  "Desktop notification for tailscale actions.
  Replaces the previous tailscale notification instead of stacking."
  ([body] (notify-tailscale body nil))
  ([body urgency]
   (apply p/shell {:out :string :err :string :continue true}
          (cond-> ["notify-send"
                   "--app-name=Tailscale"
                   "--hint=string:x-canonical-private-synchronous:tailscale"]
            urgency (conj (str "--urgency=" (name urgency)))
            :always (concat ["Tailscale" body])))))

(defn run-switch
  "Run `tailscale switch <account>`, falling back to passwordless sudo.
  Plain invocation works when `tailscale set --operator=$USER` was applied."
  [account]
  (let [plain (p/shell {:out :string :err :string :continue true}
                       "tailscale" "switch" account)]
    (if (zero? (:exit plain))
      plain
      (p/shell {:out :string :err :string :continue true}
               "sudo" "-n" "tailscale" "switch" account))))

(defn wait-for-tailnet
  "Poll the current tailnet until it equals `expected` or `timeout-ms` elapses."
  [expected timeout-ms]
  (loop [waited 0]
    (let [now (current-tailnet)]
      (if (or (= now expected) (>= waited timeout-ms))
        now
        (do (Thread/sleep 300)
            (recur (+ waited 300)))))))

(defn switch-tailnet
  "Switch to the given tailnet and notify with the actual outcome."
  [{:keys [label tailnet account]}]
  (notify-tailscale (str "Switching to " label "…"))
  (let [{:keys [exit out err]} (run-switch account)
        message (-> (str err out) str/trim str/split-lines last str/trim)]
    (if (zero? exit)
      (let [now (wait-for-tailnet tailnet 5000)]
        (if (= now tailnet)
          (notify-tailscale (str "Connected to " label " (" tailnet ")"))
          (notify-tailscale (str "Switched to " label ", tailnet reports "
                                 (or now "unknown"))
                            :normal)))
      (notify-tailscale (str "Switch to " label " failed: "
                             (if (str/blank? message) (str "exit " exit) message))
                        :critical))))

(defn other-tailnet
  "The tailnet that is not currently active (defaults to the first one)."
  [current]
  (let [configured (tailnets)]
    (or (first (remove #(= current (:tailnet %)) configured))
        (first configured))))

(defn toggle-tailnet
  "Switch to whichever configured tailnet is not currently active."
  []
  (when-let [target (other-tailnet (current-tailnet))]
    (switch-tailnet target)))

(defn get-tailscale-menu-items
  "Tailscale menu: the connected tailnet as a status row, then a `Switch to`
  row for every other tailnet. The status row does nothing on Enter: a
  silent switch from it was too easy to trigger, and the target was not
  visible as a row of its own."
  []
  (let [current (current-tailnet)
        configured (tailnets)
        now (first (filter #(= current (:tailnet %)) configured))]
    (into [{:state true :icon "󰖂"
            :label (str "Tailnet: " (or (:label now) current "none"))
            :subtitle (if current "connected" "off") :warn (nil? current)
            :action nil}]
          (for [{:keys [label tailnet] :as tn} configured :when (not= tailnet current)]
            {:icon "󰁔" :label (str "Switch to " label) :hint "switch"
             :action #(switch-tailnet tn)}))))

(defn- noctalia-on?
  "Whether `noctalia msg <cmd>` (a *-status command) reports on."
  [cmd]
  (try (contains? #{"on" "true" "enabled"}
                  (-> (p/sh {:continue true} "noctalia" "msg" cmd) :out str/trim str/lower-case))
       (catch Exception _ false)))

(defn mullvad-connected?
  "Whether Mullvad is connected (or connecting): `mullvad status` leads with it."
  []
  (try (-> (p/sh {:continue true} "mullvad" "status") :out str/trim
           (str/starts-with? "Connect"))
       (catch Exception _ false)))

(defn caffeine-on?
  "Noctalia has no caffeine-status command; its caffeine is a logind idle
  inhibitor with the reason `Caffeine`, so that is what is looked for."
  []
  (try (boolean (some #(and (str/includes? % "noctalia") (re-find #"\bidle\b.*\bCaffeine\b" %))
                      (str/split-lines (:out (p/sh {:continue true} "systemd-inhibit" "--list"
                                                   "--no-pager" "--no-legend")))))
       (catch Exception _ false)))

(defn- connection-command
  "Bound connection reads/actions, including when a desktop service is absent."
  [seconds & command]
  (try
    (apply p/sh {:continue true :extra-env {"LC_ALL" "C"}}
           "timeout" "--signal=KILL" (str seconds "s") command)
    (catch Exception e {:exit 127 :out "" :err (ex-message e)})))

(defn- connection-result! [title result]
  (when-not (zero? (:exit result))
    (let [message (str/trim (str (:err result) " " (:out result)))]
      (p/sh {:continue true} "notify-send" "--" title
            (str/escape (if (str/blank? message) "Command failed or timed out"
                            (subs message 0 (min 240 (count message))))
                        {\& "&amp;" \< "&lt;" \> "&gt;"}))))
  result)

(defn- connection-terminal!
  "Compositor-owned terminal for interactive Bluetooth pairing and QR display.
  Arguments stay separate: neither network nor device names enter a shell."
  [title & command]
  (connection-result!
   title
   (apply p/sh {:continue true} "niri" "msg" "action" "spawn" "--"
          "kitty" "--title" title "-o" "shell=zsh" command)))

(defn- nmcli-fields
  "Split terse nmcli output, respecting escaped colons and backslashes."
  [line]
  (loop [chars (seq line) field [] fields [] escaped? false]
    (if-let [c (first chars)]
      (cond
        escaped? (recur (next chars) (conj field c) fields false)
        (= c \\) (recur (next chars) field fields true)
        (= c \:) (recur (next chars) [] (conj fields (apply str field)) false)
        :else (recur (next chars) (conj field c) fields false))
      (conj fields (apply str (cond-> field escaped? (conj \\)))))))

(defn- wifi-networks [out]
  (->> (str/split-lines out)
       (keep (fn [line]
               (let [[active ssid bssid signal security device :as fields] (nmcli-fields line)]
                 (when (and (= 6 (count fields)) (not (str/blank? ssid))
                            (re-matches #"(?i)(?:[0-9a-f]{2}:){5}[0-9a-f]{2}" bssid)
                            (not (str/blank? device)))
                   {:ssid ssid :bssid bssid :signal (or (parse-long signal) 0)
                    :security security :device device :connected? (= "*" active)}))))
       ;; One network per interface/security mode, not one row per access point.
       ;; Prefer its connected AP, even when another AP has a stronger signal.
       (sort-by (juxt (comp not :connected?) (comp - :signal) :ssid :bssid))
       (reduce (fn [{:keys [seen] :as acc} network]
                 (let [k ((juxt :ssid :security :device) network)]
                   (if (contains? seen k) acc
                       (-> acc (update :seen conj k) (update :rows conj network)))))
               {:seen #{} :rows []})
       :rows
       ;; Deduplication chooses the connected AP; display order is signal-only.
       (sort-by (juxt (comp - :signal) :ssid :bssid))
       vec))

(defn prompt-secret
  "Ask for a masked value. Preserve spaces and treat cancellation as nil."
  [prompt]
  (let [value (*pick!* {:mode :secret :prompt prompt :timeout-ms 120000})]
    (when (and (seq value) (not (re-find #"[\p{Cntrl}]" value))) value)))

(defn- wifi-auth-prompt
  "Recognize complete C-locale nmcli prompts, which have no newline."
  [line]
  (when (re-matches #"(?i)(?:password|username|identity|pin|private key password)(?: \([a-z0-9_.-]+\))?: " line)
    (str/replace line #": $" "")))

(defn- answer-wifi-prompts!
  "Bridge nmcli --ask through anonymous pipes. No child output is logged: it
  can contain credentials or echoed input. Cancellation kills the request."
  [reader writer ask! cancel!]
  (loop [line "" prompts 0]
    (let [c (.read reader)]
      (when-not (= -1 c)
        (let [line (cond
                     (contains? #{10 13} c) ""
                     (< (count line) 2048) (str line (char c))
                     :else line)]
          (if-let [label (wifi-auth-prompt line)]
            (if-let [value (when (< prompts 8) (ask! label))]
              (do (.write writer (str value "\n"))
                  (.flush writer)
                  (recur "" (inc prompts)))
              (cancel!))
            (recur line prompts)))))))

(defn- saved-wifi-profile?
  "Match available profiles by SSID, not their editable connection names.
  Read metadata only, on activation; never retrieve stored passwords."
  [{:keys [ssid device]}]
  (let [available (connection-command 4 "nmcli" "--get-values"
                                       "CONNECTIONS.AVAILABLE-CONNECTION-PATHS"
                                       "device" "show" device)
        paths (when (zero? (:exit available))
                (re-seq #"/org/freedesktop/NetworkManager/Settings/[0-9]+" (:out available)))]
    (when (seq paths)
      (let [profiles (apply connection-command 4
                            (concat ["nmcli" "--terse" "--escape" "yes" "--fields"
                                     "802-11-wireless.ssid" "connection" "show"]
                                    (mapcat #(vector "path" %) paths)))]
        (and (zero? (:exit profiles))
             (some #{["802-11-wireless.ssid" ssid]}
                   (map nmcli-fields (str/split-lines (:out profiles)))))))))

(defn- wifi-connect-command! [{:keys [ssid bssid device security] :as network}]
  (let [needs-password? (and (not (str/blank? security))
                             (not (contains? #{"--" "OWE"} security))
                             (not (saved-wifi-profile? network)))
        password (when needs-password?
                   (prompt-secret (str (or ssid "Wi-Fi") " — Password")))]
    (if (and needs-password? (nil? password))
      {:exit 1 :cancelled true}
      (let [child (p/process {:extra-env {"LC_ALL" "C"} :err :out}
                             "timeout" "--signal=KILL" "180s"
                             "nmcli" "--colors" "no" "--ask" "--wait" "120"
                             "device" "wifi" "connect" bssid "ifname" device)
            cancelled? (atom false)
            pending-password (atom password)]
        (try
          (with-open [reader (io/reader (:out child)) writer (io/writer (:in child))]
            (answer-wifi-prompts! reader writer
              (fn [label]
                (if needs-password?
                  ;; Supply the pre-collected answer once. Never reprompt during
                  ;; an unsaved network's activation or expose it in arguments.
                  (when (re-matches #"(?i)password(?: \([a-z0-9_.-]+\))?" label)
                    (let [value @pending-password]
                      (reset! pending-password nil)
                      value))
                  ;; Saved credentials normally need no prompt. Missing/stale
                  ;; credentials may still require NetworkManager's challenge.
                  (prompt-secret (str (or ssid "Wi-Fi") " — " label))))
              #(do (reset! cancelled? true) (p/destroy-tree child))))
          (let [exit (:exit @child)]
            (cond
              @cancelled? {:exit 1 :cancelled true}
              (zero? exit) {:exit 0}
              :else {:exit exit :err "Could not connect. Check the password and network availability, or open Wi-Fi settings for advanced authentication."}))
          (catch Exception _
            (p/destroy-tree child)
            @child
            {:exit 1 :err "Wi-Fi authentication failed or timed out."}))))))

(defn- connect-wifi! [network]
  (let [result (wifi-connect-command! network)]
    (when-not (:cancelled result)
      (connection-result! "Wi-Fi connection" result))
    result))

(defn scan-wifi! []
  (connection-result! "Wi-Fi scan"
                      (connection-command 12 "nmcli" "--wait" "8" "device" "wifi" "list" "--rescan" "yes")))

(declare copy!)

(defn- wifi-profile!
  "Resolve the selected active profile by UUID, never its ambiguous name.
  Secrets are read only for an explicit Copy action, never while browsing."
  [{:keys [ssid device]} secrets?]
  (let [active (connection-command 4 "nmcli" "--get-values" "GENERAL.CON-UUID"
                                   "device" "show" device)
        uuid (str/trim (:out active))]
    (if (and (zero? (:exit active))
             (re-matches #"(?i)[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}" uuid))
      (let [result (apply connection-command 4
                          (cond-> ["nmcli" "--terse" "--escape" "yes" "--fields"
                                   (if secrets?
                                     "802-11-wireless.ssid,802-11-wireless-security"
                                     "802-11-wireless.ssid")]
                            secrets? (into ["--show-secrets"])
                            true (into ["connection" "show" "uuid" uuid])))
            fields (when (zero? (:exit result))
                     (into {} (map nmcli-fields (str/split-lines (:out result)))))]
        (if (and fields (= ssid (get fields "802-11-wireless.ssid")))
          {:uuid uuid :fields fields}
          ;; Never forward secret-bearing stdout/stderr to notifications or logs.
          (do (connection-result! "Wi-Fi" {:exit 1 :err "Could not read the selected network. Reopen Wi-Fi networks and try again."})
              nil)))
      (do (connection-result! "Wi-Fi" {:exit 1 :err "The selected network is no longer connected. Reopen Wi-Fi networks."})
          nil))))

(defn- copy-wifi-access! [{:keys [ssid security] :as network}]
  (when-let [{:keys [fields]} (wifi-profile! network true)]
    (let [key-mgmt (get fields "802-11-wireless-security.key-mgmt" "")
          open? (contains? #{"" "--" "owe"} key-mgmt)
          personal? (contains? #{"wpa-psk" "sae" "wpa-none" "none"} key-mgmt)
          password (if (= "none" key-mgmt)
                     (get fields (str "802-11-wireless-security.wep-key"
                                      (get fields "802-11-wireless-security.wep-tx-keyidx" "0")))
                     (get fields "802-11-wireless-security.psk"))]
      (cond
        (not (or open? personal?))
        (connection-result! "Wi-Fi sharing"
                            {:exit 1 :err "This network uses enterprise authentication, not a shared Wi-Fi password."})
        (and (not open?) (or (str/blank? password) (= "--" password)))
        (connection-result! "Wi-Fi sharing"
                            {:exit 1 :err "The saved Wi-Fi password is unavailable. Check Wi-Fi settings."})
        :else
        (let [result (copy! (str "SSID: " ssid "\nSecurity: "
                                 (if (str/blank? security) "open" security)
                                 (when-not open? (str "\nPassword: " password))))]
          (connection-result! "Copy Wi-Fi access details"
                              {:exit (:exit result) :err "Could not write to the clipboard."}))))))

(defn- wifi-qr-escape
  "Escape a value for a Wi-Fi QR payload: backslash, semicolon, comma, colon and quote."
  [value]
  (str/replace value #"([\\;,:\"])" "\\\\$1"))

(defn- wifi-qr-preview
  "The standard `WIFI:` QR code for the network, as SVG for the preview pane. The
  password reaches qrencode on stdin and the SVG stays in memory: nothing in argv,
  rows, or files carries it."
  [{:keys [ssid] :as network}]
  (if-let [{:keys [fields]} (wifi-profile! network true)]
    (let [key-mgmt (get fields "802-11-wireless-security.key-mgmt" "")
          type (cond (contains? #{"" "--" "owe"} key-mgmt) "nopass"
                     (= "none" key-mgmt) "WEP"
                     (contains? #{"wpa-psk" "sae"} key-mgmt) "WPA")
          password (if (= "WEP" type)
                     (get fields (str "802-11-wireless-security.wep-key"
                                      (get fields "802-11-wireless-security.wep-tx-keyidx" "0")))
                     (get fields "802-11-wireless-security.psk"))]
      (cond
        (nil? type)
        {:notice "This network uses enterprise sign-in, which a QR code cannot share."}
        (and (not= "nopass" type) (or (str/blank? password) (= "--" password)))
        {:notice "The saved Wi-Fi password is unavailable. Check Wi-Fi settings."}
        :else
        (let [payload (str "WIFI:T:" type ";S:" (wifi-qr-escape ssid) ";"
                           (when (not= "nopass" type) (str "P:" (wifi-qr-escape password) ";")) ";")
              {:keys [exit out]} (p/sh {:in payload :continue true}
                                       "qrencode" "--type" "SVG" "--margin" "2" "--output" "-")]
          (if (zero? exit)
            {:detail (str "Scan to join  ·  " ssid) :svg out}
            {:notice "Could not draw the QR code. Is qrencode installed?"}))))
    {:notice "Could not read the network's saved password."}))

(defn- wifi-qr-view
  "A view whose one row shows the network's QR code beside it."
  [{:keys [ssid] :as network}]
  {:prompt (str "Scan to join " ssid)
   :items [{:icon "󰐲" :label ssid :subtitle "scan with a phone camera"
            :volatile true
            :preview {:id (str "wifi qr " ssid) :load #(wifi-qr-preview network)}}]})

(defn- disconnect-wifi! [{:keys [device] :as network}]
  (when (wifi-profile! network false)
    (connection-result! "Wi-Fi disconnect"
                        (connection-command 12 "nmcli" "--wait" "8"
                                            "device" "disconnect" device))))

(defn- forget-wifi! [{:keys [ssid] :as network}]
  (when-let [{:keys [uuid]} (wifi-profile! network false)]
    (when (= "Forget network" (show-menu (str "Forget " ssid "?")
                                           ["Cancel" "Forget network"]))
      (connection-result! "Forget Wi-Fi network"
                          (connection-command 12 "nmcli" "--wait" "8"
                                              "connection" "delete" "uuid" uuid)))))

(defn- wifi-network-menu [{:keys [ssid] :as network}]
  {:prompt ssid
   :items [{:icon "󰅍" :label "Copy access details" :hint "copy network and password"
            :volatile true :action #(copy-wifi-access! network)}
           {:icon "󰐲" :label "Show QR code" :hint "share Wi-Fi access"
            :volatile true :action (wifi-qr-view network)}
           {:icon "󰖪" :label "Disconnect" :hint "disconnect from this network"
            :volatile true :action #(disconnect-wifi! network)}
           {:icon "󰆴" :label "Forget" :hint "remove saved connection and password"
            :subtitle "confirmation required · also disconnects"
            :volatile true :warn true :action #(forget-wifi! network)}]})

(defn get-wifi-items []
  (let [radio (future (connection-command 3 "nmcli" "radio" "wifi"))
        {:keys [exit out]} (connection-command 4 "nmcli" "--wait" "2" "--terse"
                                              "--escape" "yes" "--fields"
                                              "IN-USE,SSID,BSSID,SIGNAL,SECURITY,DEVICE"
                                              "device" "wifi" "list" "--rescan" "no")
        on? (= "enabled" (str/trim (:out @radio)))
        networks (when (zero? exit) (wifi-networks out))]
    (vec (concat
          (for [{:keys [ssid bssid device signal security connected?] :as network} networks]
            {:icon "󰖩" :label ssid :row-id (str "wifi:" device ":" bssid)
             :section 1 :order (- signal)
             :volatile true :state connected? :hint (if connected? "network actions" "connect")
             :subtitle (str (when connected? "connected · ") signal "% · "
                            (if (str/blank? security) "open" security) " · " device)
             :action (if connected?
                       (wifi-network-menu network)
                       #(connect-wifi! network))})
          (when-not (seq networks)
            [{:icon "󰖩" :volatile true :section 1
              :label (cond (not (zero? exit)) "Could not read Wi-Fi networks"
                           (not on?) "Turn on Wi-Fi to see networks"
                           :else "No networks found — scan to refresh")}])
          [{:state true :volatile true :section 2 :icon "󰖩"
            :label (str "Wi-Fi: " (if on? "On" "Off"))
            :subtitle (if on? "on" "off") :warn (not on?)
            :hint (if on? "turn off" "turn on") :refresh true
            :action "noctalia msg wifi-toggle"}]))))

(defn- bluetooth-inventory [out]
  (let [objects (-> (json/parse-string out) (get "data") first)]
    (when-not (map? objects)
      (throw (ex-info "Invalid BlueZ inventory" {})))
    {:on? (boolean (some #(true? (get-in (val %) ["org.bluez.Adapter1" "Powered" "data"])) objects))
     :devices (->> objects
                   (keep (fn [[path interfaces]]
                           (when-let [props (get interfaces "org.bluez.Device1")]
                             (let [prop #(get-in props [% "data"])
                                   address (prop "Address")]
                               {:path path :address address
                                :label (or (prop "Alias") (prop "Name") address)
                                :connected? (true? (prop "Connected"))
                                :paired? (true? (prop "Paired"))}))))
                   (sort-by (juxt (comp not :connected?) (comp not :paired?) :label :path))
                   vec)}))

(defn scan-bluetooth! []
  ;; This client's discovery session ends when bluetoothctl exits; do not
  ;; issue scan off, which could interfere with somebody else's discovery.
  (connection-result! "Bluetooth scan"
                      (connection-command 12 "bluetoothctl" "--timeout" "8" "scan" "on")))

(defn get-bluetooth-items []
  (let [{:keys [exit out]} (connection-command 4 "busctl" "--system" "--json=short" "--timeout=2"
                                              "call" "org.bluez" "/"
                                              "org.freedesktop.DBus.ObjectManager" "GetManagedObjects")
        inventory (when (zero? exit) (try (bluetooth-inventory out) (catch Exception _ nil)))
        {:keys [on? devices]} inventory]
    (vec (concat
          [{:state true :volatile true :icon "󰂯" :label (str "Bluetooth: " (if on? "On" "Off"))
            :subtitle (if on? "on" "off") :warn (not on?)
            :hint (if on? "turn off" "turn on") :refresh true
            :action "noctalia msg bluetooth-toggle"}]
          (for [{:keys [path address label connected? paired?]} devices]
            {:icon "󰂯" :label label :row-id (str "bluetooth:" path) :volatile true
             :state connected? :hint (cond connected? "disconnect" paired? "connect" :else "pair and connect")
             :subtitle (str (cond connected? "connected" paired? "paired" :else "nearby · not paired")
                            " · " address)
             :action (if (or connected? paired?)
                       #(connection-result! "Bluetooth"
                                            (connection-command 30 "bluetoothctl" "--timeout" "25"
                                                                (if connected? "disconnect" "connect") path))
                       ;; Pair only unpaired devices: bluetoothctl pair removes
                       ;; an existing bond before pairing again. Its agent can
                       ;; ask for a PIN/confirmation, and pair trusts/connects.
                       #(connection-terminal! "Bluetooth pairing" "bluetoothctl" "--agent" "KeyboardDisplay"
                                              "--timeout" "90" "pair" path))})
          (when-not (seq devices)
            [{:icon "󰂯" :volatile true
              :label (cond (nil? inventory) "Could not read Bluetooth devices"
                           (not on?) "Turn on Bluetooth to discover devices"
                           :else "No Bluetooth devices — scan to discover")}])))))

(defn get-connect-items
  "Enter lists networks/devices in the launcher; Ctrl+Enter toggles the radio."
  []
  (let [wifi (future (noctalia-on? "wifi-status"))
        bt (future (noctalia-on? "bluetooth-status"))
        mullvad (future (mullvad-connected?))
        tailnet (future (current-tailnet))]
    [{:icon "󰖩" :label (str "Wi-Fi: " (if @wifi "On" "Off"))
      :subtitle "connect · disconnect · networks" :hint "show networks" :action :wifi
      :alt-hint (if @wifi "turn off" "turn on") :alt-action "noctalia msg wifi-toggle"}
     {:icon "󰂯" :label (str "Bluetooth: " (if @bt "On" "Off"))
      :subtitle "connect · disconnect · pair devices" :hint "show devices" :action :bluetooth
      :alt-hint (if @bt "turn off" "turn on") :alt-action "noctalia msg bluetooth-toggle"}
     {:icon "󰦝" :label (str "Mullvad: " (if @mullvad "Connected" "Off"))
      :action (if @mullvad "mullvad disconnect" "mullvad connect")}
     {:icon "󰖂" :label (str "Tailscale: " (or (:label (first (filter #(= @tailnet (:tailnet %)) (tailnets))))
                                              @tailnet "Off"))
      :action :tailscale}]))

(defn get-focus-items []
  (into [{:icon "󰥔" :label "Countdown" :subtitle "on-screen timer" :action countdown-action}
         {:icon "󰔛" :label "Stopwatch" :subtitle "on-screen elapsed time" :action stopwatch-action}]
        (keep identity
              [(try (get-dnd-menu-item) (catch Exception _ nil))
               {:icon "󰅶" :label (str "Caffeine: " (if (caffeine-on?) "On" "Off")) :subtitle "keep awake"
                :action "noctalia msg caffeine-toggle"}])))

(def mail-triage-path (str sources/dotfiles "/bin/ai/jobs/mail_triage.clj"))

;; What bin/email.clj recorded about the last sync, per account.
(def mail-sync-status-path (str (System/getProperty "user.home") "/.cache/mail-sync/status.json"))

(defn- short-age
  "`12m`, `5h`, `3d`: how long ago unix time `t` was."
  [t]
  (let [s (max 0 (- (quot (System/currentTimeMillis) 1000) (or t 0)))]
    (cond (< s 3600) (str (max 1 (quot s 60)) "m")
          (< s 86400) (str (quot s 3600) "h")
          :else (str (quot s 86400) "d"))))

(defn- sender-name
  "The display name of a From header, else its address."
  [from]
  (let [from (str from)
        n (str/trim (str/replace (first (str/split from #"<")) #"^\"|\"$" ""))]
    (if (str/blank? n) (str/replace from #"[<>]" "") n)))

(defn- fit
  "`s` cut to `n` characters, with an ellipsis at the end when cut."
  [s n]
  (if (<= (count s) n) s (str (str/trimr (subs s 0 (dec n))) "…")))

(defn get-mail-code-items
  "The codes and links mail triage found, codes first, newest first. Enter
  copies either; Ctrl+Enter opens a link, through the same `act` the
  widget's chips call. The subtitle is who sent it and how long ago - the
  subject is too long to read there. Links whose label is a bare number
  (a survey's 0-10 rating buttons) are dropped."
  []
  (let [{:keys [exit out]} (p/sh {:continue true} mail-triage-path "actions" "--json")
        msgs (when (zero? exit) (try (json/parse-string out true) (catch Exception _ nil)))
        rows (for [m msgs
                   :let [k (str (:message-id m) "|" (:date-unix m))
                         age (short-age (:date-unix m))]
                   [i a] (map-indexed vector (:actions m))
                   :let [code? (= "code" (:kind a))
                         label (if code? (str (:value a)) (or (not-empty (:label a)) (:host a) "Link"))
                         what (when (and code? (not (str/blank? (:label a)))) (str (:label a) " · "))
                         sender (fit (sender-name (:from m)) (- 32 (count what) (count age)))]
                   :when (or code? (not (re-matches #"\d{1,2}" label)))]
               (cond-> {:icon (if code? "󰌆" "󰌹")
                        :label label
                        :subtitle (str what sender " · " age)
                        :row-id (str "mail " k " " (inc i))
                        :hint "copy"
                        :volatile true
                        :code? code?
                        :date (:date-unix m)
                        :action #(deref (p/process {:in (str (:value a)) :out :discard :err :discard} "wl-copy"))}
                 (not code?) (assoc :alt-hint "open"
                                    :alt-action #(p/sh {:continue true} mail-triage-path "act" k (str (inc i))))))]
    (if (seq rows)
      (->> rows (sort-by (juxt (comp not :code?) (comp - :date))) (mapv #(dissoc % :code? :date)))
      [{:icon "󰇮" :label "No codes or links in triaged mail"}])))

(defn- byte-size [n]
  (cond (< n 1024) (str n " B")
        (< n (* 1024 1024)) (str (quot n 1024) " KB")
        :else (format "%.1f MB" (/ n 1048576.0))))

(defn- clipboard-preview
  "The preview pane's content for a history entry: the image, or its text wrapped
  as written (up to 8,000 characters)."
  [entry image? detail]
  (try
    (let [bytes (clipboard/payload entry)]
      (if image?
        {:detail detail :bytes bytes}
        (let [text (str/trim (String. bytes "UTF-8"))]
          {:detail detail :text (if (> (count text) 8000) (str (subs text 0 8000) "…") text)})))
    (catch Exception error
      {:detail detail :notice (ex-message error)})))

(defn- export-clipboard-entry!
  "Write a history entry to ~/Downloads and say where."
  [{:keys [id mime] :as entry} image?]
  (let [file (fs/path (or (p/getenv "HOME") (str (fs/home))) "Downloads"
                      (str "clipboard-" id "." (if image? (subs mime 6) "txt")))]
    (fs/create-dirs (fs/parent file))
    (with-open [out (io/output-stream (str file))]
      (.write out (clipboard/payload entry)))
    (p/shell {:continue true} "notify-send" "--" "Exported clipboard entry" (str file))))

(defn get-clipboard-items
  "The clipboard history, saved entries first, then newest first, with a preview pane
  like the file browser's. Enter makes the entry the clipboard again, with its own MIME
  type, and pastes it into the focused window. Ctrl+S saves or unsaves, Ctrl+D deletes,
  and Ctrl+Enter shows every option."
  []
  (if-not (:clipboard-history (configuration))
    [{:icon "\uF0EA" :label "Clipboard history is off"
      :subtitle (str "set :clipboard-history true in " (config/path))}]
    (try
      (let [rows (for [{:keys [id mime preview bytes at saved] :as entry} (clipboard/history)
                       :let [image? (str/starts-with? mime "image/")
                             lines (when-not image? (remove str/blank? (str/split-lines (or preview ""))))
                             age (short-age (quot at 1000))
                             kind (if image? (str (str/upper-case (subs mime 6)) " image") "Text")
                             paste #(clipboard/paste! entry)
                             save {:hint (if saved "unsave" "save") :action #(clipboard/toggle-saved! entry)}
                             delete {:hint "delete" :action #(clipboard/delete! entry)}]]
                   {:icon (cond saved "\uF02E" image? "\uF03E" :else "\uF0F6")
                    :label (if image?
                             "Image"
                             (fit (str/replace (str/trim (or (first lines) "")) #"\s+" " ") 120))
                    :subtitle (str/join " · " (cond-> [] saved (conj "saved") true (conj age)))
                    :search (when-not image? preview)
                    :row-id (str "clipboard " id)
                    :preview {:id (str "clipboard " id)
                              :load #(clipboard-preview entry image?
                                                        (str/join "  ·  " [kind (byte-size bytes) (str age " ago")]))}
                    :hint "paste"
                    :volatile true
                    :action paste
                    :keys {"s" save "d" delete}
                    :alt-hint "options"
                    :alt-action {:prompt (if image? "Image" (fit (or (first lines) "Text") 40))
                                 :items [{:icon "\uF0EA" :label "Paste" :action paste}
                                         {:icon "\uF0C5" :label "Copy" :subtitle "without pasting"
                                          :action #(clipboard/copy! entry)}
                                         {:icon "\uF02E" :label (if saved "Unsave" "Save")
                                          :subtitle (if saved "Ctrl+S · let it age out" "Ctrl+S · keep at the top")
                                          :action (:action save) :refresh :parent}
                                         {:icon "\uF1F8" :label "Delete" :subtitle "Ctrl+D"
                                          :action (:action delete) :refresh :parent}
                                         {:icon "\uF019" :label "Export to Downloads"
                                          :action #(export-clipboard-entry! entry image?)}
                                         {:icon "\uF12D" :label "Clear clipboard history"
                                          :subtitle "removes every unsaved entry"
                                          :action clipboard/clear! :refresh :parent}]}})]
        (if (seq rows) (vec rows) [{:icon "\uF0EA" :label "Clipboard history is empty"}]))
      (catch Exception error
        [{:icon "\uF071" :label "Could not read the clipboard history" :subtitle (ex-message error)}]))))

(defn get-mail-auth-items
  "One row per mail account, the ones that need authorizing first.

  email.clj records why each account failed; a credential failure is the only
  thing here a keypress can fix, so those rows carry the reason as their
  subtitle and come first."
  []
  (let [st (try (json/parse-string (slurp mail-sync-status-path) true) (catch Exception _ nil))
        accounts (:accounts st)
        rows (for [{:keys [account ok message needsAuth]} accounts]
               {:icon (cond needsAuth "󰌋" ok "󰄬" :else "󰀦")
                :label (str "Authorize " account)
                :subtitle (when-not ok (fit (str message) 48))
                :rank (cond needsAuth 0 (not ok) 1 :else 2)
                :action (kitty (str sources/dotfiles "/bin/email.clj authorize " account) :wait? true)})]
    (if (seq rows)
      (->> rows (sort-by :rank) (mapv #(dissoc % :rank)))
      [{:icon "󰎮" :label "No mail accounts in isyncrc"}])))

(def claude-account-path (str sources/dotfiles "/bin/menu/claude_account.clj"))

(defn get-claude-items
  "The saved Claude Code accounts, switched with the widget's own script."
  []
  (let [active (try (-> (slurp (str (fs/home) "/.claude.json")) (json/parse-string true)
                        :oauthAccount :emailAddress)
                    (catch Exception _ nil))
        {:keys [exit out]} (p/sh {:continue true} claude-account-path "profiles")
        profiles (when (zero? exit) (try (:profiles (json/parse-string out true)) (catch Exception _ nil)))]
    (conj (mapv (fn [{:keys [name email]}]
                  (if (= email active)
                    {:state true :icon "󰚩" :label email :subtitle "active"}
                    {:icon "󰀄" :label (str "Switch to " email)
                     :action #(p/sh {:continue true} claude-account-path "switch" name)}))
                profiles)
          {:icon "󰐕" :label "Add account" :action (kitty (str claude-account-path " login") :wait? true)})))

(defn- pactl-json
  "`pactl -f json list <what>` as data, or [] when PipeWire is not answering."
  [what]
  (try (json/parse-string (:out (p/sh {:continue true} "pactl" "-f" "json" "list" what)) true)
       (catch Exception _ [])))

(defn- audio-label
  "A device's name as a person would say it. A built-in card's description
  is the chip (`Ryzen HD Audio Controller Analog Stereo`), so its active
  port (`Speakers`, `Headphones`, `Internal Microphone`) is used instead."
  [{:keys [description active_port ports properties]}]
  (or (when (= "pci" (get properties :device.bus))
        (some #(when (= active_port (:name %)) (:description %)) ports))
      (str/replace (str description) #" (Analog|Digital) Stereo$" "")))

(defn get-sound-items
  "Output, then input. Each starts with a status row (`:state`, drawn as a
  heading with a badge) whose Enter mutes or unmutes it: the device in use and whether it is muted. Every row below it is
  an action and starts with its verb: Mute/Unmute (through noctalia, so its
  OSD shows) and `Switch output to ...`, after which PipeWire moves the
  playing streams over. Monitors are not inputs."
  []
  (let [sinks (future (pactl-json "sinks"))
        sources (future (remove #(seq (:monitor_source %)) (pactl-json "sources")))
        dsink (future (str/trim (:out (p/sh {:continue true} "pactl" "get-default-sink"))))
        dsource (future (str/trim (:out (p/sh {:continue true} "pactl" "get-default-source"))))
        section (fn [devs default kind noun mute-cmd on-icon off-icon]
                  (let [cur (first (filter #(= default (:name %)) devs))
                        muted (:mute cur)]
                    (concat
                     [{:state true :icon (if muted off-icon on-icon)
                       :label (str (str/capitalize kind) ": " (if cur (audio-label cur) "none"))
                       :subtitle (when cur (if muted "muted" "on")) :warn muted
                       ;; DWIM: Enter on the state flips it. The hint is also
                       ;; what makes `mute` / `unmute` find this row.
                       :hint (when cur (str (if muted "unmute " "mute ") noun))
                       :action (when cur mute-cmd)}]
                     (for [{:keys [name] :as d} devs :when (not= name default)]
                       {:icon "󰁔" :label (str "Switch " kind " to " (audio-label d))
                        :hint "switch"
                        :action (str "pactl set-default-" (if (= kind "output") "sink" "source") " " name)}))))]
    (vec (concat
          (section @sinks @dsink "output" "sound" "noctalia msg volume-mute" "󰕾" "󰝟")
          (section @sources @dsource "input" "microphone" "noctalia msg mic-mute" "󰍬" "󰍭")
          [{:icon "󰒓" :label "Open sound settings" :action "pavucontrol"}]))))

(defn get-power-profile-items []
  (let [current (try (str/trim (:out (p/sh {:continue true} "powerprofilesctl" "get")))
                     (catch Exception _ nil))]
    (mapv (fn [[profile icon label]]
            {:icon icon :label label
             :subtitle (when (= profile current) "current")
             :action (str "powerprofilesctl set " profile)})
          [["performance" "󰓅" "Performance"]
           ["balanced" "󰾅" "Balanced"]
           ["power-saver" "󰾆" "Power saver"]])))

(def ^:private cast-path (str sources/dotfiles "/bin/menu/cast.clj"))
(def ^:private sync-status-path (str sources/dotfiles "/bin/menu/sync_status.clj"))
(def ^:private mirror-state-file
  "cast.clj's record of a running mirror or extra screen."
  (str (fs/path (or (System/getenv "XDG_RUNTIME_DIR") "/tmp") "noctalia-cast-mirror" "mirror.json")))
(def ^:private cast-state-file
  "cast.clj's marker for media it cast; only then is the receiver asked."
  (str (fs/path (or (System/getenv "XDG_RUNTIME_DIR") "/tmp") "noctalia-cast-mirror" "cast.json")))

(defn live-items
  "What is running right now, each row the way to stop it: a recording, a
  cast screen, a backup, Do not disturb, Caffeine, the Mullvad VPN, and
  launcher timers. The root view
  shows them above Apps, set apart. Read in parallel, and never cached: a
  stale `Stop recording` would be worse than none."
  []
  (let [recording (future (try (-> (p/sh {:continue true} wf-recorder-path "status" "--edn")
                                   :out str/trim edn/read-string)
                               (catch Exception _ nil)))
        mirror (future (try (json/parse-string (slurp mirror-state-file) true)
                            (catch Exception _ nil)))
        casting (future (when (fs/exists? cast-state-file)
                          (try (let [r (json/parse-string
                                        (:out (p/sh {:continue true} cast-path "casting")) true)]
                                 (when (:casting r) r))
                               (catch Exception _ nil))))
        backup (future (try (= "active" (str/trim (:out (p/sh {:continue true} "systemctl" "--user"
                                                                "is-active" "backup.service"))))
                            (catch Exception _ false)))
        dnd (future (try (= :on (get-dnd-mode)) (catch Exception _ false)))
        caffeine (future (caffeine-on?))
        vpn (future (mullvad-connected?))
        timers (future (try (filterv :action (active-timer-items)) (catch Exception _ [])))]
    (->> (concat
          [(overlay-item)]
          [(when (:recording @recording)
             {:icon "󰑊" :label "Stop recording" :subtitle "recording"
              :action (str wf-recorder-path " stop")})
           (when-let [{:keys [kind output]} @mirror]
             (let [extend? (= "extend" kind)]
               {:icon "󰍺" :label (if extend? "Stop extra screen" "Stop mirroring")
                :subtitle (str "casting " output)
                :action (str cast-path (if extend? " extend-stop" " mirror-stop"))}))
           (when-let [{:keys [device title]} (when-not @mirror @casting)]
             {:icon "󰓛" :label "Stop casting"
              :subtitle (str/join " → " (remove str/blank? [title device]))
              :action (str cast-path " stop")})
           (when @backup
             {:icon "󰁯" :label "Stop backup" :subtitle "backup running"
              :action (str sync-status-path " backup-stop")})
           (when @dnd
             {:icon "󰂛" :label "Turn off Do not disturb" :subtitle "notifications hidden"
              :action cycle-dnd})
           (when @caffeine
             {:icon "󰅶" :label "Turn off Caffeine" :subtitle "keeping awake"
              :action "noctalia msg caffeine-disable"})
           (when @vpn
             {:icon "󰦝" :label "Disconnect Mullvad" :subtitle "VPN connected"
              :action "mullvad disconnect"})]
          (map (fn [{:keys [file language phase progress]}]
                 {:icon "󰔟" :label (str "Transcribing " (fs/file-name file)
                                             (when (number? progress) (str " · " progress "%")))
                  :subtitle (str "local · " language (when phase (str " · " phase))) :hint "open recording folder"
                  :action (fn [] (p/sh "xdg-open" (str (fs/parent file))))})
               (:transcriptions @recording))
          @timers)
         (remove nil?)
         (mapv #(assoc % :live true)))))

;; Unified menu structure

;; Helper functions

(defn inline-menu?
  "An ephemeral submenu, built for one target and never expanded or cached."
  [action]
  (and (map? action) (string? (:prompt action)) (vector? (:items action))))

(def ^:private providers
  {:windows/list desktop-repl/window-items
   :mail/codes get-mail-code-items :mail/accounts get-mail-auth-items
   :connect/status get-connect-items :wifi/networks get-wifi-items
   :bluetooth/devices get-bluetooth-items :appearance/themes get-themes-menu
   :focus/status get-focus-items :timers/active active-timer-items
   :sound/devices get-sound-items :power/profiles get-power-profile-items
   :claude/accounts get-claude-items :keys/bindings get-keybindings-menu
   :tailscale/networks get-tailscale-menu-items :record/status get-record-menu-items
   :clipboard/history get-clipboard-items})

(def ^:private built-in-actions
  {:workspace/rename rename-workspace :packages/aur install-aur-action
   :wifi/scan scan-wifi! :bluetooth/scan scan-bluetooth!
   :timers/remind remind-me-action :timers/sleep sleep-in-action
   :timers/countdown countdown-action :timers/stopwatch stopwatch-action
   :notes/scratchpad scratchpad-note!
   :config/edit #(p/sh "emacsclient" "-c" (str (config/path)))})

(defn- command-args [argv]
  (mapv #(if (str/starts-with? % "~/")
           (str (or (p/getenv "HOME") (fs/home)) (subs % 1))
           %) argv))

(defn- run-terminal! [{:keys [exec shell title class wait?]}]
  (let [argv (if shell ["sh" "-c" shell] (command-args exec))
        argv (if wait? (into ["sh" "-c" "\"$@\"; printf 'Press enter to close...'; read -r _" "cloncher"]
                             argv)
                 argv)]
    (apply p/sh (concat ["niri" "msg" "action" "spawn" "--" "kitty" "-o" "shell=zsh"]
                        (when class ["--class" class])
                        (when title ["--title" title])
                        argv))))

(defn- argv? [value]
  (and (vector? value) (seq value) (every? string? value) (not (str/blank? (first value)))))

(defn- compile-action [menus action]
  (when (and (map? action) (= 1 (count action)))
    (let [[type value] (first action)]
      (case type
        :menu (when (contains? menus value) value)
        :call (get built-in-actions value)
        :exec (when (argv? value) #(apply p/sh (command-args value)))
        :shell (when (and (string? value) (not (str/blank? value))) #(p/sh "sh" "-c" value))
        :terminal (when (and (map? value)
                             (not= (contains? value :exec) (contains? value :shell))
                             (if (contains? value :exec)
                               (argv? (:exec value))
                               (and (string? (:shell value)) (not (str/blank? (:shell value)))))
                             (every? string? (vals (select-keys value [:class :title])))
                             (or (not (contains? value :wait?)) (boolean? (:wait? value))))
                    #(run-terminal! value))
        nil))))

(defn- compile-item [menus menu item]
  (let [invalid #(throw (ex-info (str "Invalid " % " in menu " menu) {:menu menu :item item}))]
    (cond
      (not (map? item)) (invalid "item")
      (contains? item :provider)
      (if (and (= 1 (count item)) (contains? providers (:provider item)))
        item
        (invalid "provider"))
      (or (not (string? (:label item))) (str/blank? (:label item))) (invalid "label")
      (not (every? string? (vals (select-keys item [:icon :subtitle :hint :alt-hint :search :shortcut :progress]))))
      (invalid "text field")
      (not (every? integer? (vals (select-keys item [:section :order])))) (invalid "row order")
      (and (contains? item :id) (not (or (keyword? (:id item))
                                        (and (string? (:id item)) (not (str/blank? (:id item)))))))
      (invalid "item ID")
      (and (contains? item :refresh) (not (contains? #{:current :parent true false} (:refresh item))))
      (invalid "refresh destination")
      :else
      (reduce (fn [row key]
                (if (contains? item key)
                  (assoc row key (or (compile-action menus (get item key)) (invalid (name key))))
                  row))
              (cond-> (dissoc item :id)
                (:id item) (assoc :row-id (str (:id item))))
              [:action :alt-action]))))

(defn- compile-configuration [text]
  (let [{:keys [menus tailnets scratchpad clipboard-history] :or {tailnets [] clipboard-history false}}
        (config/read-config text)]
    (when-not (and (map? menus) (contains? menus :main))
      (throw (ex-info "Expected a :menus map containing :main" {})))
    (when-not (and (vector? tailnets)
                   (every? (fn [entry]
                             (and (map? entry)
                                  (every? #(and (string? %) (not (str/blank? %)))
                                          (map entry [:label :tailnet :account]))))
                           tailnets))
      (throw (ex-info "Expected :tailnets to contain :label, :tailnet and :account strings" {})))
    (when-not (or (nil? scratchpad) (and (string? scratchpad) (not (str/blank? scratchpad))))
      (throw (ex-info "Expected :scratchpad to be the path of an Org file" {})))
    (when-not (boolean? clipboard-history)
      (throw (ex-info "Expected :clipboard-history to be true or false" {})))
    (let [menus (into {} (map (fn [[key menu]]
                                (when-not (and (keyword? key) (nil? (namespace key))
                                               (map? menu) (string? (:prompt menu)) (vector? (:items menu)))
                                  (throw (ex-info (str "Invalid menu " key) {:menu key})))
                                [key (update menu :items #(mapv (partial compile-item menus key) %))])) menus)
          root-items (search/index-rows (remove :provider (get-in menus [:main :items])))]
      {:menus menus :tailnets tailnets :scratchpad scratchpad :clipboard-history clipboard-history
       :root-items root-items :revision (str (hash text))})))

(def ^:private defaults (delay (compile-configuration config/default-text)))
(defonce ^:private configuration-files (atom {}))
(def ^:dynamic *configuration* nil)

(defn configuration []
  (or *configuration* @defaults))

(defn load-configuration!
  "Return one validated snapshot. File I/O takes place at session startup or on explicit reload."
  []
  (let [path (config/path)]
    (locking configuration-files
      (let [previous (get @configuration-files path)
            snapshot (or (:snapshot previous) @defaults)]
        (try
          (config/setup! path)
          (let [stamp [(str (fs/last-modified-time path)) (fs/size path)]]
            (if (= stamp (:stamp previous))
              snapshot
              (let [updated (try
                              (let [text (slurp (str path))]
                                (if (= text config/default-text) @defaults (compile-configuration text)))
                              (catch Exception error
                                (binding [*out* *err*]
                                  (println "Cloncher config" (str path) (ex-message error)))
                                snapshot))]
                (swap! configuration-files assoc path {:stamp stamp :snapshot updated})
                updated)))
          (catch Exception error
            (binding [*out* *err*]
              (println "Cloncher config" (str path) (ex-message error)))
            snapshot))))))

(defn definition [key]
  (if (inline-menu? key) key (get-in (configuration) [:menus key])))

(defn static-items [key]
  (into [] (remove #(or (:provider %) (:root-only? %))) (:items (definition key))))

(defn get-menu-items
  "Expand configured providers in place among static items."
  ([menu-key] (get-menu-items menu-key false))
  ([menu-key root?]
   (into [] (comp (remove #(and (not root?) (:root-only? %)))
                 (mapcat (fn [item]
                           (if-let [provider (get providers (:provider item))]
                             (try (provider)
                                  (catch Exception error
                                    (when debug? (println "Provider" (:provider item) (ex-message error)))
                                    []))
                             [item]))))
         (:items (definition menu-key)))))

(defn safe-menu-items
  "Items of a menu, or [] when building them throws. A submenu whose backing
  command or config file is missing must not take its parent menu down."
  ([menu-key] (safe-menu-items menu-key false))
  ([menu-key root?]
   (try
     (get-menu-items menu-key root?)
     (catch Exception e
       (when debug? (println "Menu" menu-key "failed:" (ex-message e)))
       []))))

(defn format-item
  "Format a menu item as one picker row: its glyph, its label, then a TAB
  and the menu path it came from. The launcher draws the text after the TAB
  as a dimmed column beside the label, so a flattened row reads `Log: Site`
  with `Projects › RRES` next to it, and matches on both. The leading glyph
  goes in its icon slot. The formatted row is also the row's id in `serve`."
  [{:keys [icon label path row-id]}]
  (let [head (or row-id (if (str/blank? icon) label (str icon "  " label)))]
    (if (str/blank? path)
      head
      (str head "\t" path))))

(defn find-menu-item
  "Find the menu item matching the selection string.
  Matches the formatted row first - glyph, label and path, because two
  menus can hold the same label - then a bare label."
  [items selection]
  (or
   ;; What the picker shows and returns
   (first (filter #(= selection (format-item %)) items))
   ;; Fall back to a bare label, for a picker that drops the subtitle
   (first (filter #(= selection (:label %)) items))))

(defn menu-item?
  "Check if action is a menu keyword."
  [action]
  (and (keyword? action) (some? (definition action))))

(def ^:private path-sep
  "Separator between the ancestor labels of a flattened submenu row."
  " › ")

(defn menu-rows
  "Every row under a menu, flattened: its own items, followed by the items of
  every submenu beneath it, each nested item carrying the path that leads to
  it on `:path`. `format-item` puts that path in the row's subtitle, so one
  query fuzzy-matches the whole tree - typing 'log site' finds `Log: Site`
  under `Projects › RRES`.

  A menu in `serve`
  carries them as `:deep` rows (see `view-items`), searchable but hidden until
  something is typed, so the main menu is not padded with everything below it.

  Nested rows that do nothing (headers, status lines) are dropped, submenus
  are expanded in parallel because most items-fn shell out, and `seen` stops
  a menu that points back at an ancestor from looping."
  ([menu-key] (menu-rows menu-key false))
  ([menu-key root?] (menu-rows menu-key "" #{menu-key} root?))
  ([menu-key path seen root?]
   (let [items (safe-menu-items menu-key root?)
         own (mapv (fn [item]
                     (cond-> item
                       (seq path) (assoc :path path)))
                   items)
         expand? (fn [{:keys [action]}]
                   (and (menu-item? action)
                        (not (contains? seen action))
                        (not= false (:searchable? (definition action)))))
         nested (->> items
                     (filter expand?)
                     (mapv (fn [{:keys [action label]}]
                             (future (menu-rows action
                                                (if (seq path)
                                                  (str path path-sep label)
                                                  label)
                                                (conj seen action) false))))
                     (mapcat deref)
                     (filter :action))]
     (into own nested))))


(defn execute-action
  "Execute an action based on its type."
  [action]
  (cond
    (or (menu-item? action) (inline-menu? action))
    (loop []
      (let [items (safe-menu-items action)
            selection (show-menu (:prompt (definition action)) (map format-item items))]
        (when-some [item (find-menu-item items selection)]
          (execute-action (:action item))
          (when (:refresh item) (recur)))))

    (string? action) (p/sh "sh" "-c" action)
    (vector? action) (run! execute-action action)
    (fn? action) (action)
    :else (println "Unknown action:" action)))
;; ── serve: providers and the loop ───────────────────────────────────────────

(def calc-file
  "The calculator's tape: `[{expr result at}]`, newest first. `ans` / `_` is
  the first result, `_2`, `_3`… the ones before it."
  (str (fs/path (fs/xdg-cache-home) "launcher" "calc.json")))

(def ^:private calc-keep 200)

(defn calc-tape []
  (try (vec (json/parse-string (slurp calc-file) true))
       (catch Exception _ [])))

(defn save-calc!
  "Put `{expr result}` on top of the tape. The same expression again moves
  to the top instead of being listed twice."
  [{:keys [expr result]}]
  (try
    (let [tape (->> (calc-tape)
                    (remove #(= expr (:expr %)))
                    (cons {:expr expr :result result :at (quot (System/currentTimeMillis) 1000)})
                    (take calc-keep)
                    vec)
          tmp (str calc-file ".tmp")]
      (fs/create-dirs (fs/parent calc-file))
      (spit tmp (json/generate-string tape))
      (fs/move tmp calc-file {:replace-existing true :atomic-move true}))
    (catch Exception e
      (when debug? (println "calc save failed:" (ex-message e))))))

(def ^:private ans-re
  "`ans`, `_` or `_N` standing alone: an earlier result."
  #"(?<![\w.])(?:ans|_(\d*))(?![\w.])")

(defn- expand-ans
  "`expr` with every `ans` / `_N` replaced by that result from the tape. A
  plain number goes in as it is, anything else (units, sums) in brackets."
  [expr tape]
  (str/replace expr ans-re
               (fn [[whole n]]
                 (if-let [v (:result (get tape (dec (if (str/blank? n) 1 (parse-long n)))))]
                   (if (re-matches #"[−-]?[\d.,]+" v) v (str "(" v ")"))
                   whole))))

(def ^:private calc-fn-re
  "A qalc function call opening the query: `sqrt(2)`, `log(100)`."
  #"(?i)^(?:sqrt|cbrt|root|abs|round|floor|ceil|trunc|exp|ln|log\d*|a?sinh?|a?cosh?|a?tanh?|factorial|gcd|lcm|min|max|mod|rem)\(")

(def ^:private calc-const-re
  "A constant opening an operation: `pi*2`, `e^3`."
  #"(?i)^(?:pi|π|e|tau|τ|phi|φ)\s*[-+−*/^×÷(]")

(defn- settle
  "`expr` as far as it is typed: a trailing operator is dropped, so `12*3+`
  keeps answering 36 while the next number is on its way."
  [expr]
  (str/trim (str/replace expr #"[\s+\-−*/^×÷]+$" "")))

(defn calc-expression
  "The expression a query asks qalc for, or nil. `=` forces it. Otherwise it
  is recognised by its shape, not by asking qalc - which reads any text as
  something (`2fa` is `2 fa`, `3 days in hours` is inches) - so ordinary
  searches never pay for it and never show nonsense:

  - arithmetic: digits and operators, as soon as there is an operation
    (`12*3`, `(3+4`, `5!`, `2^10`); a plain number is not one
  - a number in hex, binary or octal: `0x1f`, `0b101`
  - a function or a constant opening it: `sqrt(2)`, `pi*2`
  - an earlier result in an operation: `ans * 2`, `_2 + 1`
  - a conversion: a number, then `to` / `in` / `as` a unit (`10 km to mi`,
    `100 usd in eur`, `5 ft 10 in to cm`); `in` is rewritten to qalc's `to`
  - a percentage of something: `20% of 150`

  A trailing operator is dropped (see `settle`). After `=`, a leading
  operator carries on from the last result: `=*2` is `ans*2`."
  [q]
  (let [q (str/trim q)
        e (settle q)]
    (cond
      (str/starts-with? q "=")
      (when-let [e (not-empty (settle (subs q 1)))]
        (if (re-find #"^[+*/^×÷%]" e) (str "ans" e) e))

      (re-matches #"[\d\s.,+\-−*/^%!()×÷]+" e)
      (when (and (re-find #"\d" e) (not (re-matches #"[−-]?[\d.,]+" e))) e)

      (re-matches #"(?i)0(?:x[\da-f]+|b[01]+|o[0-7]+)" e) e
      (or (re-find calc-fn-re e) (re-find calc-const-re e)) e
      (and (re-find ans-re e) (re-find #"[-+−*/^%×÷]" e)) e

      (re-find #"(?i)^[$€£]?[\d.,]+\s*[^\d\s].*\s(?:to|in|as)\s+[^\d\s]\S*$" e)
      (if (re-find #"(?i)\s(?:to|as)\s" e)
        e
        (str/replace e #"(?i)^(.*)\sin\s" "$1 to "))

      (re-find #"(?i)^[\d.,]+\s*%\s+of\s+[\d.,]" e)
      (str/replace-first e #"(?i)%\s+of\s+" "% * "))))

(defn- copy! [text]
  ;; wl-copy forks a child that owns the selection and keeps whatever pipes it
  ;; was handed, so its output goes nowhere (see the mail triage notes).
  @(p/process {:in text :out :discard :err :discard} "wl-copy"))

(defn- tape-rows
  "The tape as rows, newest first: the result, then its calculation (and its
  age once older than a day). Row N is `_N` (the first is `ans`). Enter puts the calculation back in the query to go on with.
  `needle` narrows them to expressions or results containing it."
  [tape needle]
  (->> tape
       (map (fn [{:keys [expr result at]}]
                      {:icon "󰋚" :label result
                       :path (let [s (- (quot (System/currentTimeMillis) 1000) (or at 0))]
                               (if (< s 86400) expr (str expr " · " (quot s 86400) "d")))
                       :fill (str "=" expr) :hint "edit"
                       :hay (str/lower-case (str expr " " result))}))
       (filter #(or (str/blank? needle) (str/includes? (:hay %) (str/lower-case needle))))
       (take 50)
       (mapv #(dissoc % :hay))))

(defn calc-provider
  "Calculator rows for a query: qalc's answer (Enter copies it), and after
  `=` the tape below it."
  [q]
  (let [tape (calc-tape)
        expr (some-> (calc-expression q) (expand-ans tape))
        result (when expr
                 (let [{:keys [exit out]} (p/sh {:continue true} "timeout" "2" "qalc" "-t" expr)
                       r (some-> out str/split-lines last str/trim (str/replace #"^≈\s*" ""))]
                   (when (and (zero? exit) (not (str/blank? r)) (not= r expr)) r)))
        forced? (str/starts-with? (str/trim q) "=")]
    (cond-> []
      result (conj {:icon "󰃬" :label result :path expr :hint "copy"
                    :calc {:expr expr :result result} :action #(copy! result)})
      forced? (into (tape-rows tape (when-not result (str/trim (subs (str/trim q) 1))))))))

(def source-file *file*)

(defn resolve-row
  "Resolve a cached row ID by following the item’s current submenu path."
  ([key id] (resolve-row key id false))
  ([key id root?]
   (let [path (second (str/split id #"\t" 2))
         parents (when (seq path) (str/split path #" › "))
         target (reduce (fn [target parent-label]
                          (or (some (fn [{:keys [label action]}]
                                      (when (and (= label parent-label) (menu-item? action)) action))
                                    (safe-menu-items target (and root? (= target key))))
                              (reduced nil)))
                        key parents)]
     (when target
       (some #(when (= id (format-item (cond-> % (seq path) (assoc :path path)))) %)
             (safe-menu-items target (and root? (empty? parents))))))))
