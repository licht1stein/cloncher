(ns cloncher.commands
  "Command detection, suggestions, and launch history."
  (:refer-clojure :exclude [run!])
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.persistence :as disk]
            [cloncher.process :as p]
            [cloncher.search :as search]
            [cloncher.sources :as sources]))

(def ^:private terminal-programs
  #{"btop" "htop" "top" "man" "less" "more" "vim" "nvim" "vi" "nano" "tmux" "ssh"
    "sudo" "doas" "nix-shell" "yazi" "lazygit" "jolt" "bb" "clojure" "python"
    "python3" "node" "psql" "mysql" "sqlite3" "fzf" "emacs" "zsh" "bash"})

(def ^:private builtins
  #{"alias" "autoload" "bg" "bindkey" "builtin" "cd" "command" "dirs" "echo"
    "eval" "exec" "export" "false" "fg" "hash" "jobs" "kill" "local" "printf"
    "pwd" "read" "set" "setopt" "source" "test" "true" "type" "typeset" "umask"
    "unalias" "unset" "wait" "whence" "which"})

(defn- command-word [command]
  (first (remove #(re-matches #"[\w_]+=.*" %) (str/split (str/trim command) #"\s+"))))

(defn terminal? [command]
  (or (contains? terminal-programs (some-> (command-word command) fs/file-name str))
      (boolean (re-find #"(?:^|\s)(?:sudo|doas)(?:$|\s)" command))))

(defn- registry []
  (let [entries (disk/read-json (disk/cache "commands.json") [] false)]
    (if (map? entries)
      (mapv (fn [[cmd count]] {:cmd cmd :count count :at 0}) entries)
      (mapv (fn [x] {:cmd (get x "cmd") :count (get x "count" 0) :at (get x "at" 0)}) entries))))

(def ^:private history-lock (Object.))

(defn remember! [command]
  (locking history-lock
    (let [entries (registry)
          count (or (some #(when (= command (:cmd %)) (:count %)) entries) 0)
          entry {:cmd command :count (inc count) :at (quot (System/currentTimeMillis) 1000)}]
      (disk/write-json! (disk/cache "commands.json")
                        (into [entry] (take 999 (remove #(= command (:cmd %)) entries)))))))

(defn parse-zshrc [text]
  (reduce (fn [inventory line]
            (let [line (str/trim line)]
              (if-let [[_ name value] (re-matches #"alias\s+([\w.-]+)=(.*)" line)]
                (assoc inventory name (str "alias → " (str/replace value #"^(['\"])(.*)\1$" "$2")))
                (if-let [[_ name] (re-find #"^(?:function\s+)?([\w-]+)\s*\(\)\s*\{" line)]
                  (assoc inventory name "function")
                  inventory))))
          {} (str/split-lines text)))

(defn- inventory []
  (let [aliases (try (parse-zshrc (slurp (str (p/getenv "HOME") "/.zshrc"))) (catch Exception _ {}))
        bins (sequence
               (comp (filter fs/directory?)
                     (mapcat #(try (fs/list-dir %) (catch Exception _ [])))
                     (filter #(and (fs/regular-file? %) (fs/executable? %))))
               (str/split (or (p/getenv "PATH") "") #":"))]
    (merge (into {} (map #(vector (str (fs/file-name %)) (str %))) (reverse (vec bins)))
           (zipmap builtins (repeat "builtin")) aliases)))

(defn- details [catalog command]
  (let [word (command-word command)]
    (cond
      (str/blank? word) ""
      (str/includes? word "/")
      (let [path (fs/path (p/getenv "HOME") (fs/expand-home word))]
        (if (fs/executable? path) (str path) "Not found"))
      :else (get catalog word "Not found"))))

(defn run! [command directory terminal?]
  (try (remember! command) (catch Exception _ nil))
  (sources/run-command! command directory terminal?))

(defn row [command directory terminal? note]
  {:icon (if terminal? "" "")
   :label (str/replace command "\n" " ⏎ ") :subtitle note
   :command command :directory directory :terminal? terminal?
   :warn (= note "Not found") :hint (if terminal? "kitty" "background")
   :row-id (str "command:" terminal? ":" command)
   :action #(run! command directory terminal?)})

(defn- matching? [query command]
  (every? #(str/includes? (str/lower-case command) %) (search/tokens query)))

(defn- age-label [stamp]
  (when (pos? stamp)
    (let [minutes (max 0 (quot (- (quot (System/currentTimeMillis) 1000) stamp) 60))]
      (cond (< minutes 1) "just now"
            (< minutes 60) (str minutes "m ago")
            (< minutes 1440) (str (quot minutes 60) "h ago")
            :else (str (quot minutes 1440) "d ago")))))

(defn default-rows [command]
  (when-not (str/blank? command)
    (let [terminal (terminal? command)]
      [(row command nil terminal nil) (row command nil (not terminal) nil)])))

(defn suggestions [command]
  (let [catalog (if (str/blank? command) {} (inventory))
        previous (->> (registry)
                      (filter #(and (string? (:cmd %)) (not= command (:cmd %)) (matching? command (:cmd %))))
                      (sort-by :at >)
                      (map (fn [{:keys [cmd at]}] (row cmd nil false (age-label at)))))
        completions (when (re-matches #"[^\s]+" command)
                      (->> (keys catalog) sort
                           (filter #(and (not= command %) (str/starts-with? % command)))
                           (take 20)
                           (map #(row % nil (terminal? %) (get catalog %)))))
        typed (let [rows (default-rows command)]
                (when (seq rows)
                  (assoc rows 0 (row command nil (terminal? command) (details catalog command)))))]
    (->> (concat typed previous completions)
         (reduce (fn [rows x]
                   (if (some #(= (:row-id x) (:row-id %)) rows) rows (conj rows x))) [])
         (take 80)
         vec)))

(defn history [query terminal?]
  (into []
        (comp (filter #(matching? query (first %))) (take 80)
              (map (fn [[command directory]] (row command directory terminal? directory))))
        (sources/history)))
