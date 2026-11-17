(ns cloncher.sources
  "Desktop entries, files, projects and shell history."
  (:refer-clojure :exclude [run!])
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [cloncher.json :as json]
            [cloncher.search :as search]))

(def home (str (fs/home)))
(def dotfiles (or (System/getenv "CLONCHER_DOTFILES") (str home "/dotfiles")))

(defn- read-if-present [path]
  (when (fs/exists? path) (slurp (str path))))

(defn- run! [& argv]
  (apply p/shell {:out :discard :err :inherit} argv))

(defn- desktop-entry [path]
  (let [lines (->> (str/split-lines (slurp (str path)))
                   (drop-while #(not= "[Desktop Entry]" (str/trim %)))
                   rest
                   (take-while #(not (str/starts-with? (str/trim %) "["))))]
    (into {} (keep (fn [line]
                     (let [[k v] (str/split line #"=" 2)]
                       (when v [k v])))) lines)))

(defn- visible-app [path]
  (let [entry (desktop-entry path)
        desktops (set (str/split (or (System/getenv "XDG_CURRENT_DESKTOP") "") #":"))
        listed? (fn [s] (some desktops (str/split (or s "") #";")))
        desktop (str (fs/file-name path))]
    (when (and (= "Application" (entry "Type")) (seq (entry "Name"))
               (not= "true" (entry "Hidden")) (not= "true" (entry "NoDisplay"))
               (or (not (seq (entry "OnlyShowIn"))) (listed? (entry "OnlyShowIn")))
               (not (listed? (entry "NotShowIn")))
               (or (not (seq (entry "TryExec"))) (fs/which (entry "TryExec"))))
      {:label (entry "Name") :icon "󰀻" :subtitle (or (entry "GenericName") (entry "Comment"))
       :row-id (str "app:" desktop)
       :action #(run! "gio" "launch" (str path))})))

(defn apps []
  (let [dirs (cons (str (fs/xdg-data-home))
                   (str/split (or (System/getenv "XDG_DATA_DIRS") "/usr/local/share:/usr/share") #":"))]
    (->> dirs
         (mapcat (fn [dir]
                   (let [base (fs/path dir "applications")]
                     (when (fs/directory? base)
                       (map (fn [path] [(str/replace (str (fs/relativize base path)) "/" "-") path])
                            (fs/glob base "**.desktop"))))))
         (reduce (fn [by-id [id path]]
                   (if (contains? by-id id) by-id (assoc by-id id path))) {})
         vals
         (keep (fn [path] (try (visible-app path) (catch Exception _ nil))))
         (sort-by (comp str/lower-case :label))
         vec)))

(def ^:private file-exclusions
  [".git" ".cache" "node_modules" "target" ".direnv" ".venv" ".local/share/Trash"
   ".password-store" ".gnupg" ".ssh" ".local/share/containers" ".local/share/opencode"
   ".local/share/pnpm" ".local/share/Steam" ".local/share/uv" ".local/share/postgres"
   ".local/share/umu" ".bun/install/cache" ".cargo/registry" ".cargo/git"
   "go/pkg/mod" ".pub-cache" ".zoom" ".config/Notion/Partitions" "kernel-venv"
   "__pycache__" "Cache" "Code Cache" "GPUCache" ".npm/_cacache" ".gradle/caches"
   ".local/share/lutris"])

(defn- escaped-regex [s]
  (str/escape s (zipmap "\\.*+?()[]{}^$|" (map #(str "\\" %) "\\.*+?()[]{}^$|"))))

(defn files [root query]
  (let [longest (last (sort-by count (search/tokens query)))
        pattern (if (seq longest) (str/join ".*" (map (comp escaped-regex str) longest)) ".")
        argv (into ["timeout" "4" "fd" "--type" "f" "--hidden" "--ignore-case"
                    "--full-path" "--print0" "--max-results" "1600" "--base-directory" root]
                   (concat (mapcat #(vector "--exclude" %) file-exclusions) ["--" pattern]))
        {:keys [exit out err]} (apply p/sh {:continue true} argv)]
    (when-not (contains? #{0 124} exit)
      (throw (ex-info (str "File search failed: " (str/trim err)) {})))
    (into []
          (comp (remove str/blank?)
                (map (fn [relative]
                       (let [path (str (fs/path root relative))]
                         {:label (str (fs/file-name path)) :icon "󰈔"
                          :subtitle (str (fs/parent relative))
                          :row-id path :hint "actions" :next {:kind :file :path path}}))))
          (str/split out #"\u0000"))))

(defn- elisp [form]
  (run! "emacsclient" "--no-wait" "--reuse-frame" "--alternate-editor=false" "--eval" form))

(defn- open-file! [path]
  (let [{:keys [out]} (p/sh {:continue true} "file" "--brief" "--mime-type" "--" path)]
    (if (re-find #"^(?:text/|application/(?:json|xml|x-empty)|inode/x-empty)" out)
      (run! "emacsclient" "--no-wait" "--reuse-frame" "--alternate-editor=false" "--" path)
      (run! "xdg-open" path))))

(defn file-actions [path]
  (cond-> [{:label "Open" :icon "󰈔" :shortcut "o" :action #(open-file! path)}
           {:label "Reveal in Dired" :icon "󰉋" :shortcut "r"
            :action #(elisp (str "(progn (dired " (pr-str (str (fs/parent path)))
                                 ") (dired-goto-file " (pr-str path) ") (recenter))"))}
           {:label "Copy path" :icon "󰆏" :shortcut "c"
            :action #(deref (p/process {:in path :out :discard :err :discard} "wl-copy"))}]
    (re-find #"(?i)\.(m4a|mp3|mp4|mkv|mov|webm|wav|flac|ogg|opus|aac|avi)$" path)
    (conj {:label "Transcribe" :icon "󰍬"
           :action #(run! (str dotfiles "/bin/menu/wf_recorder.clj") "transcribe" "--file" path)})))

(defn- project-file []
  (or (System/getenv "LAUNCHER_PROJECTS_FILE") (str home "/.emacs.d/projects")))

(defn- project-state []
  (str (fs/path (or (System/getenv "XDG_STATE_HOME") (str home "/.local/state")) "launcher" "projects.json")))

(defn- project-root [path]
  (str (fs/normalize (fs/expand-home path))))

(defn- project-records []
  (->> (some-> (read-if-present (project-state)) (json/parse-string true) :projects)
       (filter :root)
       (into {} (map (juxt (comp project-root :root) identity)))))

(defn- remember-project! [operation path]
  (let [form (case operation
               :add (str "(project-remember-project (cons 'transient (file-name-as-directory " (pr-str path) ")))")
               :forget (str "(dolist (dir (project-known-project-roots))"
                            " (when (equal (directory-file-name (expand-file-name dir)) " (pr-str path) ")"
                            " (project-forget-project dir)))"))]
    (run! "timeout" "6" "emacsclient" "--alternate-editor=false" "--eval"
          (str "(progn (require 'project) " form ")"))))

(defn- pin-project! [path]
  (let [registry (project-state)
        parent (fs/parent registry)
        configured (project-records)
        row (get configured path {:root path})
        updated (assoc configured path (update row :pinned not))]
    (fs/create-dirs parent)
    (let [temporary (fs/create-temp-file {:dir parent :prefix "cloncher-projects-"})]
      (try
        (fs/set-posix-file-permissions temporary "rw-------")
        (spit (str temporary) (json/generate-string {:projects (vec (vals updated))}))
        (fs/move temporary registry {:replace-existing true :atomic-move true})
        (finally (when (fs/exists? temporary) (fs/delete temporary)))))))

(defn projects []
  (let [records (project-records)
        roots (into #{} (comp (map first) (filter string?) (map project-root))
                    (some-> (read-if-present (project-file)) edn/read-string))
        list (->> roots
                  (filter fs/directory?)
                  (remove #(get-in records [% :forgotten]))
                  (sort-by (fn [path] [(if (get-in records [path :pinned]) 0 1)
                                       (- (get-in records [path :used] 0))
                                       (str/lower-case (str (fs/file-name path)))]))
                  (mapv (fn [path] {:icon "󰉋" :label (str (when (get-in records [path :pinned]) "★ ")
                                                          (fs/file-name path))
                                    :subtitle path :row-id path :hint "actions"
                                    :next {:kind :project :path path}})))]
    (conj list {:label "Add project…" :icon "󰐕" :action :cloncher/add-project})))

(defn add-project! [typed]
  (let [path (project-root typed)]
    (when-not (and (fs/absolute? path) (fs/directory? path))
      (throw (ex-info "Choose an existing absolute directory" {})))
    (remember-project! :add path)))

(defn- project-elisp [path body]
  (elisp (str "(progn (require 'project) (let ((default-directory (file-name-as-directory "
              (pr-str path) "))) " body "))")))

(defn- task-view [path label shortcut runner tasks]
  (when (seq tasks)
    {:label label :icon "󰐊" :subtitle (str (count tasks) " tasks") :shortcut shortcut
     :next {:kind :tasks :prompt label
            :rows (mapv (fn [[task doc]]
                          {:label task :subtitle doc :icon "󰐊"
                           :action #(apply run! "kitty" "--directory" path
                                          (concat runner [task]))})
                        tasks)}}))

(defn- project-tasks [path]
  (let [bb-file (fs/path path "bb.edn")
        bb (when (fs/exists? bb-file)
             (binding [*read-eval* false]
               (read-string {:read-cond :allow :features #{:bb :clj}} (slurp (str bb-file)))))
        just-file (some #(when (fs/exists? (fs/path path %)) %) ["justfile" "Justfile" ".justfile"])
        make-file (some #(when (fs/exists? (fs/path path %)) %) ["GNUmakefile" "makefile" "Makefile"])]
    (keep identity
          [(task-view path "Babashka tasks" "b" ["bb" "--config" (str bb-file) "run"]
                      (->> (:tasks bb)
                           (keep (fn [[task body]]
                                   (when (and (symbol? task) (not (str/starts-with? (name task) "-")))
                                     [(str task) (when (map? body) (:doc body))])))
                           (sort-by first)))
           (when just-file
             (let [{:keys [out]} (p/sh {:dir path :continue true} "timeout" "2" "just" "--summary")]
               (task-view path "Just recipes" "j" ["just"] (map #(vector % nil) (search/tokens out)))))
           (when make-file
             (task-view path "Make targets" "m" ["make"]
                        (->> (slurp (str (fs/path path make-file)))
                             (re-seq #"(?m)^([a-zA-Z0-9_][a-zA-Z0-9_.-]*):[^=]")
                             (map second) distinct (map #(vector % nil)))))])))

(defn project-actions [path]
  (let [pinned? (:pinned (get (project-records) path))]
    (into [{:label "Find file" :shortcut "f" :icon "󰉋" :next {:kind :files :root path}}
           {:label "Dired" :shortcut "d" :icon "󰉋" :action #(project-elisp path "(dired default-directory)")}
           {:label "Magit" :shortcut "v" :icon "󰉋" :action #(project-elisp path "(require 'magit) (magit-status default-directory)")}
           {:label "Create GitHub agent token" :shortcut "a" :icon "󰊤"
            :action #(run! (str dotfiles "/bin/menu/github_token.clj") path)}
           {:label "Terminal" :shortcut "t" :icon "󰉋" :action #(run! "kitty" "--directory" path)}
           {:label "Grep in Emacs" :shortcut "g" :icon "󰉋"
            :action #(project-elisp path "(call-interactively #'project-find-regexp)")}
           {:label (if pinned? "Unpin project" "Pin project") :shortcut "p" :icon "󰉋"
            :refresh true :action #(pin-project! path)}
           {:label "Forget project" :shortcut "x" :icon "󰉋"
            :action #(remember-project! :forget path)}]
          (try (project-tasks path) (catch Exception _ nil)))))

(def ^:private terminal-programs
  #{"btop" "htop" "top" "man" "less" "vim" "nvim" "nano" "tmux" "ssh" "sudo"
    "nix-shell" "yazi" "lazygit" "jolt" "bb" "clojure"})

(defn run-command! [command directory terminal?]
  (let [dir (if (and directory (fs/directory? directory)) directory home)
        program (first (str/split (str/trim command) #"\s+" 2))
        helper (str dotfiles "/bin/menu/run-command")
        args (cond
               (or terminal? (terminal-programs program))
               ["kitty" "--directory" dir "zsh" "-ic" (str command "; exec zsh")]

               (fs/executable? helper) [helper command]
               :else ["zsh" "-ic" command])]
    (apply run! "systemd-run" "--user" "--collect" "--quiet" "--expand-environment=no"
           (str "--working-directory=" dir) "--" args)))

(defn history []
  (let [out (try
              (let [result (p/sh {:continue true :extra-env {"ATUIN_SESSION" "launcher"}}
                                 "timeout" "2" "atuin" "search" "--limit" "1000"
                                 "--format" "{directory}\\t{command}" "--print0")]
                (when (zero? (:exit result)) (:out result)))
              (catch Exception _ nil))
        rows (if out
               (keep (fn [line]
                       (let [[dir command] (str/split line #"\t" 2)]
                         (when (seq command) [command dir])))
                     (str/split out #"\u0000"))
               (->> (or (read-if-present (str home "/.histfile"))
                        (read-if-present (str home "/.zsh_history")) "")
                    str/split-lines reverse (take 1000)
                    (map #(vector (str/replace % #"^: \d+:\d+;" "") nil))))]
    (->> rows (remove (comp str/blank? first)) (distinct) vec)))

(defn command-row [command directory terminal?]
  {:icon "󰆍" :label command :subtitle directory :hint (if terminal? "terminal" "run")
   :row-id (str "command:" directory ":" command)
   :action #(run-command! command directory terminal?)})
