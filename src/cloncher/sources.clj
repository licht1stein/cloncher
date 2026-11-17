(ns cloncher.sources
  "Desktop entries, files, projects and shell history."
  (:refer-clojure :exclude [run!])
  (:require [babashka.fs :as fs]
            [cloncher.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [cloncher.json :as json]
            [cloncher.icons :as icons]
            [cloncher.persistence :as disk]
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

(defn- visible-app [path resolve-icon]
  (let [entry (desktop-entry path)
        desktops (set (str/split (or (p/getenv "XDG_CURRENT_DESKTOP") "") #":"))
        listed? (fn [s] (some desktops (str/split (or s "") #";")))
        desktop (str (fs/file-name path))]
    (when (and (= "Application" (entry "Type")) (seq (entry "Name"))
               (not= "true" (entry "Hidden")) (not= "true" (entry "NoDisplay"))
               (or (not (seq (entry "OnlyShowIn"))) (listed? (entry "OnlyShowIn")))
               (not (listed? (entry "NotShowIn")))
               (or (not (seq (entry "TryExec"))) (fs/which (entry "TryExec"))))
      {:label (entry "Name") :icon "󰀻"
       :row-id (str "app:" desktop) :app? true
       :search (str/join " " (keep entry ["Exec" "GenericName" "Comment" "Keywords"]))
       :app-icon (resolve-icon (entry "Icon"))
       :action #(run! "gio" "launch" (str path))})))

(defn- load-apps []
  (let [resolve-icon (icons/resolver)]
    (->> (icons/data-dirs)
         (mapcat (fn [dir]
                   (let [base (fs/path dir "applications")]
                     (when (fs/directory? base)
                       (map (fn [path] [(str/replace (str (fs/relativize base path)) "/" "-") path])
                            (fs/glob base "**.desktop"))))))
         (reduce (fn [by-id [id path]]
                   (if (contains? by-id id) by-id (assoc by-id id path))) {})
         vals
         (keep (fn [path] (try (visible-app path resolve-icon) (catch Exception _ nil))))
         (sort-by (comp str/lower-case :label))
         vec)))

(def ^:private app-catalogs (atom {}))

(defn- app-catalog-key []
  (mapv p/getenv ["HOME" "XDG_CURRENT_DESKTOP" "XDG_DATA_HOME" "XDG_DATA_DIRS"
                  "XDG_CONFIG_HOME" "PATH"]))

(defn cached-apps []
  (get-in @app-catalogs [(app-catalog-key) :rows]))

(defn apps []
  (let [key (app-catalog-key)]
    (locking app-catalogs
      (let [{:keys [stamp rows]} (get @app-catalogs key)
            now (System/currentTimeMillis)]
        (if (and rows (< (- now stamp) 60000))
          rows
          (let [rows (search/prepare-rows (load-apps))]
            (swap! app-catalogs #(into {key {:stamp now :rows rows}} (take 3 (dissoc % key))))
            rows))))))

(defn- elisp [form]
  (run! "emacsclient" "--no-wait" "--reuse-frame" "--alternate-editor=false" "--eval" form))

(defn- open-file! [path]
  (let [{:keys [out]} (p/sh {:continue true} "file" "--brief" "--mime-type" "--" path)]
    (if (re-find #"^(?:text/|application/(?:json|xml|x-empty)|inode/x-empty)" out)
      (run! "emacsclient" "--no-wait" "--reuse-frame" "--alternate-editor=false" "--" path)
      (run! "xdg-open" path))))

(defn copy! [text]
  @(p/process {:in text :out :discard :err :discard} "wl-copy"))

(defn file-actions [path]
  (cond-> [{:label "Open" :icon "󰈔" :shortcut "o" :action #(open-file! path)}
           {:label "Reveal in Dired" :icon "󰉋" :shortcut "r"
            :action #(elisp (str "(progn (dired " (pr-str (str (fs/parent path)))
                                 ") (dired-goto-file " (pr-str path) ") (recenter))"))}
           {:label "Copy path" :icon "󰆏" :shortcut "c" :action #(copy! path)}]
    (re-find #"(?i)\.(m4a|mp3|mp4|mkv|mov|webm|wav|flac|ogg|opus|aac|avi)$" path)
    (conj {:label "Transcribe" :icon "󰍬"
           :action #(run! (str dotfiles "/bin/menu/wf_recorder.clj") "transcribe" "--file" path)})))

(defn- project-file []
  (or (p/getenv "LAUNCHER_PROJECTS_FILE") (str (p/getenv "HOME") "/.emacs.d/projects")))

(defn- project-state []
  (str (fs/path (or (p/getenv "XDG_STATE_HOME") (str (p/getenv "HOME") "/.local/state")) "launcher" "projects.json")))

(defn- project-root [path]
  (str (fs/normalize
         (cond
           (= "~" path) (fs/path (p/getenv "HOME"))
           (str/starts-with? path "~/") (fs/path (p/getenv "HOME") (subs path 2))
           (fs/absolute? path) (fs/path path)
           :else (fs/path (or (p/getenv "PWD") (System/getProperty "user.dir")) path)))))

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

(def ^:private project-lock (Object.))

(defn- update-project! [path transform]
  (locking project-lock
    (let [records (project-records)
          updated (update records path #(transform (or % {:root path})))]
      (disk/write-json! (project-state) {:projects (vec (vals updated))}))))

(defn use-project! [path]
  (update-project! path #(assoc % :used (quot (System/currentTimeMillis) 1000))))

(defn- pin-project! [path]
  (update-project! path #(update % :pinned not)))

(defn- forget-project! [path]
  (remember-project! :forget path)
  (update-project! path #(assoc % :forgotten true)))

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
    (conj list {:label "Add project…" :icon "󰐕" :next {:kind :project-add}})))

(defn add-project! [typed]
  (let [path (project-root typed)]
    (when-not (and (fs/absolute? path) (fs/directory? path))
      (throw (ex-info "Choose an existing absolute directory" {})))
    (remember-project! :add path)
    (update-project! path #(assoc % :forgotten false :used (quot (System/currentTimeMillis) 1000)))))

(defn direct-add-row [query]
  (let [query (str/trim query)]
    (when (or (= "~" query) (str/starts-with? query "~/") (fs/absolute? query))
      (let [directory (project-root query)]
        {:label "Add directory as project" :subtitle directory :icon "󰐕"
         :next {:kind :project-add :initial-query query}}))))

(defn project-directories [query]
  (let [query (str/trim query)]
    (when-not (or (str/blank? query) (= "~" query) (str/starts-with? query "~/") (fs/absolute? query))
      (throw (ex-info "Enter ~ or an absolute directory path" {})))
    (let [path (project-root (if (str/blank? query) "~" query))
          directory? (fs/directory? path)
          parent (if directory? path (fs/parent path))
          fragment (if directory? "" (str (fs/file-name path)))
          results (->> (fs/list-dir parent)
                       (filter fs/directory?)
                       (filter #(or (str/starts-with? fragment ".")
                                    (not (str/starts-with? (str (fs/file-name %)) "."))))
                       (mapv (fn [directory]
                               {:label (str (fs/file-name directory)) :subtitle (str directory)
                                :icon "󰉋" :directory (str directory)
                                :fill (str (str/replace (str directory) #"/$" "") "/")})))]
      (into (if directory?
              [{:label "Add directory as project" :subtitle path :icon "󰐕"
                :action #(add-project! path) :refresh :parent}]
              [])
            (take 79 (search/rank fragment (sort-by :label results)))))))

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
             (let [{:keys [out exit]} (p/sh {:dir path :continue true} "timeout" "5" "just"
                                           "--justfile" just-file "--no-dotenv" "--dump" "--dump-format" "json")
                   recipes (when (zero? exit) (get (json/parse-string out false) "recipes"))
                   tasks (->> recipes
                              (keep (fn [[name recipe]]
                                      (when-not (or (str/starts-with? name "_") (get recipe "private")
                                                    (some #(or (= "private" %) (and (map? %) (contains? % "private")))
                                                          (get recipe "attributes")))
                                        (let [params (keep #(get % "name") (get recipe "parameters"))
                                              detail (str/join " · " (remove str/blank?
                                                                           [(get recipe "doc")
                                                                            (when (seq params) (str "args: " (str/join " " params)))]))]
                                          [name detail]))))
                              (sort-by first))]
               (task-view path "Just recipes" "j" ["just" "--justfile" just-file "--"] tasks)))
           (when make-file
             (task-view path "Make targets" "m" ["make" "-f" make-file "--"]
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
            :refresh :parent :action #(forget-project! path)}]
          (try (project-tasks path) (catch Exception _ nil)))))

(defn run-command! [command directory terminal?]
  (let [dir (if (and directory (fs/directory? directory)) directory (p/getenv "HOME"))
        helper (str dotfiles "/bin/menu/run-command")
        args (cond
               terminal?
               ["kitty" "--directory" dir "zsh" "-ic" (str command "; exec zsh")]

               (fs/executable? helper) [helper command]
               :else ["zsh" "-ic" command])
        excluded #{"INVOCATION_ID" "JOURNAL_STREAM" "SYSTEMD_EXEC_PID" "MANAGERPID"
                     "NOTIFY_SOCKET" "LISTEN_PID" "LISTEN_FDS" "LISTEN_FDNAMES"
                     "WATCHDOG_PID" "WATCHDOG_USEC" "PWD" "OLDPWD" "SHLVL" "_"}
          environment (for [[key value] (p/environment)
                            :when (and (not (excluded key)) (re-matches #"[A-Za-z_][A-Za-z0-9_]*" key))]
                        (str "--setenv=" key "=" value))]
    (apply run! "systemd-run" "--user" "--collect" "--quiet" "--expand-environment=no"
           (str "--working-directory=" dir) (concat environment ["--"] args))))

(defn parse-histfile [text]
  (let [[entries pending]
        (reduce (fn [[entries pending] line]
                  (let [line (if (empty? pending) (str/replace line #"^: \d+:\d+;" "") line)]
                    (if (str/ends-with? line "\\")
                      [entries (str pending (subs line 0 (dec (count line))) "\n")]
                      [(conj entries (str pending line)) ""])))
                [[] ""] (str/split-lines text))]
    (cond-> entries (seq pending) (conj pending))))

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
                    parse-histfile reverse (map #(vector % nil))))]
    (loop [seen #{} rows (seq rows) output []]
      (if-let [[command :as entry] (first rows)]
        (if (or (seen command) (str/blank? command))
          (recur seen (next rows) output)
          (recur (conj seen command) (next rows) (conj output entry)))
        output))))
