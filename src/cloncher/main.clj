(ns cloncher.main
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [cloncher.cui :as cui]
            [cloncher.lisp :as lisp]
            [cloncher.machine :as machine]
            [cloncher.menu :as menu]
            [cloncher.search :as search]
            [cloncher.sources :as sources]
            [cloncher.view :as ui-view]))

(def usage
  "Usage: cloncher [root | apps | menu [NAME] | files | projects | project-add DIRECTORY | dmenu [-p PROMPT] | secret [-p PROMPT] | screenshot PATH]")

(defn parse-args [args]
  (let [[command argument value & extra] args]
    (when (or (seq extra)
              (if (contains? #{"dmenu" "secret"} command)
                (and argument (not (and (contains? #{"-p" "--prompt"} argument) value)))
                (some? value)))
      (throw (ex-info usage {})))
    (case command
      (nil "root") (if argument (throw (ex-info usage {})) {:mode :launcher :kind :root})
      "apps" (if argument (throw (ex-info usage {})) {:mode :launcher :kind :apps})
      "files" (if argument (throw (ex-info usage {})) {:mode :launcher :kind :files})
      "projects" (if argument (throw (ex-info usage {})) {:mode :launcher :kind :projects})
      "menu" {:mode :launcher :kind :menu
              :key (keyword (str/replace-first (or argument "main") #"^:" ""))}
      "dmenu" {:mode :dmenu :prompt (or value "Search")}
      "secret" {:mode :secret :prompt (or value "Password")}
      "project-add" (if argument {:mode :project-add :path argument} (throw (ex-info usage {})))
      "screenshot" (if argument {:mode :launcher :kind :root :screenshot argument}
                       (throw (ex-info usage {})))
      "--help" {:mode :help}
      (throw (ex-info usage {})))))

(def ^:private root-items
  (into [{:label "Apps" :icon "󰀻" :action :apps}]
        (menu/get-menu-items :main)))

(defn- menu-view [key]
  (cond
    (= key :apps) {:kind :apps}
    (= key :files) {:kind :files}
    (contains? #{:projects :project-picker "projects" "project-picker"} key) {:kind :projects}
    :else {:kind :menu :key key}))

(defn- prompt [{:keys [kind key path] :as view}]
  (or (:prompt view)
      (case kind
        :root "Search"
        :apps "Apps"
        :files "Find files"
        :projects "Projects"
        :project (str (fs/file-name path))
        :file (str (fs/file-name path))
        :menu (or (:prompt (if (map? key) key (get menu/menus key))) (some-> key name) "Menu")
        "Search")))

(defn- menu-results [key]
  (if (menu/inline-menu? key)
    (:items key)
    (mapv #(cond-> % (:path %) (assoc :deep true)) (menu/menu-rows key))))

(defn- load-view [{:keys [kind key path] :as view}]
  (case kind
    :root (let [apps (future (sources/apps))
                live (future (menu/live-items))
                menus (future (menu-results :main))]
            (into (conj (vec @live) (first root-items)) (concat @menus (map #(assoc % :deep true) @apps))))
    :menu (menu-results key)
    :apps (sources/apps)
    :projects (sources/projects)
    :project (sources/project-actions path)
    :file (sources/file-actions path)
    :tasks (:rows view)
    :dmenu (mapv search/line-row (:lines view))
    []))

(defn- start-view [view]
  (assoc view
         :id (str (random-uuid))
         :query "" :index 0 :page 0 :pinned []
         :rows (case (:kind view)
                 :root root-items
                 :dmenu (load-view view)
                 :tasks (:rows view)
                 :secret []
                 [])
         :build (when-not (contains? #{:dmenu :secret :tasks :files} (:kind view))
                  (future (try {:rows (load-view view)}
                               (catch Exception error {:error (ex-message error)}))))))

(defn- command-options [query]
  (when-let [[_ mode command] (re-matches #"(?s)^\s*(>>>|>>|>)\s*(.*)$" query)]
    {:history? (not= ">" mode) :terminal? (= ">>" mode) :command command}))

(defn- search-job [view query]
  (let [{:keys [kind root]} view
        root? (= kind :root)
        files? (= kind :files)
        lisp? (and root? (lisp/query? query))
        command (when root? (command-options query))
        calc? (and root? (or (str/starts-with? (str/trim query) "=")
                             (menu/calc-expression query)))]
    (cond
      files? {:pending (future (try {:rows (sources/files (or root sources/home) query)}
                                    (catch Exception error {:error (ex-message error)})))}
      lisp? {:pending (future {:pinned (lisp/rows query)}) :exclusive? true}
      (:history? command)
      {:pending (future
                  {:pinned (->> (sources/history)
                                (filter (fn [[text _]]
                                          (every? #(str/includes? (str/lower-case text) %)
                                                  (search/tokens (:command command)))))
                                (take 100)
                                (mapv (fn [[text dir]] (sources/command-row text dir (:terminal? command)))))})
       :exclusive? true}
      command {:exclusive? true
               :pinned (if (str/blank? (:command command)) []
                           [(sources/command-row (:command command) nil false)])}
      calc? {:exclusive? (str/starts-with? (str/trim query) "=")
             :pending (future
                        (try {:pinned (menu/calc-provider query)}
                             (catch Exception error {:error (ex-message error)})))}
      :else {})))

(defn- set-query [view query]
  (when-let [pending (:pending view)] (future-cancel pending))
  (merge (assoc (dissoc view :pending :exclusive? :error) :query query :index 0 :page 0
                :pinned [] :calc nil :lisp nil
                :rows (if (= :files (:kind view)) [] (:rows view)))
         (search-job view query)))

(defn- collect-job [view key]
  (if-let [job (get view key)]
    (if (realized? job)
      (let [result (try @job (catch Exception error {:error (ex-message error)}))]
        (merge (dissoc view key) result
               (when-let [answer (some :calc (:pinned result))] {:calc answer})
               (when-let [answer (some :lisp (:pinned result))] {:lisp answer})))
      view)
    view))

(defn- results [view]
  (into (:pinned view)
        (when-not (:exclusive? view) (search/rank (:query view) (:rows view)))))

(defn- actionable? [row]
  (or (contains? row :value) (:action row) (:next row) (:fill row)))

(defn- choose [view row {:keys [control? shift?]}]
  (let [{:keys [kind query]} view
        action (or (when control? (:alt-action row)) (:action row))]
    (cond
      (= :dmenu kind) (let [value (or (when-not control? (:value row)) query)]
                        {:done true :value (not-empty value)})
      (and (= kind :root) (command-options query) (or control? shift?))
      (let [command (command-options query)
            text (if control? (:command command) (:label row))]
        (when-not (str/blank? text)
          {:done true :effect (:action (sources/command-row
                                         text (when-not control? (:subtitle row))
                                         (or shift? (:terminal? command))))}))
      (:fill row) {:fill (:fill row)}
      (:next row) {:open (:next row)}
      (or (menu/menu-item? action) (menu/inline-menu? action)) {:open (menu-view action)}
      action (if (:refresh row)
               {:done true :effect action :reopen (select-keys view [:key :kind :path :root :prompt])}
               {:done true :effect action}))))

(defn- cancel-view! [view]
  (doseq [job (keep view [:build :pending])]
    (future-cancel job)))

(defn- setup-view [{:keys [kind initial-query fixture index] :as definition}]
  (let [definition (if (= kind :menu)
                     (merge definition (menu-view (:key definition)))
                     definition)
        view (if fixture
               (assoc definition :id (str (random-uuid)) :query "" :index 0 :page 0 :pinned []
                      :rows (:rows fixture) :error (:error fixture))
               (start-view definition))
        view (cond
               fixture (assoc view :query (or initial-query ""))
               (or (= :files (:kind view)) (some? initial-query))
               (set-query view (or initial-query ""))
               :else view)]
    (cond-> view (some? index) (assoc :index index))))

(defn- move-index [index rows delta]
  (let [indexes (vec (keep-indexed (fn [i row] (when (actionable? row) i)) rows))]
    (if (seq indexes)
      (let [nearest (or (when (some #{index} indexes) index)
                        (first (filter #(< index %) indexes))
                        (first indexes))
            i (.indexOf ^java.util.List indexes nearest)]
        (get indexes (mod (+ i delta) (count indexes))))
      0)))

(defn session
  "Create a session controller. Native inputs are owned by the CUI window."
  []
  {:machine (atom machine/initial) :field (volatile! nil) :had-focus? (volatile! false)})

(defn status [session]
  (:status @(:machine session)))

(defn active-kind [session]
  (:kind (machine/current @(:machine session))))

(defn state [session]
  (let [{:keys [status stack] :as model} @(:machine session)
        {:keys [kind query index] :as view} (machine/current model)
        summary {:status status :view kind :depth (count stack)}]
    (if (and view (not= kind :secret))
      (let [rows (results view)]
        (assoc summary :query query :index index :results (count rows)
               :selected (:label (get rows index))))
      summary)))

(defn- transition! [{:keys [machine]} event]
  (swap! machine machine/transition event))

(defn show! [session definition]
  (doseq [view (:stack @(:machine session))] (cancel-view! view))
  (if (= :off (:kind definition))
    (transition! session {:event :off})
    (transition! session {:event :open :view (setup-view definition)})))

(defn close-session! [session]
  (show! session {:kind :off}))

(defn- draw-rows! [ui view rows index page size nested?]
  (let [clicked (volatile! nil)]
    (doseq [i (range page (min (+ page size) (count rows)))]
      (let [row (nth rows i)]
        (when (ui-view/row! ui (str (:id view) "-" i) row (= i index) (boolean (actionable? row)))
          (vreset! clicked row))))
    (ui-view/status! ui view index (count rows) nested?)
    @clicked))

(defn draw-frame!
  "Draw an active launcher session and transition it for this frame's input."
  [{:keys [machine field had-focus?] :as session} ui buffer {:keys [production? preview?]}]
  (let [model @machine
        view (-> (machine/current model) (collect-job :build) (collect-job :pending))
        {:keys [kind id query]} view
        secret? (= :secret kind)
        nested? (< 1 (count (:stack model)))
        focused? (cui/focused? ui)
        _ (when focused? (vreset! had-focus? true))
        dismissed? (and production? @had-focus? (not focused?)
                        (not= "1" (System/getenv "CLONCHER_KEEP_OPEN")))
        shortcuts (if secret? {} (cui/shortcuts ui))
        keys (cui/take-keys! ui (cond-> #{:escape}
                                 (not secret?) (into #{:up :down :enter})
                                 (and nested? (empty? query)) (conj :backspace)))
        field-id (str id "-" (:focus model))]
    (ui-view/begin! ui preview?)
    (try
      (let [back? (when nested? (ui-view/back! ui (prompt view)))
            _ (when (not= @field field-id)
                (cui/set-input! buffer query)
                (vreset! field field-id))
            {:keys [changed? submitted?]} (cui/field! ui field-id buffer (prompt view) secret?)
            typed (when (and changed? (not secret?)) (cui/input-value buffer))
            view (if (and (some? typed) (not= typed query)) (set-query view typed) view)
            rows (if secret? [] (results view))
            size (ui-view/page-size ui nested? rows preview?)
            delta (cond
                    (:down keys) 1
                    (:up keys) -1
                    (:page-down? shortcuts) size
                    (:page-up? shortcuts) (- size)
                    :else 0)
            index (move-index (:index view) rows delta)
            page (* size (quot index size))
            view (assoc view :index index :page page)
            clicked (when-not secret? (draw-rows! ui view rows index page size nested?))
            choice (cond
                     (and secret? submitted?)
                     (let [value (cui/input-value buffer)]
                       (when (and (seq value) (not (re-find #"[\r\n\u0000]" value)))
                         {:done true :value value}))
                     (and changed? (= :root kind) (str/starts-with? (:query view) "/"))
                     {:open {:kind :files :initial-query (subs (:query view) 1)}}
                     (and (= :root kind) (:control? shortcuts) (:r? shortcuts))
                     (let [{:keys [history? command]} (command-options (:query view))]
                       {:fill (if history? (str ">" command) (str ">>" (or command (:query view))))})
                     clicked (choose view clicked shortcuts)
                     (or submitted? (:enter keys)) (choose view (get rows index) shortcuts))]
        (transition! session {:event :view :view view})
        (cond
          (or dismissed? (and (:escape keys) (not nested?)))
          (transition! session {:event :complete})
          (or back? (and nested? (or (:backspace keys) (:escape keys))))
          (do (cancel-view! view)
              (transition! session {:event :back})
              (cui/redraw! ui))
          (:open choice)
          (do (transition! session {:event :push :view (setup-view (:open choice))})
              (cui/redraw! ui))
          (:fill choice)
          (do (transition! session {:event :fill :view (set-query view (:fill choice))})
              (cui/redraw! ui))
          (:done choice) (transition! session {:event :complete :reply choice})
          (or (:pending view) (:build view)) (cui/wake-in! ui 0.05)))
      (finally (cui/end! ui)))))

(defn open-window! [{:keys [mode screenshot] :as request}]
  (let [session (session)
        capture-started? (volatile! false)]
    (show! session (case mode
                     :launcher request
                     :dmenu (assoc request :kind :dmenu)
                     :secret (assoc request :kind :secret)))
    (try
      (cui/window! {:width 680 :height (if (= mode :secret) 125 468)}
        (fn [ui buffer]
          (draw-frame! session ui buffer {:production? (nil? screenshot)})
          (let [{:keys [status] :as model} @(:machine session)]
            (cond
              (= :complete status) (cui/quit!)
              (and screenshot @capture-started?)
              (case (cui/capture-result)
                1 (do (transition! session {:event :complete :reply {:done true}}) (cui/quit!))
                -1 (throw (ex-info "Could not capture the Cloncher window" {}))
                (cui/redraw! ui))
              (and screenshot (nil? (:build (machine/current model))))
              (do (cui/capture! screenshot)
                  (vreset! capture-started? true)
                  (cui/redraw! ui))))))
      (let [{:keys [stack reply]} @(:machine session)]
        (when-let [answer (some :calc (reverse stack))] (menu/save-calc! answer))
        (when-let [answer (some :lisp (reverse stack))] (lisp/save! answer))
        reply)
      (finally (close-session! session)))))

(defn- picker! [request]
  (:value (open-window! request)))

(defn load-cui! []
  (let [requested (System/getenv "CLONCHER_CUI_LIBRARY")
        project (or (System/getenv "CLONCHER_ROOT") ".")
        executable (java.io.File. (.getCanonicalPath (java.io.File. "/proc/self/exe")))
        candidates [(fs/path (.getParent executable) "libcui_host.so")
                    (fs/path project "native/libcui_host.so")
                    (fs/path project "../jolt-poly-template/native/libcui_host.so")]
        library (or requested (some #(when (fs/exists? %) (str (fs/absolutize %))) candidates))]
    (when-not library
      (throw (ex-info "Set CLONCHER_CUI_LIBRARY to the CUI host library or run bin/build-cui" {})))
    (cui/load! library)
    (when-let [font (try
                      (let [{:keys [exit out]} (p/sh {:continue true}
                                                   "fc-match" "--format=%{family}\n%{file}" "Symbols Nerd Font")
                            [family file] (str/split-lines out)]
                        (when (and (zero? exit) (= family "Symbols Nerd Font")) file))
                      (catch Exception _ nil))]
      (cui/fallback-font! font))))

(defn- stdio-lines []
  (with-open [reader (io/reader *in*)]
    (into [] (remove str/blank?) (line-seq reader))))

(defn- perform! [action]
  (if (= action :cloncher/add-project)
    (when-let [path (picker! {:mode :dmenu :prompt "Add project directory" :lines []})]
      (sources/add-project! path))
    (menu/execute-action action)))

(defn run [args]
  (let [{:keys [mode path] :as request} (parse-args args)]
    (case mode
      :help (do (println usage) 0)
      :project-add (do (sources/add-project! path) 0)
      (do
        (load-cui!)
        (let [request (cond-> request (= :dmenu mode) (assoc :lines (stdio-lines)))]
          (loop [request request]
            (let [{:keys [value effect reopen done]} (open-window! request)]
              (if (contains? #{:dmenu :secret} mode)
                (if (some? value) (do (println value) 0) 1)
                (do
                  (when effect
                    (binding [*out* *err* menu/*pick!* picker!]
                      (perform! effect)))
                  (if reopen
                    (recur (assoc reopen :mode :launcher))
                    (if (and (:screenshot request) (not done)) 1 0)))))))))))

(defn -main [& args]
  (System/exit
    (try (run args)
         (catch Exception error
           (binding [*out* *err*] (println "cloncher:" (ex-message error)))
           1))))
