(ns cloncher.main
  (:require [babashka.fs :as fs]
            [cloncher.process :as p]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [cloncher.clipboard :as clipboard]
            [cloncher.commands :as commands]
            [cloncher.cui :as cui]
            [cloncher.daemon :as daemon]
            [cloncher.desktop :as desktop]
            [cloncher.files :as files]
            [cloncher.edit :as edit]
            [cloncher.persistence :as disk]
            [cloncher.preview :as preview]
            [cloncher.palette :as palette]
            [cloncher.panel :as panel]
            [cloncher.lisp :as lisp]
            [cloncher.key-overlay :as key-overlay]
            [cloncher.machine :as machine]
            [cloncher.menu :as menu]
            [cloncher.note :as note]
            [cloncher.search :as search]
            [cloncher.socket :as socket]
            [cloncher.sources :as sources]
            [cloncher.timer-overlay :as timer-overlay]
            [cloncher.view :as ui-view]
            [cloncher.usage :as usage]
            [cloncher.view-cache :as view-cache]))

(def usage
  "Usage: cloncher [root | apps | menu [NAME] | files | projects | projects add [DIRECTORY] | project-add [DIRECTORY] | rename-workspace | note | keys | clipboard-capture | dmenu [-p PROMPT] | secret [-p PROMPT] | daemon | ping | close | screenshot PATH | --screenshot PATH]")

(defn parse-args [args]
  (let [args (if (= ["projects" "add"] (vec (take 2 args)))
               (cons "project-add" (drop 2 args)) args)
        [command argument value & extra] args
        picker? (contains? #{"dmenu" "secret"} command)]
    (when (or (seq extra)
              (if picker?
                (and argument (not (and (contains? #{"-p" "--prompt"} argument) value)))
                (some? value)))
      (throw (ex-info usage {})))
    (case command
      (nil "root" "apps" "files" "projects")
      (if argument (throw (ex-info usage {})) {:mode :launcher :kind (keyword (or command "root"))})
      "menu" {:mode :launcher :kind :menu
              :key (keyword (str/replace-first (or argument "main") #"^:" ""))}
      "dmenu" {:mode :dmenu :prompt (or value "")}
      "secret" {:mode :secret :prompt (or value "Password")}
      "project-add" {:mode :project-add :path (or argument ".")}
      "screenshot" (if argument {:mode :control} (throw (ex-info usage {})))
      "--screenshot" (if argument {:mode :launcher :kind :root :screenshot argument}
                         (throw (ex-info usage {})))
      ("ping" "close" "daemon" "rename-workspace" "note" "keys" "clipboard-capture")
      (if argument (throw (ex-info usage {})) {:mode (keyword command)})
      "--help" {:mode :help}
      (throw (ex-info usage {})))))

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
        :project-add "Add project directory"
        :project (str (fs/file-name path))
        :file (str (fs/file-name path))
        :menu (or (:prompt (menu/definition key)) (some-> key name) "Menu")
        "Search")))

(defn- menu-results [key root?]
  (if (menu/inline-menu? key)
    (:items key)
    (mapv #(cond-> % (:path %) (assoc :deep true)) (menu/menu-rows key root?))))

(defn- load-view [{:keys [kind key path] :as view}]
  (case kind
    :root (let [live (future (menu/live-items))
                root-labels (into {} (map (juxt menu/format-item :shortcut)) (:root-items (menu/configuration)))]
            (into (vec @live)
                  (map #(if-let [code (get root-labels (menu/format-item %))]
                          (assoc % :shortcut code) %))
                  (menu-results :main true)))
    :menu (menu-results key false)
    :apps (search/rank "" (sources/apps) (disk/read-json (disk/cache "history.json") {} false))
    :projects (sources/projects)
    :project-add []
    :project (sources/project-actions path)
    :file (sources/file-actions path)
    :tasks (:rows view)
    :dmenu (mapv search/line-row (:lines view))
    []))

;; The labels the live rows last had, so an opening root view gives them the same ones
;; its fresh build will.
(defonce ^:private live-labels (atom {}))

(defn- live-key [row] (or (:row-id row) (menu/format-item row)))

(defn- build-view [view]
  (try
    (let [rows (search/index-rows (load-view view))]
      (try (view-cache/save-rows! view rows) (catch Exception _ nil))
      (when (= :root (:kind view))
        (reset! live-labels (into {} (keep #(when (:live %) [(live-key %) (:shortcut %)])) rows)))
      {:rows rows})
    (catch Exception error {:error (ex-message error)})))

(def ^:dynamic ^:private *frame-signal* nil)

(defn- frame-job [work]
  (if-let [signal *frame-signal*]
    (let [result (promise)]
      {:result result
       :task (future
               (deliver result (try (work) (catch Throwable error error)))
               (when-let [wake @signal] (wake)))})
    (future (work))))

(defn- job-ready? [job]
  (realized? (or (:result job) job)))

(defn- job-value [job]
  (let [result @(or (:result job) job)]
    (if (instance? Throwable result) (throw result) result)))

(defn- cancel-job! [job]
  (future-cancel (or (:task job) job)))

(defn- poll-jobs! [ui]
  (when-not *frame-signal* (cui/wake-in! ui 0.012)))

(defn- start-view [{:keys [kind key] :as view}]
  (let [usage (when (contains? #{:root :apps} kind)
                (disk/read-json (disk/cache "history.json") {} false))
        apps (when (contains? #{:root :apps} kind) (sources/cached-apps))]
    (assoc view
           :id (str (random-uuid))
           :query "" :index 0 :page 0 :pinned []
           :usage usage
           :apps (when (= :root kind) apps)
           :apps-build (when (= :root kind) (frame-job #(hash-map :apps (sources/apps))))
           ;; The root opens with the last live rows above the cached menu, so nothing
           ;; moves when the fresh read lands.
           :rows (cond->> (or (view-cache/cached-rows view)
                              (case kind
                                :root (:root-items (menu/configuration))
                                :apps (search/index-rows (search/rank "" apps usage))
                                :menu (search/index-rows (menu/static-items key))
                                :dmenu (search/index-rows (load-view view))
                                :tasks (search/index-rows (:rows view))
                                []))
                    (and (= :root kind) (seq (menu/recent-live-items)))
                    (#(search/index-rows (into (mapv (fn [row] (assoc row :shortcut (@live-labels (live-key row))))
                                                     (menu/recent-live-items))
                                               %))))
           :next-live (+ (System/currentTimeMillis) 2000)
           :build (when-not (contains? #{:dmenu :secret :tasks :files :project-add} kind)
                    (frame-job #(build-view view))))))

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
      (= :project-add kind)
      {:pending (frame-job #(try {:pinned (sources/project-directories query)}
                             (catch Exception error {:error (ex-message error)})))
       :exclusive? true}
      (= :projects kind) {:pinned (into [] (keep identity) [(sources/direct-add-row query)])}
      files? {:rows (if (str/blank? query) (or (files/cached-recent root) []) [])
              :pending (frame-job #(try {:rows (files/rows root query)}
                                     (catch Exception error {:error (ex-message error)})))}
      lisp? {:pending (frame-job #(hash-map :pinned (lisp/rows query))) :exclusive? true}
      (:history? command)
      {:pending (frame-job #(hash-map :pinned (commands/history (:command command) (:terminal? command))))
       :exclusive? true}
      command {:exclusive? true
               :pinned (or (commands/default-rows (:command command)) [])
               :pending (frame-job #(hash-map :pinned (commands/suggestions (:command command))))}
      calc? {:exclusive? (str/starts-with? (str/trim query) "=")
             :pending (frame-job #(try {:pinned (menu/calc-provider query)}
                                       (catch Exception error {:error (ex-message error)})))}
      :else {})))

(defn- set-query [view query]
  (doseq [job (keep view [:pending :activation])] (cancel-job! job))
  (merge (assoc (dissoc view :pending :activation :resolved :exclusive? :error :jump :ranked) :query query :index 0 :page 0
                :pinned [] :calc nil :lisp nil
                :rows (if (= :files (:kind view)) [] (:rows view)))
         (search-job view query)))

(defn- collect-job [view key]
  (if-let [job (get view key)]
    (if (job-ready? job)
      (let [result (try (job-value job) (catch Exception error {:error (ex-message error)}))
            result (cond-> result
                     (contains? result :pinned) (update :pinned search/index-rows))]
        (merge (cond-> (dissoc view key :ranked)
                 (= :build key) (dissoc :refresh-progress))
               result
               (when-let [answer (some :calc (:pinned result))] {:calc answer})
               (when-let [answer (some :lisp (:pinned result))] {:lisp answer})))
      view)
    view))

(defn- results [{:keys [pinned exclusive? rows query jump usage kind ranked apps]}]
  (or (:rows jump)
      ranked
      (into pinned
            (when-not exclusive?
              (let [apps? (and (= :root kind) (seq apps) (not (str/blank? query)))
                    matches (search/rank query (if apps? (into rows apps) rows) usage)
                    matches (if (or (str/blank? query) (= :dmenu kind)) matches (vec (take 80 matches)))]
                (if apps? (search/label-rows matches) matches))))))

(defn- actionable? [row]
  (or (contains? row :value) (:action row) (:next row) (:fill row) (:cached-id row)))

(defn- row-key [row]
  (or (:row-id row) (:value row) (when row (menu/format-item row))))

(defn- choose [view row {:keys [control? shift?]}]
  (let [{:keys [kind query]} view
        action (or (when control? (:alt-action row)) (:action row))]
    (cond
      (= :dmenu kind) (let [value (or (when-not control? (:value row)) query)]
                        {:done true :value (if (:allow-empty? view) value (not-empty value))})
      (and (= kind :root) control? (nil? (:alt-action row))
           (not (lisp/query? query)) (not (str/blank? query)) (nil? (command-options query)))
      {:done true :effect (:action (commands/row (str/trim query) nil
                                                 (or shift? (commands/terminal? query)) nil))}
      (and (= kind :root) (command-options query) (or control? shift?))
      (let [command (command-options query)
            text (if control? (:command command) (:command row))]
        (when-not (str/blank? text)
          {:done true :effect (:action (commands/row
                                         text (when-not control? (:directory row))
                                         (or shift? (:terminal? command)
                                             (and control? (not (:history? command)) (commands/terminal? text))) nil))}))
      (:fill row) {:fill (:fill row)}
      (and (:cached-id row) (or (nil? action) control?)) {:resolve (:cached-id row) :shortcuts {:control? control? :shift? shift?}}
      (and control? (:file row))
      {:done true :effect (:action (second (sources/file-actions (:file row))))}
      (:next row) {:open (:next row)}
      (or (menu/menu-item? action) (menu/inline-menu? action)) {:open (menu-view action)}
      action (if (:refresh row)
               (when-not (:refresh-progress view)
                 {:refresh action :destination (:refresh row)
                  :progress {:view-id (:id view) :row-key (row-key row) :label (:progress row)}})
               {:done true :effect action}))))

(defn- cancel-view! [view]
  (doseq [job (keep view [:build :pending :live-job :activation :apps-build])]
    (cancel-job! job)))

(defn- setup-view [{:keys [kind initial-query fixture index] :as definition}]
  (let [definition (if (= kind :menu)
                     (merge definition (menu-view (:key definition)))
                     definition)
        view (if fixture
               (assoc definition :id (str (random-uuid)) :query "" :index 0 :page 0 :pinned []
                      :rows (search/index-rows (:rows fixture)) :error (:error fixture))
               (start-view definition))
        view (cond
               fixture (assoc view :query (or initial-query ""))
               (or (contains? #{:files :project-add} (:kind view)) (some? initial-query))
               (set-query view (or initial-query ""))
               :else view)]
    (cond-> view (some? index) (assoc :index index))))

(defn- move-index [index rows delta]
  (if (and (zero? delta) (actionable? (get rows index)))
    index
    (let [indexes (vec (keep-indexed (fn [i row] (when (actionable? row) i)) rows))]
      (if (seq indexes)
        (let [nearest (or (when (some #{index} indexes) index)
                          (first (filter #(< index %) indexes))
                          (first indexes))
              i (.indexOf ^java.util.List indexes nearest)]
          (get indexes (mod (+ i delta) (count indexes))))
        0))))

(defn session
  "Create a session controller. Native inputs are owned by the CUI window."
  []
  {:machine (atom machine/initial) :field (volatile! nil) :had-focus? (volatile! false)
   :preview (volatile! nil) :used (volatile! []) :icons (volatile! {})})

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

(defn reconfigure! [session]
  (let [{:keys [status stack]} @(:machine session)]
    (when (= :on status)
      (let [views (mapv (fn [{:keys [kind key query fixture] :as view}]
                          (if (and (contains? #{:root :menu} kind) (not fixture) (not (map? key)))
                            (do (cancel-view! view)
                                (setup-view (assoc (select-keys view [:kind :key :prompt :index])
                                                   :initial-query query)))
                            view))
                        stack)]
        (when (some true? (map #(not (identical? %1 %2)) stack views))
          (transition! session {:event :reconfigure :views views}))))))

(defn close-session! [session]
  (show! session {:kind :off}))

(defn- preview-key
  "A preview target is a file path, or a row's `{:id :load}` for content that is not a file."
  [target]
  (if (map? target) (:id target) target))

(defn- preview-state! [session ui target]
  (let [holder (:preview session)
        {:keys [job image] :as current} @holder
        key (preview-key target)]
    (cond
      (not= (:target current) key)
      (do
        (when job (cancel-job! job))
        (when image (cui/destroy-texture! ui (:texture image)))
        (vreset! holder
                 (when target
                   {:target key :working? true
                    :job (frame-job (if (map? target) (:load target) #(preview/load-preview target)))}))
        (when target (poll-jobs! ui)))

      (and job (job-ready? job))
      (let [{:keys [bytes svg] :as answer} (try (job-value job) (catch Exception _ {:notice "Preview unavailable"}))]
        ;; An SVG answer (a QR code) is rasterized from memory; its text never reaches a file.
        (vreset! holder (assoc (dissoc answer :bytes :svg) :target key
                               :image (cond bytes (cui/load-thumbnail! ui bytes)
                                            svg (cui/svg-thumbnail! ui svg 512)))))

      (:working? current) (poll-jobs! ui))
    @holder))

(defn clear-preview! [session ui]
  (preview-state! session ui nil))

(defn- row-texture! [holder ui path]
  (if (contains? @holder path)
    (get @holder path)
    (let [texture (cui/icon-texture! ui path)]
      (when (>= (count @holder) 64)
        (let [[key old] (first @holder)]
          (when old (cui/destroy-texture! ui old))
          (vswap! holder dissoc key)))
      (vswap! holder assoc path texture)
      texture)))

(defn- render-rows! [session ui view rows first end index width row-height]
  (let [clicked (volatile! nil)
        prefix (get-in view [:jump :prefix] "")
        progress (:refresh-progress view)]
    (doseq [i (range first end)]
      (let [previous (get rows (dec i))
            row (nth rows i)
            section-start? (ui-view/section-start? previous row)
            divided? (ui-view/divided? previous row)
            shortcut (:shortcut row)
            row (cond-> row
                  row-height (assoc :row-height row-height)
                  ;; A paged list has a fixed row height, so the row draws its own divider.
                  (and row-height divided?) (assoc :divider? true)
                  (and (= (:view-id progress) (:id view)) (= (:row-key progress) (row-key row)))
                  (assoc :running-progress progress)
                  (and shortcut (str/starts-with? shortcut prefix))
                  (assoc :display-shortcut (subs shortcut (count prefix)))
                  (:app-icon row) (assoc :texture (row-texture! (:icons session) ui (:app-icon row))))]
        (cond row-height nil
              divided? (ui-view/divider! ui (str (:id view) "-divider-" i))
              section-start? (cui/separator! ui))
        (when (ui-view/row! ui (str (:id view) "-" i) row (= i index) (boolean (actionable? row)) width)
          (vreset! clicked row))))
    @clicked))

(defn- rows-pane! [session ui view rows index page {:keys [page-size virtual-row-height]} width]
  (let [count (count rows)]
    (if-not virtual-row-height
      (render-rows! session ui view rows 0 count index width nil)
      (let [height virtual-row-height
            id (str "visible-" (:id view) "-" (:query view))
            _ (when (:scroll-selection? view)
                (panel/scroll-to! ui id (double (* height page))))
            [first last] (panel/virtual-list! ui id count height (* page-size height))]
        (try
          (render-rows! session ui view rows first last index width height)
          (finally (panel/end-list! ui)))))))

(defn- draw-results! [session ui view rows index page layout width path]
  (let [preview-width (min 265 (quot width 2))
        preview (preview-state! session ui path)]
    (if path
      (do
        (panel/begin! ui "file-results" {:row? true :gap 8
                                         :height (ui-view/preview-body-height layout)})
        (try
          (let [clicked (do
                          (cui/column! ui "results" {:pad 0 :gap 0})
                          (try (rows-pane! session ui view rows index page layout (- width preview-width 12))
                               (finally (cui/end! ui))))]
            (ui-view/file-preview! ui preview-width (preview-key path) preview)
            clicked)
          (finally (cui/end! ui))))
      (rows-pane! session ui view rows index page layout width))))

(defn- inserted-text [old typed caret]
  (let [length (when typed (- (count typed) (count old)))]
    (when (and caret length (pos? length) (<= length caret))
      (let [start (- caret length)]
        (when (= old (str (subs typed 0 start) (subs typed caret)))
          {:start start :text (subs typed start caret)})))))

(defn- jump-choice [view rows prefix latched? restore-index]
  (let [{:keys [exact first]} (search/jump rows prefix)]
    {:view (cond-> (assoc view :jump {:rows rows :prefix prefix :latched? latched?})
             (some? first) (assoc :index first))
     :target (when (some? exact) (nth rows exact))
     :restore-index restore-index :consumed? true}))

(defn- handle-typing [view typed caret keys {:keys [letter alt? control? jump-cancel?]}]
  (let [{:keys [query kind jump]} view
        latched? (:latched? jump)
        {start :start inserted :text} (inserted-text query typed caret)
        [_ initial-code] (when inserted (re-matches #";([a-z0-9]*)" inserted))
        begin? (and (some? initial-code) (not latched?)
                    (not (and (= :root kind) (or (command-options query) (lisp/query? query)))))]
    (cond
      (and jump (or (:escape keys) jump-cancel? (and (:backspace keys) (empty? (:prefix jump)))))
      {:view (dissoc view :jump) :consumed? true}

      (and jump (:backspace keys))
      {:view (update-in view [:jump :prefix] #(subs % 0 (dec (count %)))) :consumed? true}

      (and (= inserted ";") latched?) {:view (set-query view typed)}

      begin?
      (let [rows (results view)]
        (if (seq rows)
          (jump-choice view rows initial-code true start)
          {:view (set-query view typed)}))

      (and (or alt? latched?) (not control?))
      (let [code (or (when (and latched? inserted (re-matches #"[a-z0-9]+" inserted)) inserted) letter)]
        (cond
          code (jump-choice view (or (:rows jump) (results view)) (str (:prefix jump) code)
                            latched? (when typed (or start (count query))))
          (and latched? typed) {:view view :restore-index (or start (count query)) :consumed? true}
          :else {:view view}))

      (and (some? typed) (not= typed query))
      (if-let [destination (when (= :root kind)
                             (case (first typed) \/ :files \@ :projects nil))]
        {:view (dissoc view :jump) :open {:kind destination :initial-query (subs typed 1)}}
        {:view (set-query view typed)})

      :else {:view (if (and jump (not latched?) (not alt?)) (dissoc view :jump) view)})))

(defn- copy-choice [view row]
  (let [command (when (= :root (:kind view)) (command-options (:query view)))
        text (or (:file row) (:path (when (= :file (:kind view)) view))
                 (:command row) (when command (not-empty (:command command))))]
    (when text
      {:done true :effect #(sources/copy! text)})))

(defn- complete-choice [view row rows]
  (if (= :project-add (:kind view))
    (when-let [directory (or (:directory row) (some :directory rows))]
      {:fill (str (str/replace directory #"/$" "") "/")})
    (when-let [{:keys [command]} (when (= :root (:kind view)) (command-options (:query view)))]
      (when-let [suggestion (or (when (not= command (:command row)) (:command row))
                                (some #(when (not= command (:command %)) (:command %)) rows))]
        {:fill (str ">" suggestion (when-not (str/includes? suggestion " ") " "))}))))

(defn- preserve-selection [previous view]
  (if (and (= (:query previous) (:query view))
           (or (not (identical? (:rows previous) (:rows view)))
               (not (identical? (:pinned previous) (:pinned view)))
               (not (identical? (:apps previous) (:apps view)))))
    (let [selected (row-key (get (results previous) (:index previous)))
          index (when selected
                  (first (keep-indexed (fn [i row] (when (= selected (row-key row)) i))
                                       (results view))))]
      (cond-> view (some? index) (assoc :index index)))
    view))

(defn- update-live [ui view]
  (if (or (not= :root (:kind view)) (:fixture view))
    view
    (let [now (System/currentTimeMillis)
          job (:live-job view)]
      (cond
        (and job (job-ready? job))
        (let [rows (try (job-value job) (catch Exception _ nil))
              view (dissoc view :live-job)]
          (cui/wake-in! ui (max 0.001 (/ (- (:next-live view) now) 1000.0)))
          (if rows
            (assoc (dissoc view :ranked)
                   :rows (search/index-rows (into (mapv #(assoc % :shortcut (@live-labels (live-key %))) rows)
                                                  (remove :live (:rows view)))))
            view))
        (and (nil? job) (not (:build view)) (>= now (:next-live view 0)))
        (do (poll-jobs! ui)
            (assoc view :next-live (+ now 2000) :live-job (frame-job menu/live-items)))
        :else (do (if job (poll-jobs! ui) (cui/wake-in! ui 2.0)) view)))))

(defn draw-frame!
  "Draw an active launcher session and transition it for this frame's input."
  [{:keys [machine field had-focus?] :as session} ui buffer {:keys [production? preview? fit?]}]
  (let [model @machine
        previous (machine/current model)
        view (->> (-> previous (collect-job :build) (collect-job :pending)
                        (collect-job :apps-build) (collect-job :activation))
                  (update-live ui)
                  (preserve-selection previous))
        {:keys [kind id query]} view
        secret? (= :secret kind)
        nested? (< 1 (count (:stack model)))
        focused? (cui/focused? ui)
        _ (when focused? (vreset! had-focus? true))
        dismissed? (and production? @had-focus? (not focused?)
                        (not= "1" (System/getenv "CLONCHER_KEEP_OPEN")))
        shortcuts (if secret? {} (cui/shortcuts ui (some? (:jump view))))
        keys (cui/take-keys! ui (cond-> #{:escape}
                                 (not secret?) (into #{:up :down :enter :tab})
                                 (or (:jump view) (and nested? (empty? query))) (conj :backspace)))
        field-id (str id "-" (:focus model))]
    (ui-view/begin! ui preview? fit?)
    (try
      (let [_ (when (not= @field field-id)
                (cui/set-input! buffer query)
                (vreset! field field-id))
            {:keys [changed? submitted?]} (ui-view/prompt! ui field-id buffer (prompt view) secret?)
            typed (when (and changed? (not secret?)) (cui/input-value buffer))
            caret (when typed (cui/caret ui field-id typed))
            paired (when (and typed (= :root kind) (or (lisp/query? typed) (lisp/query? query)))
                     (edit/paired-edit query typed caret))
            typed (or (:text paired) typed)
            {:keys [view target restore-index consumed?] destination :open}
            (handle-typing view typed caret keys shortcuts)
            _ (cond
                (some? restore-index) (cui/replace-input! ui field-id buffer query restore-index)
                paired (cui/replace-input! ui field-id buffer (:text paired) (:caret paired)))
            rows (if secret? [] (results view))
            height (cond
                     fit? ui-view/preview-height
                     preview? (min ui-view/preview-height (- (cui/height ui) 108))
                     :else (cui/height ui))
            layout (ui-view/result-layout height rows)
            size (:page-size layout)
            delta (cond
                    (or (:down keys) (:next? shortcuts)) 1
                    (or (:up keys) (:previous? shortcuts)) -1
                    (and (:tab keys) (not= :project-add kind)
                         (not (and (= :root kind) (command-options (:query view)))))
                    (if (:shift? shortcuts) -1 1)
                    (:page-down? shortcuts) size
                    (:page-up? shortcuts) (- size)
                    :else 0)
            index (move-index (:index view) rows delta)
            page (* size (quot index size))
            view (assoc view :index index :page page :ranked (when-not (:jump view) rows)
                        :scroll-selection? (and (or (not (zero? delta)) consumed?)
                                                (not= (:page view) page)))
            width (if preview?
                    (min ui-view/preview-width (max 1 (- (cui/width ui) 64)))
                    (cui/width ui))
            path (when (>= width 500)
                   (let [row (get rows index)]
                     (or (:file row) (:preview row) (when (= :file kind) (:path view)))))
            clicked (when-not secret?
                      (let [clicked (draw-results! session ui view rows index page layout width path)]
                        (when fit? (cui/spacer! ui))
                        (ui-view/status! ui view (get rows index) (count rows) nested?)
                        clicked))
            resolved (:resolved view)
            view (dissoc view :resolved)
            ;; A row's Ctrl+letter binding acts on it and keeps the view open, refreshed.
            bound (get-in (get rows index) [:keys (:control-letter shortcuts)])
            choice (cond
                     resolved (choose view (:row resolved) (:shortcuts resolved))
                     (and secret? submitted?)
                     (let [value (cui/input-value buffer)]
                       (when (and (seq value) (not (re-find #"[\r\n\u0000]" value)))
                         {:done true :value value}))
                     target (some-> (choose (dissoc view :jump) target shortcuts)
                                    (assoc :used target))
                     destination {:open destination}
                     (and (= :root kind) (:history? shortcuts))
                     (let [{:keys [history? command]} (command-options (:query view))]
                       {:fill (if history? (str ">" command) (str ">>" (or command (:query view))))})
                     bound {:refresh (:action bound) :destination :current
                            :progress {:view-id (:id view) :row-key (row-key (get rows index))}}
                     (:copy? shortcuts) (copy-choice view (get rows index))
                     (:tab keys) (complete-choice view (get rows index) rows)
                     clicked (some-> (choose view clicked shortcuts) (assoc :used clicked))
                     (or submitted? (:enter keys))
                     (let [row (get rows index)] (some-> (choose view row shortcuts) (assoc :used row))))]
        (transition! session {:event :view :view view})
        (when (and production? (contains? #{:root :apps} kind) (:used choice))
          (vswap! (:used session) conj (:used choice)))
        (cond
          (or dismissed? (and (not consumed?) (:escape keys) (not nested?)))
          (transition! session {:event :complete})
          (and (not consumed?) nested? (or (:backspace keys) (:escape keys)))
          (do (cancel-view! view)
              (transition! session {:event :back})
              (cui/redraw! ui))
          (:resolve choice)
          (let [key (or (:key view) :main)
                id (:resolve choice)]
            (transition! session {:event :view :view
                                  (assoc view :activation
                                         (frame-job #(hash-map :resolved {:row (menu/resolve-row key id (= :root kind))
                                                                          :shortcuts (:shortcuts choice)})))})
            (poll-jobs! ui))
          (:open choice)
          (do
            (let [target (:open choice)]
              (when (and production? (= :project (:kind target)))
                (future (try (sources/use-project! (:path target)) (catch Exception _ nil))))
              (transition! session {:event :push :view (setup-view target)}))
            (cui/redraw! ui))
          (and production? (:refresh choice))
          (let [parent? (= :parent (:destination choice))
                target (if parent? (or (peek (pop (:stack model))) {:kind :projects}) view)
                action (:refresh choice)
                job (frame-job
                      #(try
                        (menu/execute-action action)
                        (build-view target)
                        (catch Exception error {:error (ex-message error)})))]
            (when-let [build (:build target)] (cancel-job! build))
            (when parent? (transition! session {:event :back}))
            (transition! session {:event :view :view
                                  (assoc (dissoc target :error) :build job :refresh-progress (:progress choice))})
            (cui/redraw! ui))
          (:refresh choice)
          (transition! session {:event :complete})
          (:fill choice)
          (do (transition! session {:event :fill :view (set-query view (:fill choice))})
              (cui/redraw! ui))
          (:done choice) (transition! session {:event :complete :reply choice})
          consumed? (cui/redraw! ui)
          (or (:pending view) (:build view) (:apps-build view) (:activation view)) (poll-jobs! ui))
        (if secret? ui-view/secret-height (ui-view/window-height layout (some? path))))
      (finally (cui/end! ui)))))

(defn- capture-frame! [ui capture started]
  (when-let [{:keys [path reply] :as current} @capture]
    (if (identical? current @started)
      (when-not (zero? (cui/capture-result))
        (reset! capture nil)
        (vreset! started nil)
        (deliver reply {:exit (if (pos? (cui/capture-result)) 0 1)}))
      (do (cui/capture! path)
          (vreset! started current)
          (cui/redraw! ui)))))

(defn- open-configured-window! [{:keys [mode screenshot redraw capture cancelled disconnected? timeout-ms] :as request}]
  (let [session (session)
        redraw (or redraw (atom nil))
        capture (or capture (atom nil))
        capture-started (volatile! nil)
        palette (volatile! {:until 0 :colors {}})
        placement (volatile! nil)
        initialized? (volatile! false)
        resize-request (volatile! nil)
        fit? (not= "1" (p/getenv "CLONCHER_TILING"))
        deadline (when timeout-ms (+ (System/currentTimeMillis) timeout-ms))
        width ui-view/preview-width
        screenshot-reply (when screenshot (promise))]
    (when (and (= :launcher mode) (nil? (:fd request)))
      (future (view-cache/prime!)))
    (when screenshot
      (reset! capture {:path screenshot :reply screenshot-reply}))
    (binding [*frame-signal* redraw]
      (show! session (case mode
                       :launcher request
                       :dmenu (assoc request :kind :dmenu)
                       :secret (assoc request :kind :secret))))
    (try
      (let [view (machine/current @(:machine session))
            rows (results view)
            height (cond
                     (= mode :secret) ui-view/secret-height
                     fit? (ui-view/window-height (ui-view/result-layout ui-view/preview-height rows)
                                                 (or (= :file (:kind view)) (:file (get rows (:index view)))))
                     :else ui-view/preview-height)]
        (transition! session {:event :view :view (assoc view :ranked rows)})
        (cui/window! {:width width :height height :redraw redraw
                      :prepared? (and fit? (:fd request) (nil? screenshot))
                      :app-id (if fit? "cloncher.launcher" "cloncher.launcher.tiled")}
        (fn [ui buffer]
          (cond
            (or (and cancelled @cancelled) (and disconnected? (disconnected?))
                (and deadline (>= (System/currentTimeMillis) deadline)))
            (cui/quit!)
            :else
            (do
              (when (and fit? (not @initialized?))
                (cui/allow-compact-window! 360 ui-view/secret-height)
                (when (and (not screenshot) (not (cui/layer-active?)))
                  (vreset! placement (future (desktop/place-launcher! (cui/process-id) width))))
                (vreset! initialized? true))
              (when deadline (cui/wake-in! ui 1.0))
              (when (>= (System/currentTimeMillis) (:until @palette))
                (vreset! palette {:until (+ (System/currentTimeMillis) 1000)
                                  :colors (palette/load-colors)}))
              (cui/apply-colors! ui (:colors @palette))
              (when (seq (:colors @palette)) (cui/wake-in! ui 1.0))
              (let [desired-height (binding [*frame-signal* redraw]
                                     (draw-frame! session ui buffer {:production? (nil? screenshot)
                                                                     :fit? fit?}))
                    {:keys [status] :as model} @(:machine session)
                    now (System/currentTimeMillis)
                    placing? (and @placement (not (realized? @placement)))]
                (when (and fit? (not= :complete status))
                  (cond
                    placing? (cui/wake-in! ui 0.02)
                    (not= desired-height (:height @resize-request))
                    (do
                      (vreset! resize-request {:height desired-height :at now})
                      (when (not= [width desired-height] [(cui/width ui) (cui/height ui)])
                        (cui/resize-window! width desired-height)))))
                (when (and (not= :secret mode)
                           (or (nil? screenshot) (nil? (:build (machine/current model)))))
                  (if (or @capture-started (not fit?)
                          (and (not placing?)
                               (or (= desired-height (cui/height ui))
                                   (>= (- now (:at @resize-request now)) 300))))
                    (capture-frame! ui capture capture-started)
                    (when @capture (cui/wake-in! ui 0.02))))
                (cond
                  (= :complete status) (cui/quit!)
                  (and screenshot (realized? screenshot-reply))
                  (do
                    (transition! session {:event :complete :reply {:done true :status (:exit @screenshot-reply)}})
                    (cui/quit!))
                  disconnected? (cui/wake-in! ui 0.2))))))))
      (let [{:keys [stack reply]} @(:machine session)]
        (when-not (and cancelled @cancelled)
          (doseq [row @(:used session)]
            (try (usage/remember! row) (catch Exception _ nil)))
          (when-let [answer (some :calc (reverse stack))] (menu/save-calc! answer))
          (when-let [answer (some :lisp (reverse stack))] (lisp/save! answer))
          reply))
      (finally
        (when-let [job (:job @(:preview session))] (cancel-job! job))
        (close-session! session)))))

(defn open-window! [request]
  (binding [menu/*configuration* (menu/load-configuration!)]
    (open-configured-window! request)))

;; The daemon records the clipboard while `:clipboard-history` is on: `wl-paste --watch`
;; runs `cloncher clipboard-capture` for every change.
(defonce ^:private clipboard-watcher (atom nil))

(defn- sync-clipboard-watch! [enabled?]
  (let [running @clipboard-watcher
        alive? (and running (.isAlive ^Process (:proc running)))]
    (cond
      (and enabled? (not alive?)) (reset! clipboard-watcher (clipboard/watch! (p/self-command)))
      (and (not enabled?) running) (do (p/destroy-tree running) (reset! clipboard-watcher nil)))))

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

(defn- run-effect! [effect picker]
  (when effect
    (binding [*out* *err* menu/*pick!* picker]
      (menu/execute-action effect))))

(defn- open-and-finish! [request]
  (let [{:keys [effect] :as result} (open-window! request)]
    (run-effect! effect picker!)
    result))

(defn- picker-reply [{:keys [mode prompt lines] :as request}]
  (socket/request! {:args [(name mode) "-p" prompt]
                    :lines (or lines []) :env (mapv vec (p/environment))
                    :options (select-keys request [:allow-empty? :timeout-ms])}))

(defn- daemon-picker! [request]
  (:out (picker-reply request)))

(defn- daemon-reply [args lines]
  (let [args (if (and (= "screenshot" (first args)) (second args))
               ["screenshot" (str (fs/absolutize (second args)))]
               args)]
    (socket/request! {:args (vec args) :lines lines :env (mapv vec (p/environment))})))

(defn run [args]
  (let [{:keys [mode path] :as request} (parse-args args)]
    (case mode
      :help (do (println usage) 0)
      :project-add (do (sources/add-project! path) 0)
      :rename-workspace (do
                          (run-effect! menu/rename-workspace
                                       (fn [picker]
                                         (if-let [reply (when-not (p/getenv "LAUNCHER_STANDALONE")
                                                          (picker-reply picker))]
                                           (:out reply)
                                           (do (load-cui!) (picker! picker)))))
                          0)
      ;; The editor runs in this process; the hotkey client already started a fresh one.
      :note (do (menu/scratchpad-note! #(note/worker! % load-cui!)) 0)
      :clipboard-capture (do (clipboard/capture!) 0)
      :keys (do (menu/toggle-key-display!) 0)
      :ping (or (:exit (daemon-reply args [])) 1)
      :close (or (:exit (daemon-reply args [])) 0)
      :control (or (:exit (daemon-reply args [])) 3)
      :daemon (do (load-cui!)
                  (let [configuration (menu/load-configuration!)
                        width ui-view/preview-width
                        height (:height (ui-view/result-layout ui-view/preview-height (:root-items configuration)))
                        primed? (volatile! false)]
                    (or (daemon/serve! parse-args open-window!
                                       (fn [{:keys [effect]}] (run-effect! effect daemon-picker!))
                                       (fn []
                                         ;; Runs between sessions, so a config edit takes effect on the next open.
                                         (sync-clipboard-watch! (:clipboard-history (menu/load-configuration!)))
                                         ;; A session may have changed what is running; read it before the next open.
                                         (future (try (menu/live-items) (catch Exception _ nil)))
                                         (when-not (= "1" (p/getenv "CLONCHER_TILING"))
                                           (cui/prepare-launcher! width height))
                                         (when-not @primed?
                                           (vreset! primed? true)
                                           ;; Keep the live rows at most five seconds old for an instant root view.
                                           (future (loop []
                                                     (Thread/sleep 5000)
                                                     (try (menu/live-items) (catch Exception _ nil))
                                                     (recur)))
                                           (binding [menu/*configuration* configuration]
                                             (future (view-cache/prime!)
                                                     ;; Labels the live rows, so even the first open keeps them.
                                                     (build-view {:kind :root})))
                                           (future (sources/apps))
                                           (future (files/prime!)))))
                        0)))
      (let [lines (when (= :dmenu mode) (stdio-lines))
            remote (when-not (or (:screenshot request) (System/getenv "LAUNCHER_STANDALONE"))
                     (daemon-reply args (or lines [])))]
        (if remote
          (do (when-some [value (:out remote)] (println value)) (:exit remote))
          (do
            (load-cui!)
            (let [{:keys [value done status]} (open-and-finish! (assoc request :lines lines))]
              (cond
                status status
                (contains? #{:dmenu :secret} mode)
                (if (some? value) (do (println value) 0) 1)
                (and (:screenshot request) (not done)) 1
                :else 0))))))))

(defn -main [& args]
  (System/exit
    (try (case (first args)
           "--timer-overlay"
           (let [[_ path initial & extra] args]
             (when-not (and path initial (empty? extra)) (throw (ex-info usage {})))
             (timer-overlay/worker! path initial load-cui!)
             0)
           "--note-editor"
           (let [[_ path & extra] args]
             (when-not (and path (empty? extra)) (throw (ex-info usage {})))
             (note/worker! path load-cui!)
             0)
           "--key-overlay"
           (do (when (next args) (throw (ex-info usage {})))
               (key-overlay/worker! load-cui!)
               0)
           (run args))
         (catch Exception error
           (binding [*out* *err*] (println "cloncher:" (ex-message error)))
           1))))
