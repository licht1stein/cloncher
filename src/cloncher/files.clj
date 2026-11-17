(ns cloncher.files
  "Freshness-ranked file snapshots. Enumeration and matching run off the UI thread."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.persistence :as disk]
            [cloncher.process :as p]
            [cloncher.search :as search]))

(def ^:private file-exclusions
  [".git" ".cache" "node_modules" "target" ".direnv" ".venv" ".local/share/Trash"
   ".password-store" ".gnupg" ".ssh" ".local/share/containers" ".local/share/opencode"
   ".local/share/pnpm" ".local/share/Steam" ".local/share/uv" ".local/share/postgres"
   ".local/share/umu" ".bun/install/cache" ".cargo/registry" ".cargo/git"
   "go/pkg/mod" ".pub-cache" ".zoom" ".config/Notion/Partitions" "kernel-venv"
   "__pycache__" "Cache" "Code Cache" "GPUCache" ".npm/_cacache" ".gradle/caches"
   ".local/share/lutris"])

(def ^:private maximum 80)
(def ^:private lifetime-ms (* 5 60 1000))
(def ^:private personal-prefixes ["Downloads/" "Videos/" "Screenshots/"])
(defonce ^:private indexes (atom {}))

(defn- relative-path [path]
  (when (and (not (str/blank? path))
             (not (str/starts-with? path "/"))
             (not= path "..")
             (not (str/starts-with? path "../"))
             (not (str/includes? path "/../"))
             (not (str/ends-with? path "/..")))
    (if (str/starts-with? path "./") (subs path 2) path)))

(defn parse-snapshot [^String text]
  (let [length (count text)
        delimiter (int \tab)]
    (loop [start 0 tab (.indexOf text delimiter 0) entries (transient [])]
      (if (and (< start length) (not (neg? tab)))
        (let [separator (.indexOf text 0 start)
              end (if (neg? separator) length separator)
              path (when (<= start tab (dec end)) (relative-path (subs text (inc tab) end)))]
          (recur (inc end)
                 (if (< tab end) (.indexOf text delimiter (inc end)) tab)
                 (if path
                   (let [split (.lastIndexOf ^String path (int \/))
                         folded (str/lower-case path)]
                     (conj! entries {:path path :name-start (inc split)
                                     :folded (if (= path folded) path folded)
                                     :preferred? (some #(str/starts-with? path %) personal-prefixes)
                                     :hidden? (or (str/starts-with? path ".") (str/includes? path "/."))
                                     :modified (or (parse-long (subs text start tab)) 0)}))
                   entries)))
        (persistent! entries)))))

(defn- enumerate [root target]
  (let [argv (into ["timeout" "90" "fd" "--hidden" "--type" "f" "--type" "l"
                    "--threads" "2" "--color" "never" "--base-directory" root]
                   (concat (mapcat #(vector "--exclude" %) file-exclusions)
                           ["--exec-batch" "stat" "--printf=%Y\\t%n\\0" "--" "{}"]))
        options (if target {:out (str target) :continue true} {:continue true})
        {:keys [exit out err]} (apply p/sh options argv)]
    (when-not (zero? exit)
      (throw (ex-info (str "Could not index files: " (str/trim err)) {:root root})))
    (if target (slurp (str target)) out)))

(defn- refresh-home [root snapshot]
  (let [parent (fs/parent snapshot)]
    (fs/create-dirs parent)
    (let [tmp (fs/create-temp-file {:dir parent :prefix ".files-"})]
      (try
        (fs/set-posix-file-permissions tmp "rw-------")
        (let [index (parse-snapshot (enumerate root tmp))]
          (fs/move tmp snapshot {:atomic-move true :replace-existing true})
          index)
        (finally (fs/delete-if-exists tmp))))))

(defn- stale? [time]
  (>= (- (System/currentTimeMillis) time) lifetime-ms))

(defn- file-time [path]
  (if (fs/regular-file? path)
    (.toMillis (fs/last-modified-time path))
    0))

(defn- fuzzy-position [term text]
  (loop [from 0 i 0 first-hit -1]
    (if (= i (count term))
      (- 0 (- from first-hit))
      (let [hit (.indexOf ^String text (int (nth term i)) from)]
        (when (<= 0 hit)
          (recur (inc hit) (inc i) (if (neg? first-hit) hit first-hit)))))))

(defn- match-score [terms text]
  (loop [terms (seq terms) score 0]
    (if-let [term (first terms)]
      (when-let [hit (fuzzy-position term text)]
        (recur (next terms) (+ score hit)))
      score)))

(defn- scored [entry query personal? terms dot-prefixes show-normal?]
  (let [{:keys [path name-start modified hidden? folded preferred?]} entry]
    (if (empty? query)
      (when-not hidden?
        [(if (and personal? preferred?) -1 0)
         (- modified) path])
      (when-let [score (match-score terms folded)]
        (let [filename (subs folded (min name-start (count folded)))]
          (when (or (and show-normal? (not hidden?))
                    (some #{filename} terms)
                    (and (seq dot-prefixes)
                         (some (fn [part] (and (str/starts-with? part ".")
                                              (some #(str/starts-with? part %) dot-prefixes)))
                               (str/split folded #"/"))))
            (let [relevance (cond
                              (some #{filename} terms) 3
                              (some #(str/includes? filename %) terms) 2
                              (some #(fuzzy-position % filename) terms) 1
                              :else 0)]
              [(- relevance) (- modified) (- score) (count path) path])))))))

(defn rank [entries query personal?]
  (let [query (str/trim query)
        terms (re-seq #"\S+" (str/lower-case query))
        dot-prefixes (into [] (comp (mapcat #(str/split % #"/"))
                                    (filter #(and (str/starts-with? % ".") (not= % ".."))))
                           terms)
        show-normal? (or (empty? dot-prefixes) (some #{"."} dot-prefixes))]
    (->> entries
         (reduce (fn [top entry]
                   (if (.isInterrupted (Thread/currentThread))
                     (reduced top)
                     (if-let [score (scored entry query personal? terms dot-prefixes show-normal?)]
                       (if (or (< (count top) maximum) (neg? (compare score (first (rseq top)))))
                         (let [next (conj top score)]
                           (if (> (count next) maximum) (disj next (last next)) next))
                         top)
                       top)))
                 (sorted-set))
         (mapv peek))))

(defn- index-context [root]
  (let [personal? (nil? root)
        root (or root (p/getenv "HOME"))
        snapshot (when personal? (disk/cache "cloncher-files-v1.nul"))]
    {:root root :snapshot snapshot :personal? personal?
     :key [::file-index root (str snapshot)]}))

(defn- index-slot [key]
  (locking indexes
    (or (get @indexes key)
        (let [slot (atom {})]
          (swap! indexes #(into {key slot} (take 3 %)))
          slot))))

(defn- result-rows [root personal? paths]
  (mapv (fn [relative]
          (let [path (str (fs/path root relative))]
            {:label (str (fs/file-name path)) :icon "󰈔"
             :subtitle (str (when personal? "~/") (fs/parent relative))
             :row-id path :file path :hint "actions" :next {:kind :file :path path}}))
        paths))

(defn cached-recent
  "Return prepared rows without touching the filesystem or waiting for the index."
  [root]
  (some-> (get @indexes (:key (index-context root))) deref :recent))

(defn- seed-recent! [slot root]
  (let [{:keys [home paths]} (disk/read-json (disk/cache "cloncher-files-recent-v1.json") nil)]
    (when (and (= home root) (vector? paths))
      (let [rows (search/index-rows
                   (result-rows root true (into [] (comp (filter string?) (keep relative-path))
                                                   (take maximum paths))))]
        (swap! slot #(if (contains? % :recent) % (assoc % :recent rows)))))))

(declare ensure-index!)

(defn- load-index! [slot {:keys [root snapshot personal?] :as context} read-snapshot?]
  (try
    (let [disk-time (when (and snapshot read-snapshot?) (file-time snapshot))
          entries (cond
                    (and disk-time (pos? disk-time)) (parse-snapshot (slurp (str snapshot)))
                    snapshot (refresh-home root snapshot)
                    :else (parse-snapshot (enumerate root nil)))
          paths (rank entries "" personal?)
          recent (search/index-rows (result-rows root personal? paths))
          result {:entries entries :recent recent
                  :time (if (and disk-time (pos? disk-time)) disk-time (System/currentTimeMillis))}]
      (when personal?
        (try (disk/write-json! (disk/cache "cloncher-files-recent-v1.json") {:home root :paths paths})
             (catch Exception _ nil)))
      (locking slot (reset! slot result))
      (when (stale? (:time result))
        (ensure-index! slot context))
      result)
    (catch Exception error
      (locking slot
        (swap! slot #(-> % (dissoc :job) (assoc :error error :time (System/currentTimeMillis)))))
      nil)))

(defn- ensure-index! [slot {:keys [root personal?] :as context}]
  (locking slot
    (when (and personal? (not (contains? @slot :recent)))
      (seed-recent! slot root))
    (let [{:keys [entries job time]} @slot]
      (when (and (nil? job) (or (nil? time) (stale? time)))
        (swap! slot assoc :job (future (load-index! slot context (nil? entries))))))
    @slot))

(defn prime!
  "Load recent files and warm the home index in a worker."
  []
  (let [{:keys [key] :as context} (index-context nil)
        slot (index-slot key)
        {:keys [job]} (ensure-index! slot context)]
    (when job @job)
    nil))

(defn rows [root query]
  (let [{:keys [key personal?] :as context} (index-context root)
        slot (index-slot key)
        {:keys [job entries recent]} (ensure-index! slot context)]
    (if (str/blank? query)
      (or recent (:recent (when job @job))
          (when-let [error (:error @slot)] (throw error)) [])
      (let [entries (or entries (:entries (when job @job)))]
        (when (nil? entries)
          (when-let [error (:error @slot)] (throw error)))
        (search/index-rows (result-rows (:root context) personal? (rank entries query personal?)))))))
