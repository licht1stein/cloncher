(ns cloncher.files
  "Freshness-ranked file snapshots. Enumeration and matching run off the UI thread."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.persistence :as disk]
            [cloncher.process :as p]))

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
(def ^:private personal-folders #{"Downloads" "Videos" "Screenshots"})
(defonce ^:private indexes (atom {}))

(defn- relative-path [path]
  (when (and (not (str/blank? path))
             (not (str/starts-with? path "/"))
             (not (some #{".."} (str/split path #"/"))))
    (str/replace-first path #"^\./" "")))

(defn parse-snapshot [text]
  (into []
        (keep (fn [record]
                (when-let [end (str/index-of record "\t")]
                  (when-let [path (relative-path (subs record (inc end)))]
                    (let [split (str/last-index-of path "/")]
                      {:path path :name-start (if split (inc split) 0)
                       :hidden? (boolean (re-find #"(?:^|/)\." path))
                       :modified (or (parse-long (subs record 0 end)) 0)})))))
        (str/split text #"\u0000")))

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

(defn- current-index [root personal?]
  (let [snapshot (when personal? (disk/cache "cloncher-files-v1.nul"))
        key [root (str snapshot)]
        slot (locking indexes
               (or (get @indexes key)
                   (let [slot (atom nil)]
                     (swap! indexes
                       (fn [m] (into {key slot} (take 3 m))))
                     slot)))]
    (locking slot
      (let [{:keys [time rows job]} @slot
            disk-time (when snapshot (file-time snapshot))
            read-disk #(parse-snapshot (slurp (str snapshot)))]
        (cond
          (and rows (not (stale? time))) rows
          (and job (not (realized? job))) rows
          (and (nil? rows) snapshot (pos? disk-time) (not (stale? disk-time)))
          (let [rows (read-disk)] (reset! slot {:time disk-time :rows rows}) rows)
          (and snapshot (or rows (pos? disk-time)))
          (let [rows (or rows (read-disk))
                job (future
                      (try
                        (let [updated (refresh-home root snapshot)]
                          (swap! slot assoc :time (System/currentTimeMillis) :rows updated))
                        (catch Exception _
                          (swap! slot assoc :time (System/currentTimeMillis)))))]
            (swap! slot assoc :time (System/currentTimeMillis) :rows rows :job job)
            rows)
          :else
          (let [rows (if snapshot (refresh-home root snapshot) (parse-snapshot (enumerate root nil)))]
            (reset! slot {:time (System/currentTimeMillis) :rows rows})
            rows))))))

(defn- fuzzy-position [term text]
  (loop [from 0 i 0 first-hit -1]
    (if (= i (count term))
      (- 0 (- from first-hit))
      (let [hit (.indexOf ^String text (int (nth term i)) from)]
        (when (<= 0 hit)
          (recur (inc hit) (inc i) (if (neg? first-hit) hit first-hit)))))))

(defn- hidden-visible? [path name terms hidden? dot-prefixes]
  (or (and (not hidden?) (or (empty? dot-prefixes) (some #{"."} dot-prefixes)))
      (and (seq terms) (some #(= (str/lower-case name) %) terms))
      (and (seq dot-prefixes)
           (some (fn [part] (some #(str/starts-with? part %) dot-prefixes))
                 (filter #(str/starts-with? % ".") (str/split path #"/"))))))

(defn- scored [entry query personal? terms dot-prefixes]
  (let [{:keys [path name-start modified hidden?]} entry]
    (if (empty? query)
      (when-not hidden?
        [(- (if (and personal? (personal-folders (first (str/split path #"/" 2)))) 1 0))
         (- modified) path])
      (let [folded (str/lower-case path)
            filename (subs folded (min name-start (count folded)))]
        (when (hidden-visible? folded filename terms hidden? dot-prefixes)
          (let [hits (mapv #(fuzzy-position % folded) terms)]
            (when (every? some? hits)
              (let [relevance (cond
                                (some #{filename} terms) 3
                                (some #(str/includes? filename %) terms) 2
                                (some #(fuzzy-position % filename) terms) 1
                                :else 0)]
                [(- relevance) (- modified) (- (reduce + hits)) (count path) path]))))))))

(defn rank [entries query personal?]
  (let [query (str/trim query)
        terms (re-seq #"\S+" (str/lower-case query))
        dot-prefixes (into [] (comp (mapcat #(str/split % #"/"))
                                    (filter #(and (str/starts-with? % ".") (not= % ".."))))
                           terms)]
    (->> entries
         (reduce (fn [top entry]
                   (if (.isInterrupted (Thread/currentThread))
                     (reduced top)
                     (if-let [score (scored entry query personal? terms dot-prefixes)]
                       (if (or (< (count top) maximum) (neg? (compare score (last top))))
                         (let [next (conj top score)]
                           (if (> (count next) maximum) (disj next (last next)) next))
                         top)
                       top)))
                 (sorted-set))
         (mapv peek))))

(defn rows [root query]
  (let [personal? (nil? root)
        root (or root (p/getenv "HOME"))]
    (mapv (fn [relative]
            (let [path (str (fs/path root relative))]
              {:label (str (fs/file-name path)) :icon "󰈔"
               :subtitle (str (when personal? "~/") (fs/parent relative))
               :row-id path :file path :hint "actions" :next {:kind :file :path path}}))
          (rank (current-index root personal?) query personal?))))
