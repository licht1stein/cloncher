(ns cloncher.search
  (:require [clojure.string :as str]))

(defn- fuzzy-score [query word]
  (loop [chars (seq query) from 0 previous -1 score 0]
    (if-let [ch (first chars)]
      (let [i (.indexOf ^String word (int ch) from)]
        (when (<= 0 i)
          (recur (next chars) (inc i) i
                 (+ score 10
                    (if (or (zero? i) (contains? #{\- \_ \.} (nth word (dec i)))) 8 0)
                    (cond (= previous -1) 0
                          (= i (inc previous)) 12
                          :else (- (min 8 (- i previous))))))))
      (+ score (if (str/starts-with? word query) 65 0)))))

(defn tokens [text]
  (re-seq #"[^\s]+" (str/lower-case text)))

(defn- score [query tokens {:keys [label subtitle path shortcut]}]
  (let [title (str/lower-case label)
        hay (str/lower-case (str label " " subtitle " " path))
        words (re-seq #"[^\s_:/›()\[\],]+" hay)
        initials (apply str (map first words))]
    (if (= shortcut query)
      10000
      (loop [remaining tokens total (if (str/starts-with? title query) 60 0)]
        (if-let [token (first remaining)]
          (let [within (reduce max -10000
                               (keep-indexed (fn [index word]
                                               (when-let [n (fuzzy-score token word)]
                                                 (- n (min index 10))))
                                             words))
                across (when (> (count token) 1) (fuzzy-score token initials))
                best (max within (if across (- across 15) -10000))]
            (when (> best -10000)
              (recur (next remaining) (+ total best (if (str/includes? hay token) 20 0)))))
          total)))))

(defn rank [query rows]
  (let [query (str/lower-case (str/trim query))
        tokens (tokens query)]
    (if (empty? tokens)
      (->> rows (remove :deep) (sort-by (juxt #(if (:live %) 0 1) #(or (:section %) 0) #(or (:order %) 0))) vec)
      (->> rows
           (keep (fn [row] (when-let [n (score query tokens row)] [row n])))
           (sort-by (fn [[row n]]
                      [(if (= (:shortcut row) query) 0 1)
                       (if (:window row) 0 1) (if (:live row) 0 1)
                       (or (:section row) 0) (or (:order row) 0) (- n)]))
           (mapv first)))))

(defn line-row [line]
  (let [[head subtitle] (str/split line #"\t" 2)
        [_ glyph label] (re-matches #"^(\S+) {2,}(.+)$" head)]
    {:label (or label head) :icon glyph :subtitle subtitle :value line}))
