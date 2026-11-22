(ns cloncher.search
  (:require [clojure.string :as str]))

(def ^:private word-breaks #{\- \_ \.})

(defn- fuzzy-score [query word]
  (if (str/starts-with? word query)
    (let [n (count query)]
      (+ 73 (* 10 n) (* 12 (dec n))
         (* 8 (count (filter word-breaks (take (dec n) query))))))
    (loop [chars (seq query) from 0 previous -1 score 0]
      (if-let [ch (first chars)]
        (let [i (.indexOf ^String word (int ch) from)]
          (when (<= 0 i)
            (recur (next chars) (inc i) i
                   (+ score 10
                      (if (or (zero? i) (word-breaks (nth word (dec i)))) 8 0)
                      (cond (= previous -1) 0
                            (= i (inc previous)) 12
                            :else (- (min 8 (- i previous))))))))
        score))))

(defn tokens [text]
  (re-seq #"[^\s]+" (str/lower-case text)))

(defn- alphabet-mask [text]
  (reduce (fn [mask ch]
            (let [i (- (int ch) (int \a))]
              (if (<= 0 i 25) (bit-set mask i) mask))) 0 text))

(defn- prepare [row]
  (let [{:keys [label subtitle path hint search]} row
        title (str/lower-case label)
        hay (str/lower-case (str label " " subtitle " " path " " hint " " search))
        words (vec (re-seq #"[^\s_:/›()\[\],]+" hay))]
    {:title title :hay hay :words (mapv #(vector % (alphabet-mask %)) words)
     :initials (apply str (map first words)) :mask (alphabet-mask hay)}))

(defn prepare-rows [rows]
  (mapv #(cond-> % (nil? (::search-data %)) (assoc ::search-data (prepare %))) rows))

(defn- within-word [token mask words]
  (loop [i 0 best -10000]
    (if-let [[word word-mask] (get words i)]
      (let [score (when (= mask (bit-and mask word-mask)) (fuzzy-score token word))]
        (recur (inc i) (if score (max best (- score (min i 10))) best)))
      best)))

(defn- score [query tokens mask {:keys [shortcut] :as row}]
  (if (= shortcut query)
    10000
    (let [{:keys [title hay words initials] :as data} (or (::search-data row) (prepare row))]
      (when (= mask (bit-and mask (:mask data)))
        (loop [remaining tokens total (if (str/starts-with? title query) 60 0)]
          (if-let [[token token-mask] (first remaining)]
            (let [within (within-word token token-mask words)
                  across (when (> (count token) 1) (fuzzy-score token initials))
                  best (max within (if across (- across 15) -10000))]
              (when (> best -10000)
                (recur (next remaining) (+ total best (if (str/includes? hay token) 20 0)))))
            total))))))

(defn rank
  ([query rows] (rank query rows {}))
  ([query rows usage]
  (let [used #(min 30 (get usage (:row-id %) 0))
        query (str/lower-case (str/trim query))
        tokens (mapv #(vector % (alphabet-mask %)) (tokens query))
        mask (alphabet-mask query)]
    (if (empty? tokens)
      (->> rows (remove :deep) (sort-by (juxt #(if (:live %) 0 1) #(or (:section %) 0) #(or (:order %) 0)
                                  #(if (:app? %) (- (used %)) 0))) vec)
      (->> rows
           (keep (fn [row] (when-let [n (score query tokens mask row)] [row n])))
           (sort-by (fn [[row n]]
                      [(if (= (:shortcut row) query) 0 1)
                       (if (:window row) 0 1) (if (:live row) 0 1) (if (:app? row) 1 0)
                       (or (:section row) 0) (or (:order row) 0) (- (+ n (* 4 (used row))))]))
           (mapv first))))))

(defn emoji? [text]
  (and (seq text)
       (<= (count text) 12)
       (not (re-find #"[\p{L}\s]" text))
       (some #(or (<= 0x2190 % 0x2bff) (<= 0x1f000 % 0x1faff))
             (.toArray (.codePoints ^String text)))))

(defn line-row [line]
  (let [[head subtitle] (str/split line #"\t" 2)
        head (str/trim head)
        codepoint (when (seq head) (.codePointAt ^String head 0))
        nerd? (and codepoint (or (<= 0xe000 codepoint 0xf8ff) (<= 0xf0000 codepoint 0x10ffff)))
        [glyph label] (if nerd?
                        [(subs head 0 1) (str/trim (subs head 1))]
                        (let [[candidate label] (str/split head #" {2,}" 2)]
                          (if (and label (emoji? candidate)) [candidate label] [nil head])))]
    {:label label :icon glyph :subtitle (some-> subtitle str/trim) :value line}))

(def ^:private home-keys "asdfghjkl")

(defn- codes [letters width]
  (if (zero? width) [""]
      (for [letter letters suffix (codes letters (dec width))] (str letter suffix))))

(defn- prefixes [code]
  (map #(subs code 0 %) (range 1 (inc (count code)))))

(defn- conflicts? [used prefixes-used code]
  (or (prefixes-used code) (some used (prefixes code))))

(defn label-rows [rows]
  (let [[fixed used prefixes-used]
        (reduce (fn [[fixed used prefixes-used] [index row]]
                  (let [hint (some-> (:shortcut row) str/lower-case)]
                    (if (and (seq hint) (not (conflicts? used prefixes-used hint)))
                      [(assoc fixed index hint) (conj used hint) (into prefixes-used (prefixes hint))]
                      [fixed used prefixes-used])))
                [{} #{} #{}] (map-indexed vector rows))
        needed (- (count rows) (count fixed))
        letters (if (every? used (map str home-keys))
                  (remove used (map str "abcdefghijklmnopqrstuvwxyz0123456789"))
                  home-keys)
        generated (when (and (pos? needed) (seq letters))
                    (loop [width 1]
                      (let [available (->> (codes letters width)
                                           (remove #(conflicts? used prefixes-used %))
                                           (take needed) vec)]
                        (cond (= needed (count available)) available
                              (>= width 5) available
                              :else (recur (inc width))))))]
    (loop [result [] rows (seq rows) pool (seq generated)]
      (if-let [row (first rows)]
        (if-let [code (get fixed (count result))]
          (recur (conj result (assoc row :shortcut code)) (next rows) pool)
          (recur (conj result (assoc row :shortcut (first pool))) (next rows) (next pool)))
        result))))

(defn index-rows [rows]
  (prepare-rows (label-rows rows)))

(defn jump [rows prefix]
  (let [fits? #(str/starts-with? (or (:shortcut %) "") prefix)
        matches (keep-indexed (fn [i row] (when (fits? row) [i row])) rows)
        exact (some (fn [[i row]] (when (= prefix (:shortcut row)) i)) matches)]
    {:exact exact :first (ffirst matches)}))
