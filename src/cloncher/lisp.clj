(ns cloncher.lisp
  "Bounded interpreter for desktop queries. Only selecting a row can focus a window."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.desktop :as desktop]
            [cloncher.json :as json]))

(defn query? [q] (str/starts-with? (str/trim q) "("))

(def ^:private builtins
  {'= = 'not= not= '< < '<= <= '> > '>= >= '+ + '- - '* * '/ /
   'first first 'second second 'last last 'nth nth 'get get 'get-in get-in
   'keys keys 'vals vals 'count count 'empty? empty? 'seq seq 'not not
   'filter filter 'remove remove 'map map 'mapv mapv 'keep keep 'take take
   'drop drop 'take-while take-while 'drop-while drop-while 'sort-by sort-by
   'reverse reverse 'vec vec 'distinct distinct 'some some 'every? every?
   'contains? contains? 'identity identity 'comp comp 'partial partial 'juxt juxt
   'inc inc 'dec dec 'str str 'string? string? 'number? number? 'nil? nil? 'boolean boolean
   'str/includes? str/includes? 'str/starts-with? str/starts-with?
   'str/ends-with? str/ends-with? 'str/lower-case str/lower-case
   'clojure.string/includes? str/includes? 'clojure.string/starts-with? str/starts-with?
   'clojure.string/ends-with? str/ends-with? 'clojure.string/lower-case str/lower-case})

(defn- parse [source]
  (when (> (count source) 4096)
    (throw (ex-info "Expression exceeds 4096 characters" {})))
  (binding [*read-eval* false *data-readers* {}]
    (with-open [reader (java.io.PushbackReader. (java.io.StringReader. source))]
      (let [eof (Object.)]
        (loop [forms []]
          (let [form (read {:eof eof} reader)]
            (if (identical? form eof) forms (recur (conj forms form)))))))))

(defn- bounded [value]
  (cond
    (seq? value)
    (let [items (vec (take 201 value))]
      (when (> (count items) 200)
        (throw (ex-info "Expression returned more than 200 items" {})))
      (mapv bounded items))

    (and (vector? value) (> (count value) 200))
    (throw (ex-info "Expression returned more than 200 items" {}))

    (and (string? value) (> (count value) 16384))
    (throw (ex-info "Expression produced too much text" {}))

    :else value))

(defn evaluate [source]
  (let [forms (parse source)
        steps (volatile! 0)
        windows (delay (vec (take 200 (desktop/list-windows))))
        environment (assoc builtins 'list-windows (fn [] @windows) 'focus desktop/focus)]
    (letfn [(body [env depth forms]
              (reduce (fn [_ form] (eval-form env depth form)) nil forms))
            (eval-form [env depth form]
              (when (or (> depth 64) (> (vswap! steps inc) 2000))
                (throw (ex-info "Expression exceeded its evaluation budget" {})))
              (let [child (inc depth)]
                (bounded
                 (cond
                   (symbol? form)
                   (if (contains? env form) (get env form)
                       (throw (ex-info (str "Unknown name " form) {})))

                   (vector? form) (mapv #(eval-form env child %) form)
                   (map? form) (into {} (map (fn [[k v]] [(eval-form env child k) (eval-form env child v)])) form)
                   (set? form) (into #{} (map #(eval-form env child %)) form)

                   (list? form)
                   (let [[op a b c & _] form
                         val #(eval-form env child %)]
                     (cond
                       (empty? form) []
                       (= op 'if) (val (if (val a) b c))
                       (contains? #{'when 'when-not} op)
                       (when (= (= op 'when) (boolean (val a))) (body env child (nnext form)))
                       (= op 'and) (reduce (fn [v x] (if v (val x) (reduced v))) true (rest form))
                       (= op 'or) (reduce (fn [v x] (if v (reduced v) (val x))) nil (rest form))
                       (contains? #{'let 'let*} op)
                       (do
                         (when-not (and (vector? a) (even? (count a)) (every? symbol? (take-nth 2 a)))
                           (throw (ex-info "let expects symbol/value pairs" {})))
                         (let [local (reduce (fn [e [name expr]] (assoc e name (eval-form e child expr)))
                                             env (partition 2 a))]
                           (body local child (nnext form))))
                       (contains? #{'fn 'fn*} op)
                       (do
                         (when-not (and (vector? a) (every? symbol? a) (not-any? #{'&} a))
                           (throw (ex-info "fn expects a fixed vector of argument symbols" {})))
                         (fn [& args]
                           (when-not (= (count a) (count args))
                             (throw (ex-info "Wrong number of arguments" {})))
                           (body (into env (map vector a args)) child (nnext form))))
                       (contains? #{'-> '->> 'some-> 'some->>} op)
                       (reduce (fn [value x]
                                 (if (and (nil? value) (contains? #{'some-> 'some->>} op))
                                   (reduced nil)
                                   (let [parts (if (seq? x) (rest x) [])
                                         thread-tail? (contains? #{'->> 'some->>} op)
                                         args (mapv val parts)
                                         f (val (if (seq? x) (first x) x))]
                                     (bounded (apply f (if thread-tail? (conj args value) (cons value args)))))))
                               (val a) (nnext form))
                       :else (apply (val op) (mapv val (rest form)))))

                   (or (nil? form) (boolean? form) (number? form) (string? form)
                       (keyword? form) (char? form))
                   form

                   :else (throw (ex-info "Unsupported literal" {}))))))]
      (body environment 0 forms))))

(def ^:private examples
  [["List windows" "(list-windows)"]
   ["Focus the previous window" "(-> (list-windows) first focus)"]
   ["Filter windows by app" "(->> (list-windows) (filter #(= \"kitty\" (:app-id %))))"]])

(defn- history-file []
  (fs/path (fs/xdg-cache-home) "launcher" "lisp.json"))

(defn- history []
  (try (vec (json/parse-string (slurp (str (history-file))) true))
       (catch Exception _ [])))

(defn- summary [value]
  (let [text (binding [*print-length* 12 *print-level* 4] (pr-str value))]
    (subs text 0 (min 500 (count text)))))

(defn save! [{:keys [expr result]}]
  (let [file (history-file)
        parent (fs/parent file)
        entries (into [{:expr expr :result result :at (quot (System/currentTimeMillis) 1000)}]
                      (take 199 (remove #(= expr (:expr %)) (history))))]
    (fs/create-dirs parent)
    (let [temporary (fs/create-temp-file {:dir parent :prefix "cloncher-lisp-"})]
      (try
        (spit (str temporary) (json/generate-string entries))
        (fs/move temporary file {:replace-existing true :atomic-move true})
        (finally (when (fs/exists? temporary) (fs/delete temporary)))))))

(defn- value-row [value]
  (case (when (map? value) (:type value))
    :window (desktop/window-item value)
    :focus (assoc (desktop/window-item (:window value))
                  :label (str "Focus " (or (get-in value [:window :title]) "window")))
    {:label (summary value) :subtitle "Value"}))

(defn rows [query]
  (let [q (str/trim query)
        help? (contains? #{"(" "(help)"} q)
        past (into [] (comp (filter #(str/includes? (:expr %) (if help? "(" q)))
                            (take 24)
                            (map (fn [{:keys [expr result]}]
                                   {:icon "󰋚" :label expr :subtitle result :fill expr :hint "edit"})))
                   (history))]
    (if help?
      (into past (map (fn [[label expr]] {:label label :subtitle expr :fill expr})) examples)
      (try
        (let [value (evaluate q)
              values (if (sequential? value) value [value])]
          (into (if (seq values)
                  (mapv #(assoc (value-row %) :lisp {:expr q :result (summary value)}) values)
                  [{:label "No results"}])
                (remove #(= q (:fill %)) past)))
        (catch Exception error
          (into [{:label (if (re-find #"(?i)EOF|end of (?:input|string)" (ex-message error))
                           "Complete the expression to evaluate" "Evaluation failed")
                  :subtitle (ex-message error)}] past))))))
