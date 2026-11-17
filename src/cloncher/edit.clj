(ns cloncher.edit
  "Pair delimiters in the desktop expression field after a single-character edit.")

(def ^:private closing {\( \) \[ \] \{ \} \" \"})

(defn- context [text]
  (loop [i 0 mode :code escaped? false stack []]
    (if (= i (count text))
      {:mode mode :escaped? escaped? :stack stack}
      (let [character (nth text i)]
        (case mode
          :comment (recur (inc i) (if (= character \newline) :code :comment) false stack)
          :character (recur (inc i) (if (Character/isLetterOrDigit character) :character :code) false stack)
          :string (cond
                    escaped? (recur (inc i) :string false stack)
                    (= character \\) (recur (inc i) :string true stack)
                    (= character \") (recur (inc i) :code false stack)
                    :else (recur (inc i) :string false stack))
          :code (cond
                  (= character \;) (recur (inc i) :comment false stack)
                  (= character \") (recur (inc i) :string false stack)
                  (= character \\)
                  (if (< (inc i) (count text))
                    (recur (+ i 2) (if (Character/isLetter (nth text (inc i))) :character :code) false stack)
                    {:mode :character :stack stack})
                  (#{\( \[ \{} character) (recur (inc i) :code false (conj stack character))
                  (and (seq stack) (= character (get closing (peek stack))))
                  (recur (inc i) :code false (pop stack))
                  :else (recur (inc i) :code false stack)))))))

(defn paired-edit [old typed caret]
  (when (some? caret)
    (let [delta (- (count typed) (count old))]
      (cond
        (and (= 1 delta) (pos? caret)
             (= old (str (subs typed 0 (dec caret)) (subs typed caret))))
        (let [character (nth typed (dec caret))
              next (get typed caret)
              {:keys [mode escaped? stack]} (context (subs old 0 (dec caret)))]
          (cond
            (and (= next character)
                 (or (and (= mode :string) (= character \") (not escaped?))
                     (and (= mode :code) (= character (get closing (peek stack))))))
            {:text old :caret caret}
            (and (= mode :code) (closing character))
            {:text (str (subs typed 0 caret) (closing character) (subs typed caret)) :caret caret}))

        (and (= -1 delta) (< caret (count typed))
             (= typed (str (subs old 0 caret) (subs old (inc caret)))))
        (let [before (get old caret)
              after (get typed caret)]
          (when (and (= after (closing before)) (= :code (:mode (context (subs old 0 caret)))))
            {:text (str (subs typed 0 caret) (subs typed (inc caret))) :caret caret}))))))
