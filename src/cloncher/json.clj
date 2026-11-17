(ns cloncher.json
  (:require [clojure.data.json :as json]))

(defn parse-string
  ([s] (parse-string s false))
  ([s keywordize?] (json/read-str s :key-fn (if keywordize? keyword identity))))

(defn generate-string [value]
  (json/write-str value))
