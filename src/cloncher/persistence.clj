(ns cloncher.persistence
  (:require [babashka.fs :as fs]
            [cloncher.json :as json]
            [cloncher.process :as p]))

(defn cache [& parts]
  (apply fs/path (or (p/getenv "XDG_CACHE_HOME")
                     (str (p/getenv "HOME") "/.cache")) "launcher" parts))

(defn read-json
  ([path fallback] (read-json path fallback true))
  ([path fallback keywordize?]
   (try (json/parse-string (slurp (str path)) keywordize?)
        (catch Exception _ fallback))))

(defn write-json! [file value]
  (let [parent (fs/parent file)]
    (fs/create-dirs parent)
    (let [temporary (fs/create-temp-file {:dir parent :prefix ".cloncher-"})]
      (try
        (fs/set-posix-file-permissions temporary "rw-------")
        (spit (str temporary) (json/generate-string value))
        (fs/move temporary file {:atomic-move true :replace-existing true})
        (finally (fs/delete-if-exists temporary))))))
