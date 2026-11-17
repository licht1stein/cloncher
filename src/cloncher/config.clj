(ns cloncher.config
  "Locate user configuration and seed the packaged defaults on first launch."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cloncher.process :as p]))

(defmacro bundled-config []
  (slurp (or (io/resource "cloncher/default.edn")
             (io/file "resources/cloncher/default.edn"))))

(def default-text (bundled-config))

(defn path []
  (let [xdg (p/getenv "XDG_CONFIG_HOME")
        directory (if (and xdg (fs/absolute? xdg))
                    xdg
                    (fs/path (or (p/getenv "HOME") (fs/home)) ".config"))]
    (fs/path directory "cloncher" "config.edn")))

(defn setup! [path]
  (when-not (fs/exists? path)
    (let [directory (fs/parent path)]
      (fs/create-dirs directory)
      (let [pending (fs/create-temp-file {:dir directory :prefix ".config-" :suffix ".edn"})]
        (try
          (spit (str pending) default-text)
          ;; Publishing a completed inode without replacement also handles simultaneous launches.
          (try
            (java.nio.file.Files/createLink path pending)
            (catch java.nio.file.FileAlreadyExistsException _ nil))
          (finally (fs/delete-if-exists pending))))))
  path)

(defn read-config [text]
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [eof (Object.)
          options {:eof eof}
          config (edn/read options reader)]
      (when-not (and (map? config) (identical? eof (edn/read options reader)))
        (throw (ex-info "Expected one EDN configuration map" {})))
      config)))
