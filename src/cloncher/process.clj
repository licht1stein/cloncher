(ns cloncher.process
  "Process calls that preserve the requesting desktop's environment."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]))

(def ^:dynamic *environment* nil)

(defn environment []
  (or *environment* (System/getenv)))

(defn getenv [k]
  (get (environment) k))

(defn- invoke [f args]
  (if *environment*
    (let [[options args] (if (map? (first args)) [(first args) (rest args)] [{} args])]
      (apply f (merge {:env *environment*} options) args))
    (apply f args)))

(defn sh [& args] (invoke process/sh args))
(defn shell [& args] (invoke process/shell args))
(defn process [& args] (invoke process/process args))
(def destroy-tree process/destroy-tree)

(defn- self-command
  "The argv that runs this Cloncher build again: the release binary, or Jolt with the source tree."
  []
  (let [exe (.getCanonicalFile (io/file "/proc/self/exe"))]
    (if (= "jolt" (.getName exe))
      (let [resources (.getParentFile (.getParentFile (io/file (io/resource "cloncher/default.edn"))))
            source (io/file (.getParentFile resources) "src")]
        [(str exe) "-Srepro" "-Scp" (str source ":" resources) "-m" "cloncher.main"])
      [(str exe)])))

(defn start-helper!
  "Start this build with `args` as a separate window process. Return its first
  stdout line, or nil when it says nothing within `timeout-ms` (it is then stopped)."
  [args timeout-ms]
  (let [child (apply process
                     {:in :close :out :stream :err :discard
                      :env (merge (environment)
                                  (select-keys (System/getenv) ["CLONCHER_ROOT" "CLONCHER_CUI_LIBRARY"]))}
                     (into (self-command) args))
        reply (future
                (with-open [stream (io/reader (:out child))]
                  (.readLine stream)))
        line (deref reply timeout-ms ::timeout)]
    (if (= ::timeout line)
      (do (.destroy (:proc child)) nil)
      line)))
