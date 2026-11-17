(ns cloncher.process
  "Process calls that preserve the requesting desktop's environment."
  (:require [babashka.process :as process]))

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
