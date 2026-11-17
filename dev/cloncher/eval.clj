(ns cloncher.eval
  "Command-line client for the running Cloncher development REPL."
  (:require [clojure.string :as str]))

(def usage "Usage: jolt eval [desktop] [--ns NAMESPACE] -e EXPRESSION")

(defn- write-string! [out text]
  (let [bytes (.getBytes ^String text "UTF-8")]
    (.write out (.getBytes (str (alength bytes) ":") "UTF-8"))
    (.write out bytes)))

(defn- send-request! [out request]
  (.write out (int \d))
  (doseq [[key value] (sort-by first request)]
    (write-string! out key)
    (write-string! out value))
  (.write out (int \e))
  (.flush out))

(defn- read-byte! [in]
  (let [value (.read in)]
    (when (neg? value) (throw (ex-info "nREPL closed the connection" {})))
    value))

(defn- read-number! [in first-byte delimiter]
  (loop [text (if first-byte (str (char first-byte)) "")]
    (let [byte (read-byte! in)]
      (if (= byte (int delimiter))
        (or (parse-long text) (throw (ex-info "Invalid nREPL number" {})))
        (do
          (when (>= (count text) 20) (throw (ex-info "Invalid nREPL number" {})))
          (recur (str text (char byte))))))))

(defn- read-value! [in byte depth]
  (when (> depth 24)
    (throw (ex-info "nREPL response is too deeply nested" {})))
  (if (<= (int \0) byte (int \9))
    (let [length (read-number! in byte \:)]
      (when-not (<= 0 length (* 8 1024 1024))
        (throw (ex-info "nREPL response is too large" {})))
      (let [bytes (byte-array length)]
        (loop [offset 0]
          (when (< offset length)
            (let [n (.read in bytes (int offset) (int (- length offset)))]
              (when (neg? n) (throw (ex-info "nREPL closed the connection" {})))
              (recur (+ offset n)))))
        (String. bytes "UTF-8")))
    (case (char byte)
      \i (read-number! in nil \e)
      \l (loop [items []]
           (let [next (read-byte! in)]
             (if (= next (int \e))
               items
               (recur (conj items (read-value! in next (inc depth)))))))
      \d (loop [entries {}]
           (let [next (read-byte! in)]
             (if (= next (int \e))
               entries
               (let [key (read-value! in next (inc depth))
                     value (read-value! in (read-byte! in) (inc depth))]
                 (recur (assoc entries key value))))))
      (throw (ex-info "Invalid nREPL response" {:byte byte})))))

(defn- evaluate! [port namespace code]
  (with-open [socket (java.net.Socket.)]
    (.connect socket (java.net.InetSocketAddress. "127.0.0.1" (int port)) 2000)
    (.setSoTimeout socket 8000)
    (let [in (java.io.BufferedInputStream. (.getInputStream socket))]
      (send-request! (.getOutputStream socket) {"op" "eval" "ns" namespace "code" code})
      (loop [failed? false]
        (let [reply (read-value! in (read-byte! in) 0)
              status (set (get reply "status"))
              failed? (or failed? (some status ["error" "eval-error" "namespace-not-found"]))]
          (when-let [out (get reply "out")] (print out) (flush))
          (when-let [error (get reply "err")] (binding [*out* *err*] (print error) (flush)))
          (when (contains? reply "value") (println (get reply "value")))
          (if (status "done") (if failed? 1 0) (recur failed?)))))))

(defn- options [args]
  (loop [[option value & rest] (if (= "desktop" (first args)) (next args) args)
         parsed {:namespace "user"}]
    (case option
      "--ns" (if value (recur rest (assoc parsed :namespace value)) (throw (ex-info usage {})))
      "-e" (if (and value (empty? rest)) (assoc parsed :code value) (throw (ex-info usage {})))
      (nil "--help" "help") nil
      (throw (ex-info usage {})))))

(defn -main [& args]
  (System/exit
    (try
      (if-let [{:keys [namespace code]} (options args)]
        (let [port (some-> (slurp ".nrepl-port") str/trim parse-long)]
          (when-not (and port (<= 1 port 65535))
            (throw (ex-info "Invalid .nrepl-port. Start jolt dev first." {})))
          (evaluate! port namespace code))
        (do (println usage) 0))
      (catch java.io.FileNotFoundException _
        (binding [*out* *err*] (println "Start jolt dev first."))
        1)
      (catch Exception error
        (binding [*out* *err*] (println "jolt eval:" (ex-message error)))
        1))))
