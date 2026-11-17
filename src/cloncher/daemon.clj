(ns cloncher.daemon
  "Resident launcher request routing. Native windows run on the owning thread."
  (:require [cloncher.process :as process]
            [cloncher.socket :as socket]))

(defn- wake! [{:keys [redraw]}]
  (when-let [wake @redraw] (wake)))

(defn- cancel! [job]
  (when job
    (reset! (:cancelled job) true)
    (wake! job)))

(defn- finalize! [active job]
  (locking active
    (when (identical? job @active) (reset! active nil))
    (when-let [request (first (swap-vals! (:capture job) (constantly nil)))]
      (deliver (:reply request) {:exit 3}))))

(defn- capture! [active path]
  (let [reply (promise)
        job (locking active
              (let [job @active]
                (cond
                  (not (and (string? path) (seq path))) (deliver reply {:exit 2})
                  (or (nil? job) @(:cancelled job)) (deliver reply {:exit 3})
                  (= :secret (:mode job)) (deliver reply {:exit 1})
                  (not (compare-and-set! (:capture job) nil {:path path :reply reply}))
                  (deliver reply {:exit 1}))
                job))]
    (when (and job (not (realized? reply))) (wake! job))
    (deref reply 8000 {:exit 3})))

(defn- handle! [active queue parse fd]
  (try
    (when-let [{:keys [args lines env options]} (socket/read! fd 2000)]
      (let [reply (case (first args)
                    "ping" {:exit 0}
                    "close" (let [job (locking active (let [job @active] (cancel! job) job))]
                              (when job (deref (:reply job) 3000 nil))
                              {:exit 0})
                    "screenshot" (capture! active (second args))
                    (let [request (try (parse args) (catch Exception _ nil))]
                      (if-not (contains? #{:launcher :dmenu :secret} (:mode request))
                        {:exit 2}
                        (let [job (assoc request
                                         :allow-empty? (boolean (:allow-empty? options))
                                         :timeout-ms (when (pos-int? (:timeout-ms options))
                                                       (min 120000 (:timeout-ms options)))
                                         :reply (promise) :cancelled (atom false)
                                         :capture (atom nil) :redraw (atom nil)
                                         :fd fd :env (if (map? env)
                                                        (into {} (map (fn [[k v]] [(if (keyword? k) (name k) k) v])) env)
                                                        (into {} (map vec) env))
                                         :lines (vec lines))]
                          (locking active
                            (cancel! @active)
                            (reset! active job)
                            (.put queue job))
                          @(:reply job)))))]
        (socket/write! fd reply)))
    (catch Exception _
      (socket/write! fd {:exit 2}))
    (finally
      (socket/close! fd))))

(defn serve!
  "Keep accepting requests while the main thread owns the current CUI window."
  [parse open! finish! prepare!]
  (if-let [listener (socket/listen! (socket/path))]
    (let [queue (java.util.concurrent.LinkedBlockingQueue.)
          active (atom nil)
          acceptor (future
                     (loop []
                       (when-let [fd (socket/accept! (:fd listener))]
                         (future (handle! active queue parse fd))
                         (recur))))]
      (try
        (loop []
          (when (.isEmpty queue) (prepare!))
          (let [{:keys [fd reply env cancelled] :as job} (.take queue)]
            (if @cancelled
              (deliver reply {:exit 1})
              (binding [process/*environment* env *out* *err*]
                (let [result (try
                               (open! (assoc job :disconnected? #(socket/disconnected? fd)))
                               (catch Exception error
                                 (println "cloncher:" (ex-message error))
                                 {:status 1}))]
                  (finalize! active job)
                  (let [exit (or (:status result)
                                 (if (and (contains? #{:dmenu :secret} (:mode job))
                                          (nil? (:value result)))
                                   1 0))]
                    (deliver reply {:exit exit :out (:value result)}))
                  (when (and (:effect result) (not @cancelled))
                    (future (finish! result))))))
            (recur)))
        (finally
          (future-cancel acceptor)
          (socket/stop! listener))))
    0))
