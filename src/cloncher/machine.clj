(ns cloncher.machine
  "Pure transitions for a launcher session. The host controls window lifetime.")

(def initial
  {:status :off :stack [] :focus 0})

(defn current [state] (peek (:stack state)))

(defn transition
  "Apply a launcher event. Views carry prepared rows or pending jobs supplied by the host."
  [{:keys [status stack] :as state} {:keys [event view reply]}]
  (case event
    :open (-> state (assoc :status :on :stack [view]) (dissoc :reply) (update :focus inc))
    :off (-> state (assoc :status :off :stack []) (dissoc :reply))
    :complete (if (= :on status) (assoc state :status :complete :reply reply) state)
    :push (if (= :on status) (-> state (update :stack conj view) (update :focus inc)) state)
    :back (if (and (= :on status) (< 1 (count stack)))
            (-> state (update :stack pop) (update :focus inc)) state)
    :view (if (= :on status) (assoc state :stack (conj (pop stack) view)) state)
    :fill (if (= :on status)
            (-> state (assoc :stack (conj (pop stack) view)) (update :focus inc)) state)
    (throw (ex-info "Unknown launcher event" {:event event}))))
