(ns cloncher.usage
  (:require [cloncher.menu :as menu]
            [cloncher.persistence :as disk]))

(def ^:private state-lock (Object.))

(defn remember! [row]
  (when (and row (not (:volatile row)) (not (:live row))
             (not (:command row)) (not (:calc row)) (not (:lisp row)))
    (let [id (or (:row-id row) (menu/format-item row))]
      (when (seq id)
        (locking state-lock
          (let [path (disk/cache "history.json")
                prior (disk/read-json path {} false)]
            (disk/write-json! path (update prior id (fnil inc 0)))))))))
