(ns cloncher.view-cache
  "Persist displayable menu rows. Actions are resolved in the current menu on activation."
  (:require [babashka.fs :as fs]
            [cloncher.menu :as menu]
            [cloncher.persistence :as disk]
            [cloncher.search :as search]))

(defn- cache-file [{:keys [kind key]}]
  (let [name (case kind
               :root "root"
               :menu (when (and (keyword? key) (not (contains? #{:windows :files :projects} key)))
                       (name key))
               nil)]
    (when (and name (re-matches #"[\w-]+" name))
      (disk/cache "cloncher-views" (str name ".json")))))

(def ^:private memory (atom {}))

(defn- cache-key [file]
  [(str file) (:revision (menu/configuration))])

(defn cached-rows [view]
  (when-let [file (cache-file view)]
    (get @memory (cache-key file))))

(defn- remember! [file rows]
  (let [key (cache-key file)
        rows (search/index-rows
               (mapv #(cond-> % (:destination %) (assoc :action (keyword (:destination %)))) rows))]
    (swap! memory (fn [entries] (into {key rows} (take 63 (dissoc entries key)))))
    rows))

(defn- load-rows! [view]
  (when-let [file (cache-file view)]
    (when (and (not (contains? @memory (cache-key file)))
               (fs/regular-file? file)
               (or (not (fs/regular-file? menu/source-file))
                   (>= (.toMillis (fs/last-modified-time file))
                       (.toMillis (fs/last-modified-time menu/source-file)))))
      (let [{:keys [version revision rows]} (disk/read-json file nil)]
        (when (and (= version 2) (= revision (:revision (menu/configuration))))
          (remember! file rows))))))

(defn prime!
  "Populate the in-memory menu cache off the window thread."
  []
  (doseq [view (cons {:kind :root} (map #(hash-map :kind :menu :key %) (keys (:menus (menu/configuration)))))]
    (load-rows! view)))

(defn save-rows! [view rows]
  (when-let [file (cache-file view)]
    (let [safe (fn [row]
                 (let [action (:action row)]
                   (cond-> (select-keys row [:icon :label :subtitle :path :row-id :state :deep
                                             :hint :alt-hint :shortcut :warn :section :order :search])
                     (menu/menu-item? action) (assoc :destination (name action))
                     (or (and action (not (menu/menu-item? action))) (:alt-action row))
                     (assoc :cached-id (menu/format-item row)))))
          rows (into [] (comp (remove #(or (:app? %) (:live %) (:volatile %))) (map safe)) rows)]
      (remember! file rows)
      (disk/write-json! file {:version 2 :revision (:revision (menu/configuration)) :rows rows}))))
