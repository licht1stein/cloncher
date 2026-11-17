(ns cloncher.palette
  "Use the Noctalia-generated launcher palette with CUI's semantic colors."
  (:require [babashka.fs :as fs]
            [cloncher.process :as p]))

(def ^:private tokens
  {1 "terminal_background" 2 "surface_container_low" 3 "surface_container_high"
   4 "terminal_background" 5 "terminal_foreground" 6 "on_surface_variant"
   7 "terminal_bright_black" 8 "terminal_background" 9 "outline_variant" 10 "terminal_background"
   11 "primary" 12 "primary" 13 "primary_container" 14 "on_primary"
   15 "secondary_container" 16 "on_secondary_container"
   17 "error" 18 "terminal_selection_fg" 19 "primary" 20 "on_primary" 21 "surface_container_highest"
   25 "surface_container_high" 26 "terminal_selection_bg" 28 "terminal_background"})

(defn parse-css [text]
  (let [colors (into {}
                     (map (fn [[_ name hex]]
                            [name (mapv #(Integer/parseInt (apply str %) 16) (partition 2 hex))]))
                     (re-seq #"(?m)^\s*@define-color\s+(\w+)\s+#([\da-fA-F]{6})\s*;" text))]
    (into {} (keep (fn [[index label]]
                     (when-let [rgb (colors label)] [index (conj rgb 255 0)]))) tokens)))

(defn load-colors []
  (let [path (fs/path (or (p/getenv "XDG_CONFIG_HOME") (str (p/getenv "HOME") "/.config"))
                      "launcher" "colors.css")]
    (try (parse-css (slurp (str path))) (catch Exception _ {}))))
