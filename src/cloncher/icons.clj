(ns cloncher.icons
  "Resolve application icons from the desktop entry and the current XDG data roots."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.process :as p]))

(defn data-dirs []
  (distinct (cons (or (p/getenv "XDG_DATA_HOME") (str (p/getenv "HOME") "/.local/share"))
                  (str/split (or (p/getenv "XDG_DATA_DIRS") "/usr/local/share:/usr/share") #":"))))

(defn- icon-themes []
  (let [config (or (p/getenv "XDG_CONFIG_HOME") (str (p/getenv "HOME") "/.config"))
        gtk-theme (some (fn [version]
                          (try (some->> (slurp (str (fs/path config version "settings.ini")))
                                        (re-find #"(?m)^gtk-icon-theme-name\s*=\s*(\S+)") second)
                               (catch Exception _ nil)))
                        ["gtk-4.0" "gtk-3.0"])]
    (distinct (remove nil? [gtk-theme "hicolor" "Papirus-Dark" "Papirus" "Adwaita"]))))

(defn resolve-icon [roots themes icon]
  (when (seq icon)
    (let [extensions (if (re-find #"\.(?:png|svg|jpe?g)$" icon) [""] [".png" ".svg"])
          candidates (if (fs/absolute? icon)
                       [icon]
                       (concat
                         (for [theme themes
                               dir roots
                               size ["48x48" "48x48@2" "64x64" "scalable" "128x128"
                                     "256x256" "32x32" "24x24" "symbolic"]
                               extension extensions]
                           (fs/path dir "icons" theme size "apps" (str icon extension)))
                         (for [dir roots extension extensions]
                           (fs/path dir "pixmaps" (str icon extension)))))]
      (some (fn [path]
              (when (and (fs/regular-file? path)
                         (<= (fs/size path) (* 2 1024 1024)))
                (str path)))
            candidates))))

(defn resolver []
  (let [dirs (cons (str (p/getenv "HOME") "/.icons") (data-dirs))
        themes (icon-themes)]
    (memoize (partial resolve-icon dirs themes))))
