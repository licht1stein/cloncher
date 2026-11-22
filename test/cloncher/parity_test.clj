(ns cloncher.parity-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is run-tests]]
            [cloncher.commands :as commands]
            [cloncher.config :as config]
            [cloncher.edit :as edit]
            [cloncher.files :as files]
            [cloncher.lisp :as lisp]
            [cloncher.machine :as machine]
            [cloncher.main :as app]
            [cloncher.menu :as menu]
            [cloncher.palette :as palette]
            [cloncher.process :as process]
            [cloncher.search :as search]
            [cloncher.sources :as sources]
            [cloncher.view :as view]))

(deftest first-launch-menu-configuration
  (let [directory (fs/create-temp-dir {:prefix "cloncher-test-"})]
    (try
      (binding [process/*environment* {"XDG_CONFIG_HOME" (str directory)}]
        (let [path (config/path)
              launches (mapv (fn [_] (future (config/setup! path))) (range 4))
              original (menu/load-configuration!)]
          (is (every? #(= path @%) launches))
          (is (= config/default-text (slurp (str path))))
          (is (= "Apps" (:label (first (:root-items original)))))
          (is (= ["Scan Wi-Fi networks" "Wi-Fi settings"] (mapv :label (menu/static-items :wifi))))
          (let [custom "{:menus {:main {:prompt \"Custom\" :items [{:id :example :label \"My action\" :action {:exec [\"true\"]}}]}}}"]
            (spit (str path) custom)
            (let [updated (menu/load-configuration!)]
              (is (= "My action" (:label (first (:root-items updated)))))
              (is (not= (:revision original) (:revision updated)))
              (config/setup! path)
              (is (= custom (slurp (str path))))
              (spit (str path) "{:menus {:main {:prompt \"Broken\" :items [{:provider :missing}]}}}")
              (binding [*err* (java.io.StringWriter.)]
                (is (= updated (menu/load-configuration!))))))))
      (finally (fs/delete-tree directory)))))

(deftest compatible-commands
  (is (= {:mode :project-add :path "."} (app/parse-args ["projects" "add"])))
  (is (= {:mode :project-add :path "/tmp"} (app/parse-args ["project-add" "/tmp"])))
  (is (= {:mode :rename-workspace} (app/parse-args ["rename-workspace"])))
  (is (= {:mode :control} (app/parse-args ["screenshot" "/tmp/panel.png"])))
  (is (= {:mode :secret :prompt "Wi-Fi"} (app/parse-args ["secret" "-p" "Wi-Fi"]))))

(deftest picker-rows-and-quick-jump
  (let [line "👩‍✈️  pilot\ttravel · people"
        row (search/line-row line)]
    (is (= line (:value row)))
    (is (= "👩‍✈️" (:icon row)))
    (is (= "travel · people" (:subtitle row)))
    (is (= "󰀻" (:icon (search/line-row "󰀻 App")))))
  (let [rows (search/label-rows (mapv #(hash-map :label (str %)) (range 110)))
        codes (mapv :shortcut rows)]
    (is (= ["aaa" "aas" "aad"] (subvec codes 0 3)))
    (is (= (count rows) (count (distinct codes))))
    (is (= 1 (:exact (search/jump rows "aas"))))))

(deftest prefixed-search-keeps-the-parent-query
  (doseq [[prefix kind] [["/" :files] ["@" :projects]]
          query ["" "marker"]]
    (let [parent {:kind :root :query query :index 2}
          {:keys [view open]} (#'app/handle-typing parent (str prefix query) 1 {} {})
          root (machine/transition machine/initial {:event :open :view view})
          nested (machine/transition root {:event :push :view open})]
      (is (= {:kind kind :initial-query query} open))
      (is (= parent (machine/current (machine/transition nested {:event :back})))))))

(deftest ranked-file-snapshot
  (let [index (files/parse-snapshot
                (str "4\tCode/lib/bindings.clj\u0000"
                     "2\tDownloads/binding.clj\u0000"
                     "3\tCode/.config/bindings.edn\u0000"
                     "10\t../escape\u0000"
                     "8\t/absolute\u0000"
                     "1\tCode/app.clj\u0000"))]
    (is (= ["Downloads/binding.clj" "Code/lib/bindings.clj" "Code/app.clj"]
           (files/rank index "" true)))
    (is (= ["Code/lib/bindings.clj"] (files/rank index "lib bindings" true)))
    (is (= ["Code/.config/bindings.edn"] (files/rank index ".conf bindings" true)))
    (is (= ["Downloads/binding.clj" "Code/lib/bindings.clj"] (files/rank index "binding.clj" true)))))

(deftest app-catalog-picks-up-installs-before-expiry
  (let [directory (fs/create-temp-dir {:prefix "cloncher-apps-"})
        apps (fs/create-dirs (fs/path directory "applications"))
        write-app! (fn [file label]
                     (spit (str file) (str "[Desktop Entry]\nType=Application\nName=" label "\nExec=true\n")))
        installed (fs/path apps "new.desktop")]
    (try
      (binding [process/*environment* {"HOME" (str directory)
                                       "XDG_CURRENT_DESKTOP" "test"
                                       "XDG_DATA_HOME" (str directory)
                                       "XDG_DATA_DIRS" (str directory)}]
        (write-app! (fs/path apps "existing.desktop") "Existing App")
        (is (= ["Existing App"] (mapv :label (sources/apps))))
        (write-app! installed "New App")
        (is (= ["Existing App" "New App"] (mapv :label (sources/apps))))
        (fs/delete installed)
        (is (= ["Existing App"] (mapv :label (sources/apps)))))
      (finally (fs/delete-tree directory)))))

(deftest shell-histories-and-expressions
  (is (= ["echo first\necho second" "pwd"]
         (sources/parse-histfile ": 100:0;echo first\\\necho second\n: 110:0;pwd")))
  (is (= {"j" "alias → jolt" "build" "function"}
         (commands/parse-zshrc "alias j='jolt'\nbuild() {\n  echo ignored\n}")))
  (is (= {:text "(λ [])" :caret 4} (edit/paired-edit "(λ )" "(λ [)" 4)))
  (is (= {:text "()" :caret 1} (edit/paired-edit "(())" "())" 1)))
  (is (= [3 4] (lisp/evaluate "(->> [1 2 3] (filter #(> % 1)) (map inc))")))
  (is (thrown? Exception (lisp/evaluate "(slurp \"/tmp/value\")"))))

(deftest palette-and-view-actions
  (let [colors (palette/parse-css "@define-color terminal_background #122334;")]
    (is (= [18 35 52 255 0] (get colors 1))))
  (is (= "Emacs config"
         (:label (menu/resolve-row :main "󱁻  Emacs config\tSystem › Edit config"))))
  (let [root {:kind :root}
        open (machine/transition machine/initial {:event :open :view root})
        nested (machine/transition open {:event :push :view {:kind :apps}})
        back (machine/transition nested {:event :back})
        complete (machine/transition back {:event :complete :reply {:done true}})
        off (machine/transition complete {:event :off})]
    (is (= root (machine/current back)))
    (is (= 3 (:focus back)))
    (is (= :off (:status off)))
    (is (empty? (:stack off)))
    (is (nil? (:reply off)))))

(deftest window-height-follows-results
  (let [two-rows [{:label "One"} {:label "Two"}]
        short (view/result-layout 520 two-rows)
        mixed (view/result-layout 520 [{:label "App" :app? true :live true}
                                       {:label "Emoji" :icon "😀"}])
        full (view/result-layout 520 (repeat 80 {:label "Result"}))]
    (is (= 62 (:height (view/result-layout 520 []))))
    (is (= {:page-size 2 :height 110} short))
    (is (= 131 (:height mixed)))
    (is (= {:page-size 19 :height 518 :virtual-row-height 24} full))
    (is (= 340 (view/window-height short true)))
    (is (= 518 (view/window-height full true)))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'cloncher.parity-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
