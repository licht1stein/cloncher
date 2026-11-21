(ns release
  "Build and publish Cloncher and its CUI backend. Prefer an optimized
  executable with no compiler image and retry with the standard Jolt build."
  (:require
    [clojure.java.io :as io]
    [jolt-template.terminal :as terminal]))

(defn- build!
  [log executable flags]
  (terminal/logged! log
    (into ["jolt" "build" "-m" "cloncher.main" "-o" (.getPath executable)] flags))
  executable)

(defn- build-executable!
  [stage]
  (try
    (terminal/status! :step "Compile optimized executable")
    (let [executable (build! "release-optimized" (io/file stage "optimized" "cloncher") ["--closed-world" "--opt"])
          boot (io/file (str (.getPath executable) ".build"))]
      (when-not (and (.isFile (io/file boot "petite.boot"))
                     (not (.exists (io/file boot "scheme.boot"))))
        (throw (ex-info (if (terminal/verbose?)
                          "Jolt retained the compiler image."
                          "Jolt retained the compiler image. See .dev/task-logs/release-optimized.log.")
                        {})))
      (terminal/status! :ok "Compiler image omitted")
      executable)
    (catch Exception error
      (terminal/status! :warning "Compiler-free build unavailable" (ex-message error))
      (terminal/status! :step "Compile with runtime compiler support")
      (build! "release-standard" (io/file stage "standard" "cloncher") []))))

(defn- release!
  []
  (terminal/status! :step "Build CUI backend")
  (terminal/logged! "native" ["sh" "bin/build-cui"])
  (terminal/status! :ok "CUI backend ready" "native/libcui_host.so")
  (let [target (io/file "target")]
    (.mkdirs target)
    (let [stage (.toFile (java.nio.file.Files/createTempDirectory (.toPath target) "release-"
                                                                  (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (let [executable (build-executable! stage)
              backend (io/file stage "libcui_host.so")
              client (io/file stage "cloncher-client")]
          (terminal/logged! "release-client"
                            [(or (System/getenv "CC") "cc") "-std=c99" "-O2" "-Wall" "-Wextra"
                             "native/launcher-client.c" "-o" (.getPath client)])
          (terminal/run! ["install" "-m755" "native/libcui_host.so" (.getPath backend)])
          (terminal/run! ["mv" "-f" "-t" (.getPath target) "--"
                          (.getPath backend) (.getPath executable) (.getPath client)])
          (terminal/status! :ok "Release ready" (.getPath (io/file target "cloncher")))
          (println "    CUI library     target/libcui_host.so")
          (println "    Hotkey client   target/cloncher-client")
          (when-not (terminal/verbose?)
            (println "    Build logs      .dev/task-logs/")))
        (finally
          (terminal/run! ["rm" "-rf" "--" (.getPath stage)]))))))

(defn- install! []
  (let [local (io/file (System/getProperty "user.home") ".local")
        destination (str (io/file local "libexec" "cloncher"))
        bin (str (io/file local "bin"))
        command (str (io/file bin "cloncher"))
        files ["libcui_host.so" "cloncher" "cloncher-client"]]
    (terminal/status! :step "Install Cloncher" destination)
    (terminal/run! ["install" "-d" "--" destination bin])
    (let [stage (str (java.nio.file.Files/createTempDirectory (.toPath (io/file destination)) ".install-"
                                                             (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (terminal/run! (into ["install" "-m755" "-t" stage "--"] (map #(str (io/file "target" %))) files))
        (terminal/run! (into ["mv" "-f" "-t" destination "--"] (map #(str (io/file stage %))) files))
        (finally
          (terminal/run! ["rm" "-rf" "--" stage]))))
    (terminal/run! ["ln" "-sfnT" "--" "../libexec/cloncher/cloncher-client" command])
    (terminal/status! :ok "Installed Cloncher" command)
    (terminal/status! :step "Restart launcher.service")
    (terminal/run! ["systemctl" "--user" "restart" "launcher.service"])
    (terminal/status! :ok "Restarted launcher.service")))

(terminal/exit!
  (fn []
    (let [install? (= ["install"] (vec *command-line-args*))]
      (terminal/heading! (if install? "install" "release"))
      (release!)
      (when install? (install!)))))
