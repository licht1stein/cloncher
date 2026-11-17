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
  (terminal/heading! "release")
  (terminal/status! :step "Build CUI backend")
  (terminal/logged! "native" ["sh" "bin/build-cui"])
  (terminal/status! :ok "CUI backend ready" "native/libcui_host.so")
  (let [target (io/file "target")]
    (.mkdirs target)
    (let [stage (.toFile (java.nio.file.Files/createTempDirectory (.toPath target) "release-"
                                                                  (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (let [executable (build-executable! stage)
              backend (io/file stage "libcui_host.so")]
          (terminal/run! ["install" "-m755" "native/libcui_host.so" (.getPath backend)])
          (terminal/run! ["mv" "-f" "-t" (.getPath target) "--" (.getPath backend) (.getPath executable)])
          (terminal/status! :ok "Release ready" (.getPath (io/file target "cloncher")))
          (println "    CUI library     target/libcui_host.so")
          (when-not (terminal/verbose?)
            (println "    Build logs      .dev/task-logs/")))
        (finally
          (terminal/run! ["rm" "-rf" "--" (.getPath stage)]))))))

(terminal/exit! release!)
