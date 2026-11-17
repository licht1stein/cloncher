(ns release
  "Build and publish Cloncher and its CUI backend. Prefer an optimized
  executable with no compiler image and retry with the standard Jolt build."
  (:require
    [clojure.java.io :as io]
    [jolt.process :as process]))

(defn- build!
  [executable flags]
  (process/shell
    (into ["jolt" "build" "-m" "cloncher.main" "-o" (.getPath executable)] flags))
  executable)

(defn- build-executable!
  [stage]
  (try
    (let [executable (build! (io/file stage "optimized" "cloncher") ["--closed-world" "--opt"])
          boot (io/file (str (.getPath executable) ".build"))]
      (when-not (and (.isFile (io/file boot "petite.boot"))
                     (not (.exists (io/file boot "scheme.boot"))))
        (throw (ex-info "Jolt retained the compiler image. See the closed-world diagnostics above." {})))
      executable)
    (catch Exception error
      (println "Compiler-free release unavailable." (ex-message error))
      (println "Retrying with the standard build and runtime compiler support.")
      (build! (io/file stage "standard" "cloncher") []))))

(defn- release!
  []
  (let [target (io/file "target")]
    (.mkdirs target)
    (let [stage (.toFile (java.nio.file.Files/createTempDirectory (.toPath target) "release-"
                                                                  (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (let [executable (build-executable! stage)
              backend (io/file stage "libcui_host.so")]
          (process/shell ["install" "-m755" "native/libcui_host.so" (.getPath backend)])
          (process/shell ["mv" "-f" "-t" (.getPath target) "--" (.getPath backend) (.getPath executable)])
          (println "Native application:" (.getPath (io/file target "cloncher"))))
        (finally
          (process/shell ["rm" "-rf" "--" (.getPath stage)]))))))

(release!)
