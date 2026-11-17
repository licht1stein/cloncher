(ns jolt-template.terminal
  "Terminal presentation and process reporting for the project tasks."
  (:refer-clojure :exclude [run!])
  (:require
    [clojure.java.io :as io]
    [jolt.ffi :as ffi]
    [jolt.process :as process]))

(ffi/defcfn ^:private isatty "isatty" [:int] :int)

(defn tty?
  "Whether file descriptor fd is connected to a terminal."
  [fd]
  (pos? (isatty fd)))

(defn style
  "Format text for fd, observing NO_COLOR and TERM=dumb."
  [fd tone text]
  (if (and (tty? fd) (not (seq (System/getenv "NO_COLOR"))) (not= "dumb" (System/getenv "TERM")))
    (str "\u001b[" ({:heading "1;35" :step "36" :ok "1;32" :warning "1;33" :error "1;31" :muted "2"} tone)
         "m" text "\u001b[0m")
    text))

(defn heading!
  "Display the command name before its progress lines."
  [command]
  (println (str (style 1 :heading "Jolt") "  " command))
  (flush))

(defn status!
  "Print a labelled status and optional detail. Errors use stderr."
  [tone label & [detail]]
  (let [fd (if (= :error tone) 2 1)
        marker (if (and (tty? fd) (not= "dumb" (System/getenv "TERM")))
                 ({:step "›" :ok "✓" :warning "!" :error "×"} tone)
                 ({:step ">" :ok "+" :warning "!" :error "x"} tone))]
    (binding [*out* (if (= fd 2) *err* *out*)]
      (println (str "  " (style fd tone marker) " " label
                    (when detail (str "  " (style fd :muted detail)))))
      (flush))))

(defn verbose?
  "Whether JOLT_VERBOSE=1 requests live subprocess output."
  []
  (= "1" (System/getenv "JOLT_VERBOSE")))

(defn- check-exit!
  [arguments exit log]
  (when-not (zero? exit)
    (throw (ex-info (str (first arguments) " exited with status " exit
                         (when log (str " · " (.getPath log))))
                    {:exit exit}))))

(defn run!
  "Inherit stdio while running argv. Throw with :exit on command failure."
  [arguments]
  (check-exit! arguments (:exit @(process/process arguments {:in :inherit :out :inherit :err :inherit})) nil))

(defn logged!
  "Run argv with its combined output in .dev/task-logs/name.log.
  Publish the completed log atomically and show its tail on failure.
  In verbose mode inherit stdio instead."
  [name arguments]
  (if (verbose?)
    (run! arguments)
    (let [directory (io/file ".dev/task-logs")
          log (io/file directory (str name ".log"))]
      (.mkdirs directory)
      (let [pending (.toFile (java.nio.file.Files/createTempFile
                               (.toPath directory) "task-" ".log"
                               (make-array java.nio.file.attribute.FileAttribute 0)))]
        (try
          (let [{:keys [exit]} @(process/process arguments {:in :inherit :out pending :err :out})]
            (when-not (.renameTo pending log)
              (throw (ex-info (str "Could not publish task log " (.getPath log)) {})))
            (when-not (zero? exit)
              (let [{:keys [out]} @(process/process ["tail" "-n" "24" (.getPath log)] {:out :string :err :inherit})]
                (when (seq out)
                  (binding [*out* *err*]
                    (print out)
                    (when-not (.endsWith out "\n") (newline))
                    (flush)))))
            (check-exit! arguments exit log))
          (finally
            (io/delete-file pending true)))))))

(defn exit!
  "Run a task and exit the CLI, preserving a subprocess's failure status."
  [task]
  (System/exit
    (try (task) 0
         (catch Exception error
           (status! :error (ex-message error))
           (or (:exit (ex-data error)) 1)))))
