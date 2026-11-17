(ns cloncher.socket
  "Linux AF_UNIX transport. Every connection carries one JSON request and reply."
  (:require [babashka.fs :as fs]
            [cloncher.json :as json]
            [jolt.ffi :as ffi]))

(ffi/defcfn ^:private c-socket "socket" [:int :int :int] :int)
(ffi/defcfn ^:private c-bind "bind" [:int :pointer :uint32] :int)
(ffi/defcfn ^:private c-connect "connect" [:int :pointer :uint32] :int :blocking)
(ffi/defcfn ^:private c-listen "listen" [:int :int] :int)
(ffi/defcfn ^:private c-accept "accept4" [:int :pointer :pointer :int] :int :blocking)
(ffi/defcfn ^:private c-read "recv" [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn ^:private c-write "send" [:int :pointer :size_t :int] :ssize_t :blocking)
(ffi/defcfn ^:private c-close "close" [:int] :int)
(ffi/defcfn ^:private c-open "open" [:string :int :uint32] :int)
(ffi/defcfn ^:private c-flock "flock" [:int :int] :int)
(ffi/defcfn ^:private c-poll "poll" [:pointer :size_t :int] :int :blocking)
(ffi/defcfn ^:private effective-user "geteuid" [] :uint32)

(def ^:private cloexec 0x80000)
(def ^:private max-message (* 16 1024 1024))

(defn path []
  (str (fs/path (or (System/getenv "CLONCHER_RUNTIME_DIR")
                    (System/getenv "XDG_RUNTIME_DIR")
                    (str "/tmp/cloncher-" (effective-user)))
                "cloncher" "launcher.sock")))

(defn close! [fd]
  (when (and (some? fd) (<= 0 fd)) (c-close fd)))

(defn- address! [path operate]
  (let [octets (.getBytes ^String path "UTF-8")
        n (alength octets)]
    (when (or (> n 107) (some zero? octets))
      (throw (ex-info "Launcher socket path must fit a Linux Unix socket address" {:path path})))
    (let [p (ffi/alloc (+ 3 n))]
      (try
        (ffi/write p :uint16 1)
        (ffi/write-bytes (ffi/slice p 2) octets)
        (ffi/write p :uint8 0 (+ 2 n))
        (operate p (+ 3 n))
        (finally (ffi/free p))))))

(defn connect [path]
  (let [fd (c-socket 1 (bit-or 1 cloexec) 0)]
    (when (neg? fd) (throw (ex-info "Could not create launcher socket" {})))
    (try
      (if (zero? (address! path #(c-connect fd %1 %2)))
        fd
        (do (close! fd) nil))
      (catch Throwable error
        (close! fd)
        (throw error)))))

(defn accept! [fd]
  (let [peer (c-accept fd ffi/null ffi/null cloexec)]
    (when-not (neg? peer) peer)))

(defn- ready? [fd timeout]
  (let [poll (ffi/alloc 8)]
    (try
      (ffi/write poll :int fd)
      (ffi/write poll :int16 1 4)
      (ffi/write poll :int16 0 6)
      (pos? (c-poll poll 1 timeout))
      (finally (ffi/free poll)))))

(defn disconnected? [fd]
  (when (ready? fd 0)
    (let [p (ffi/alloc 1)]
      (try (zero? (c-read fd p 1 0x42))
           (finally (ffi/free p))))))

(defn read! [fd timeout]
  (let [size 32768
        buffer (ffi/alloc size)
        output (java.io.ByteArrayOutputStream.)]
    (try
      (loop []
        (when (and timeout (not (ready? fd timeout)))
          (throw (ex-info "Launcher socket read timed out" {})))
        (let [n (c-read fd buffer size 0)]
          (cond
            (neg? n) (throw (ex-info "Could not read launcher request" {}))
            (zero? n) nil
            (> (+ (.size output) n) max-message)
            (throw (ex-info "Launcher socket message is too large" {}))
            :else
            (let [bytes (ffi/read-array buffer :uint8 n)
                  end (loop [i 0]
                        (cond (= i n) n
                              (= 10 (aget bytes i)) i
                              :else (recur (inc i))))]
              (.write output bytes 0 (int end))
              (if (< end n)
                (json/parse-string (String. (.toByteArray output) "UTF-8") true)
                (recur))))))
      (finally (ffi/free buffer)))))

(defn write! [fd value]
  (let [bytes (.getBytes (str (json/generate-string value) "\n") "UTF-8")
        n (alength bytes)
        buffer (ffi/alloc n)]
    (try
      (ffi/write-bytes buffer bytes)
      (loop [start 0]
        (if (= start n)
          true
          (let [written (c-write fd (ffi/slice buffer start) (- n start) 0x4000)]
            (if (pos? written) (recur (+ start written)) false))))
      (finally (ffi/free buffer)))))

(defn request! [message]
  (when-let [fd (connect (path))]
    (try
      (if (write! fd message)
        (or (read! fd nil) {:exit 1})
        {:exit 1})
      (finally (close! fd)))))

(defn listen! [path]
  (let [parent (fs/parent path)]
    (fs/create-dirs parent {:posix-file-permissions "rwx------"})
    (when (or (fs/sym-link? parent)
              (not= "rwx------" (fs/posix->str (fs/posix-file-permissions parent {:nofollow-links true}))))
      (throw (ex-info "Launcher socket directory must be private (0700)" {:path (str parent)})))
    (let [lock (c-open (str path ".lock") (bit-or 0x1 0x40 0x20000 cloexec) 0x180)]
      (when (neg? lock) (throw (ex-info "Could not open launcher socket lock" {})))
      (if (neg? (c-flock lock 6))
        (do (close! lock) nil)
        (let [fd (c-socket 1 (bit-or 1 cloexec) 0)]
          (try
            (fs/delete-if-exists path)
            (when-not (and (<= 0 fd) (zero? (address! path #(c-bind fd %1 %2))))
              (throw (ex-info "Could not bind launcher socket" {:path path})))
            (fs/set-posix-file-permissions path "rw-------")
            (when-not (zero? (c-listen fd 16))
              (throw (ex-info "Could not listen on launcher socket" {:path path})))
            {:fd fd :lock lock :path path}
            (catch Throwable error
              (close! fd)
              (close! lock)
              (throw error))))))))

(defn stop! [{:keys [fd lock path]}]
  (close! fd)
  (try (fs/delete-if-exists path)
       (finally (close! lock))))
