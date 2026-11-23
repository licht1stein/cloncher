(ns cloncher.clipboard
  "Cloncher's clipboard history: an encrypted store fed by `wl-paste --watch`.

  The store lives in `$XDG_STATE_HOME/cloncher/clipboard`: an `index.enc` manifest
  and one `entries/<id>.enc` payload per entry. Each file is an XChaCha20-Poly1305
  envelope (magic, version, algorithm, nonce, ciphertext and tag) whose associated
  data binds it to its purpose and entry. The key is derived from a master key kept
  in the Secret Service, created on first use.

  On first use the history Noctalia recorded is imported once; Noctalia's store is
  only read, with the key Noctalia keeps in the Secret Service."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.json :as json]
            [cloncher.process :as p]
            [jolt.ffi :as ffi]))

;; ── libsodium ───────────────────────────────────────────────────────────────

(ffi/defcfn ^:private sodium-init "sodium_init" [] :int)
(ffi/defcfn ^:private derive-key* "crypto_kdf_derive_from_key"
  [:pointer :size_t :uint64 :pointer :pointer] :int)
(ffi/defcfn ^:private encrypt* "crypto_aead_xchacha20poly1305_ietf_encrypt"
  [:pointer :pointer :pointer :uint64 :pointer :uint64 :pointer :pointer :pointer] :int)
(ffi/defcfn ^:private decrypt* "crypto_aead_xchacha20poly1305_ietf_decrypt"
  [:pointer :pointer :pointer :pointer :uint64 :pointer :uint64 :pointer :pointer] :int)
(ffi/defcfn ^:private random-bytes* "randombytes_buf" [:pointer :size_t] :void)
(ffi/defcfn ^:private hash* "crypto_generichash" [:pointer :size_t :pointer :uint64 :pointer :size_t] :int)
(ffi/defcfn ^:private memzero "sodium_memzero" [:pointer :size_t] :void)

(ffi/defcfn ^:private c-open "open" [:string :int :uint32] :int)
(ffi/defcfn ^:private c-flock "flock" [:int :int] :int :blocking)
(ffi/defcfn ^:private c-close "close" [:int] :int)

;; Noctalia links the system libsodium; a Nix-built Cloncher does not search the system paths.
(defonce ^:private sodium
  (delay (let [library (or (some #(when (fs/exists? %) %)
                                 ["/usr/lib/libsodium.so.26" "/usr/lib64/libsodium.so.26"
                                  "/usr/lib/x86_64-linux-gnu/libsodium.so.23"])
                           (throw (ex-info "Install libsodium for the clipboard history" {})))]
           (ffi/load-library library))
         (when (neg? (sodium-init))
           (throw (ex-info "libsodium could not initialize" {})))))

(defn- with-memory
  "Call `f` with native memory holding `bytes`, wiped and freed afterwards."
  [bytes f]
  @sodium
  (let [memory (ffi/alloc (max 1 (alength bytes)))]
    (try
      (when (pos? (alength bytes)) (ffi/write-bytes memory bytes))
      (f memory)
      (finally
        (memzero memory (max 1 (alength bytes)))
        (ffi/free memory)))))

(defn- native-bytes
  "`n` bytes that `f` writes into native memory, wiped and freed afterwards."
  [n f]
  @sodium
  (let [memory (ffi/alloc (max 1 n))]
    (try
      (f memory)
      (ffi/read-array memory :uint8 n)
      (finally
        (memzero memory (max 1 n))
        (ffi/free memory)))))

(defn- random-bytes [n] (native-bytes n #(random-bytes* % n)))

(defn- hex [bytes] (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn- unhex [^String text]
  (let [out (byte-array (quot (count text) 2))]
    (dotimes [i (alength out)]
      (aset-byte out i (unchecked-byte (Integer/parseInt (subs text (* 2 i) (+ 2 (* 2 i))) 16))))
    out))

(defn- content-hash [^String mime bytes]
  (let [prefix (.getBytes (str mime "\u0000") "UTF-8")
        data (byte-array (+ (alength prefix) (alength bytes)))]
    (System/arraycopy prefix 0 data 0 (alength prefix))
    (System/arraycopy bytes 0 data (alength prefix) (alength bytes))
    (hex (native-bytes 32 (fn [out] (with-memory data #(hash* out 32 % (alength data) ffi/null 0)))))))

(defn- derive-key [master ^String context]
  (native-bytes 32 (fn [out]
                     (with-memory master
                       (fn [master]
                         (with-memory (.getBytes context "US-ASCII")
                           #(when-not (zero? (derive-key* out 32 1 % master))
                              (throw (ex-info "Could not derive the clipboard key" {})))))))))

;; ── envelopes ───────────────────────────────────────────────────────────────

(def ^:private header-size (+ 12 2 24))
(def ^:private tag-size 16)

(defn- length-prefixed [^String text]
  (let [bytes (.getBytes text "UTF-8")
        n (alength bytes)
        out (byte-array (+ 8 n))]
    (dotimes [i 8] (aset-byte out i (unchecked-byte (bit-shift-right n (* 8 (- 7 i))))))
    (System/arraycopy bytes 0 out 8 n)
    out))

(defn- concat-bytes [& parts]
  (let [out (byte-array (reduce + (map alength parts)))]
    (reduce (fn [offset part] (System/arraycopy part 0 out offset (alength part)) (+ offset (alength part)))
            0 parts)
    out))

(defn- associated-data [magic purpose object-id]
  (concat-bytes magic (byte-array [1 1]) (length-prefixed purpose) (length-prefixed object-id)))

(defn- read-envelope
  "The plaintext of the envelope at `path`, authenticated against `purpose` and `object-id`."
  [{:keys [magic key]} path purpose object-id]
  (let [envelope (fs/read-all-bytes path)
        ciphertext-size (- (alength envelope) header-size)]
    (when (or (< ciphertext-size tag-size)
              (not= (seq magic) (take 12 envelope))
              (not= [1 1] (map #(aget envelope %) [12 13])))
      (throw (ex-info "Unsupported clipboard file" {:path (str path)})))
    (let [aad (associated-data magic purpose object-id)
          plaintext-size (- ciphertext-size tag-size)]
      (native-bytes plaintext-size
        (fn [output]
          (let [written (ffi/alloc 8)]
            (try
              (with-memory envelope
                (fn [memory]
                  (with-memory aad
                    (fn [aad-memory]
                      (with-memory (key)
                        (fn [key]
                          (when-not (zero? (decrypt* output written ffi/null (ffi/slice memory header-size)
                                                     ciphertext-size aad-memory (alength aad)
                                                     (ffi/slice memory 14) key))
                            (throw (ex-info "The clipboard history did not decrypt" {:path (str path)})))))))))
              (finally (ffi/free written)))))))))

(defn- write-atomically! [path bytes]
  (let [parent (fs/parent path)]
    (fs/create-dirs parent {:posix-file-permissions "rwx------"})
    (let [temporary (fs/create-temp-file {:dir parent :prefix ".cloncher-"})]
      (try
        (fs/set-posix-file-permissions temporary "rw-------")
        (fs/write-bytes temporary bytes)
        (fs/move temporary path {:atomic-move true :replace-existing true})
        (finally (fs/delete-if-exists temporary))))))

(defn- write-envelope!
  [{:keys [magic key]} path purpose object-id plaintext]
  (let [nonce (random-bytes 24)
        aad (associated-data magic purpose object-id)
        ciphertext (native-bytes (+ (alength plaintext) tag-size)
                     (fn [output]
                       (with-memory plaintext
                         (fn [message]
                           (with-memory aad
                             (fn [aad-memory]
                               (with-memory nonce
                                 (fn [nonce-memory]
                                   (with-memory (key)
                                     #(when-not (zero? (encrypt* output ffi/null message (alength plaintext)
                                                                 aad-memory (alength aad) ffi/null
                                                                 nonce-memory %))
                                        (throw (ex-info "Could not encrypt a clipboard entry" {}))))))))))))]
    (write-atomically! path (concat-bytes magic (byte-array [1 1]) nonce ciphertext))))

;; ── keys ────────────────────────────────────────────────────────────────────

(def ^:private secret-attributes ["application" "cloncher" "purpose" "clipboard-history" "version" "1"])

(defn- secret-lookup [attributes]
  (let [{:keys [exit out]} @(apply p/process {:out :bytes :err :discard :continue true}
                                   "secret-tool" "lookup" attributes)]
    (when (and (zero? exit) (pos? (alength out))) out)))

(defn- master-key
  "Cloncher's clipboard master key, created and stored in the Secret Service on first use."
  []
  (if-let [stored (secret-lookup secret-attributes)]
    (unhex (str/trim (String. stored "US-ASCII")))
    (let [created (random-bytes 32)
          {:keys [exit]} @(apply p/process {:in (hex created) :out :discard :err :discard :continue true}
                                 "secret-tool" "store" "--label" "Cloncher clipboard history key"
                                 secret-attributes)]
      (when-not (zero? exit)
        (throw (ex-info "Could not store the clipboard key in the Secret Service (is the keyring unlocked?)" {})))
      created)))

;; The derived keys stay for the process's lifetime.
(defonce ^:private derived-keys (atom {}))

(defn- cached-key [name f]
  (or (get @derived-keys name)
      (get (swap! derived-keys assoc name (do @sodium (f))) name)))

(defn- state-home []
  (or (p/getenv "XDG_STATE_HOME") (str (p/getenv "HOME") "/.local/state")))

(def ^:private store
  {:magic (.getBytes "CLONCHERENC1" "US-ASCII")
   :key #(cached-key :cloncher (fn [] (derive-key (master-key) "CLONCLIP")))})

(def ^:private noctalia
  {:magic (.getBytes "NOCTALIAENC1" "US-ASCII")
   :key #(cached-key :noctalia
                     (fn []
                       (let [master (secret-lookup ["application" "noctalia" "scope" "storage" "owner"
                                                    "encrypted-state" "name" "master-key" "version" "1"])]
                         (when-not (and master (= 32 (alength master)))
                           (throw (ex-info "Noctalia's clipboard key is not available" {})))
                         (derive-key master "NOCTCLIP"))))})

;; ── index ───────────────────────────────────────────────────────────────────

(def ^:private keep-unsaved 200)
(def ^:private max-text-bytes (* 1024 1024))
(def ^:private max-image-bytes (* 16 1024 1024))

(defn- directory [] (fs/path (state-home) "cloncher" "clipboard"))
(defn- index-path [] (fs/path (directory) "index.enc"))
(defn- entry-path [id] (fs/path (directory) "entries" (str id ".enc")))

(defn- with-lock
  "Run `f` holding the store's lock, so concurrent captures and edits do not interleave."
  [f]
  (fs/create-dirs (directory) {:posix-file-permissions "rwx------"})
  (let [fd (c-open (str (fs/path (directory) "lock")) (bit-or 0x1 0x40 0x80000) 0x180)]
    (when (neg? fd) (throw (ex-info "Could not open the clipboard lock" {})))
    (try
      (c-flock fd 2)
      (f)
      (finally (c-close fd)))))

(defn- read-index []
  (when (fs/exists? (index-path))
    (:entries (json/parse-string (String. (read-envelope store (index-path) "clipboard-index-v1" "index") "UTF-8")
                                 true))))

(defn- write-index! [entries]
  (write-envelope! store (index-path) "clipboard-index-v1" "index"
                   (.getBytes (json/generate-string {:entries entries}) "UTF-8")))

(defn- ordered
  "Saved entries first, then the rest newest first; drop unsaved entries past the limit."
  [entries]
  (let [{saved true unsaved false} (group-by (comp boolean :saved) entries)]
    (into (vec saved) (take keep-unsaved (sort-by (comp - :at) unsaved)))))

(defn- text? [mime] (str/starts-with? mime "text/"))

(defn- add-entry
  "Record `bytes` of `mime` captured at `at` in `entries`. The same content again moves
  to the top and stays saved if it was."
  [entries mime bytes at]
  (let [hash (content-hash mime bytes)]
    (if (some #(= hash (:hash %)) entries)
      (mapv #(if (= hash (:hash %)) (assoc % :at at) %) entries)
      (let [id (str at "-" (hex (random-bytes 4)))]
        (write-envelope! store (entry-path id) "clipboard-entry-v1" id bytes)
        (conj (vec entries)
              {:id id :mime mime :hash hash :bytes (alength bytes) :at at :saved false
               :preview (when (text? mime)
                          (let [text (String. bytes "UTF-8")] (subs text 0 (min 200 (count text)))))})))))

(defn- commit!
  "Write `entries` in order and remove payloads no entry refers to."
  [entries]
  (let [entries (ordered entries)
        kept (set (map :id entries))]
    (write-index! entries)
    (doseq [file (fs/list-dir (fs/path (directory) "entries"))]
      (let [id (str/replace (str (fs/file-name file)) #"\.enc$" "")]
        (when-not (kept id) (fs/delete-if-exists file))))
    entries))

(defn- import-noctalia
  "Noctalia's history as entries of this store, oldest first, its pinned entries saved;
  none when it cannot be read."
  [entries]
  (try
    (let [base (fs/path (state-home) "noctalia" "clipboard")
          manifest (read-envelope noctalia (fs/path base "index.enc") "clipboard-manifest-v1" "index")]
      (reduce (fn [entries {:keys [id data_mime_type captured_at_ms pinned]}]
                (try
                  (let [bytes (read-envelope noctalia (fs/path base "entries" (str id ".enc"))
                                             "clipboard-payload-v1" id)
                        added (add-entry entries data_mime_type bytes captured_at_ms)]
                    (cond-> added
                      (and pinned (> (count added) (count entries))) (update (count entries) assoc :saved true)))
                  (catch Exception _ entries)))
              entries
              (reverse (:entries (json/parse-string (String. manifest "UTF-8") true)))))
    (catch Exception _ entries)))

(defn- update-store!
  "Apply `f` to the entries under the lock and keep the result. The first use imports Noctalia's history."
  [f]
  (with-lock
    #(let [existing (read-index)]
       (commit! (f (or existing (import-noctalia [])))))))

;; ── public API ──────────────────────────────────────────────────────────────

(defn history
  "The entries, saved first and then newest first:
  `{:id :mime :preview :bytes :at :saved}` with `:at` in epoch milliseconds."
  []
  (or (read-index) (update-store! identity)))

(defn payload
  "The bytes of an entry."
  [{:keys [id]}]
  (read-envelope store (entry-path id) "clipboard-entry-v1" id))

(defn delete! [{:keys [id]}]
  (update-store! (fn [entries] (filterv #(not= id (:id %)) entries))))

(defn toggle-saved!
  "Save an entry, which keeps it at the top and out of pruning, or unsave it."
  [{:keys [id]}]
  (update-store! (fn [entries] (mapv #(if (= id (:id %)) (update % :saved not) %) entries))))

(defn clear!
  "Remove every unsaved entry."
  []
  (update-store! (fn [entries] (filterv :saved entries))))

(def ^:private text-types ["text/plain;charset=utf-8" "text/plain" "UTF8_STRING" "STRING" "TEXT"])
(def ^:private image-types ["image/png" "image/jpeg" "image/webp" "image/gif" "image/bmp"])

;; Every request to the selection's owner is bounded: an application that never answers
;; must not wedge the capture, and with it `wl-paste --watch` and every later copy.
(def ^:private paste-timeout-ms 3000)

(defn- wl-paste
  "The output of wl-paste with `args`, or nil when it fails or outlasts `paste-timeout-ms`."
  [& args]
  (let [child (apply p/process {:out :bytes :err :discard :continue true} "wl-paste" args)
        result (deref child paste-timeout-ms ::timeout)]
    (cond (= ::timeout result) (do (p/destroy-tree child) nil)
          (zero? (:exit result)) (:out result))))

(defn- textual?
  "wl-paste's test for a text type."
  [mime]
  (boolean (or (str/starts-with? mime "text/") (#{"TEXT" "STRING" "UTF8_STRING"} mime)
               (str/includes? mime "json") (str/includes? mime "application/vnd.ms-publisher")
               (some #(str/ends-with? mime %) ["script" "xml" "yaml" "csv" "ini" "pgp-keys"]))))

(defn- watched-type
  "The type of `types`, in offer order, that `wl-paste --watch` without `--type` pipes
  to its command: plain UTF-8 text, plain text, any text, else the first type."
  [types]
  (or (some #{"text/plain;charset=utf-8"} types) (some #{"text/plain"} types)
      (first (filter textual? types)) (first types)))

(defn- drain
  "Read `input` to its end; its bytes, or nil past `limit`. Whatever the size, every
  byte is read, so the application writing the selection is never left blocked."
  [^java.io.InputStream input limit]
  (let [buffer (byte-array 65536)
        out (java.io.ByteArrayOutputStream.)]
    (loop [kept? true]
      (let [n (.read input buffer 0 (alength buffer))]
        (cond
          (neg? n) (when kept? (.toByteArray out))
          (and kept? (<= (+ (.size out) n) limit)) (do (.write out buffer 0 n) (recur true))
          :else (do (.reset out) (recur false)))))))

(defn capture!
  "Record the clipboard change `wl-paste --watch` pipes to stdin. The pipe is read to
  its end first: the copying application writes the selection into it, and one that
  writes synchronously (Qt apps, wl-copy) stalls until it is read, unable to answer
  any other paste. Only then is the selection asked again, when the piped type is not
  the one kept. Selections a password manager marks secret are not kept."
  []
  (let [piped (drain System/in max-image-bytes)
        types (some-> (wl-paste "--list-types") (String. "UTF-8") str/split-lines vec)
        offered (set types)
        secret? (and (offered "x-kde-passwordManagerHint")
                     ;; Unreadable counts as secret.
                     (not= "public" (some-> (wl-paste "--type" "x-kde-passwordManagerHint")
                                            (String. "UTF-8") str/trim)))
        text-type (some offered text-types)
        mime (if text-type "text/plain;charset=utf-8" (some offered image-types))
        wanted (or text-type mime)]
    (when (and mime (not secret?))
      (let [out (if (= wanted (watched-type types)) piped (wl-paste "--no-newline" "--type" wanted))]
        (when (and out (pos? (alength out))
                   (<= (alength out) (if text-type max-text-bytes max-image-bytes))
                   (not (and text-type (str/blank? (String. out "UTF-8")))))
          (update-store! #(add-entry % mime out (System/currentTimeMillis))))))))

(defn watch!
  "Start `wl-paste --watch` running `command` (this build's argv) with `clipboard-capture`.
  Return the process; destroy it to stop recording."
  [command]
  (apply p/process {:in :close :out :discard :err :discard}
         "wl-paste" "--watch" (conj (vec command) "clipboard-capture")))

(defn copy!
  "Make an entry the current clipboard, with its MIME type."
  [{:keys [mime] :as entry}]
  ;; wl-copy forks a child that owns the selection and keeps whatever pipes it
  ;; was handed, so its output goes nowhere.
  @(p/process {:in (java.io.ByteArrayInputStream. (payload entry)) :out :discard :err :discard}
              "wl-copy" "--type" mime))

;; App IDs and process names: a terminal started with its own class (kitty --class pkg)
;; is still recognised by its process.
(def ^:private terminals
  #{"kitty" "foot" "footclient" "Alacritty" "alacritty" "org.wezfurlong.wezterm" "wezterm-gui"
    "com.mitchellh.ghostty" "ghostty"})

(defn- paste-keys
  "The wtype arguments for a clipboard paste in the focused window: terminals take
  Ctrl+Shift+V, Emacs Shift+Insert (its C-v scrolls), everything else Ctrl+V."
  [{:keys [app_id pid]}]
  (let [process (when pid (try (str/trim (slurp (str "/proc/" pid "/comm"))) (catch Exception _ nil)))]
    (cond (or (terminals app_id) (terminals process))
          ["-M" "ctrl" "-M" "shift" "-k" "v" "-m" "shift" "-m" "ctrl"]
          (= "emacs" app_id) ["-M" "shift" "-k" "Insert" "-m" "shift"]
          :else ["-M" "ctrl" "-k" "v" "-m" "ctrl"])))

(defn paste!
  "Make an entry the current clipboard, then paste it into the focused window."
  [entry]
  (copy! entry)
  (let [{:keys [exit out]} (p/sh {:continue true} "niri" "msg" "--json" "focused-window")
        window (when (zero? exit) (json/parse-string out true))]
    ;; The launcher has just closed; give the window its keyboard focus back first.
    (Thread/sleep 150)
    (apply p/sh {:continue true} "wtype" (paste-keys window))))
