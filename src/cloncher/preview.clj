(ns cloncher.preview
  "Bounded file previews. Archive entries are read as data and are never extracted."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cloncher.process :as p]))

(def ^:private max-sample (* 64 1024))
(def ^:private max-part (* 4 1024 1024))
(def ^:private max-archive (* 128 1024 1024))
(def ^:private max-image (* 64 1024 1024))

(defn- read-prefix [input limit line-limit]
  (let [buffer (byte-array 8192)
        out (java.io.ByteArrayOutputStream.)]
    (loop [lines 0]
      (let [budget (min (- limit (.size out)) (alength buffer))]
        (if (and (pos? budget) (or (nil? line-limit) (< lines line-limit))
                 (not (.isInterrupted (Thread/currentThread))))
          (let [n (.read input buffer 0 (int budget))]
            (if (neg? n)
              (.toByteArray out)
              (let [[length lines] (if line-limit
                                     (loop [i 0 lines lines]
                                       (if (or (= i n) (>= lines line-limit)) [i lines]
                                           (recur (inc i) (+ lines (if (= 10 (aget buffer i)) 1 0)))))
                                     [n lines])]
                (.write out buffer 0 (int length))
                (recur lines))))
          (.toByteArray out))))))

(defn- decoded [value]
  (-> value
      (str/replace "&#39;" "'")
      (str/replace "&apos;" "'")
      (str/replace "&quot;" "\"")
      (str/replace "&lt;" "<")
      (str/replace "&gt;" ">")
      (str/replace "&nbsp;" " ")
      (str/replace "&amp;" "&")))

(defn- plain-markup [xml]
  (-> xml
      (str/replace #"(?is)<(?:[\w.-]+:)?(?:script|style|head)(?:\s[^>]*|)>.*?</(?:[\w.-]+:)?(?:script|style|head)\s*>" "")
      (str/replace #"(?is)</(?:[\w.-]+:)?(?:p|h[1-6]|li|tr|row|table-row|section|div)>|<(?:[\w.-]+:)?(?:br|line-break)(?:\s[^>]*|)/?>" "\n")
      (str/replace #"(?s)</(?:[\w.-]+:)?(?:td|tc|c|table-cell)>" "\t")
      (str/replace #"(?s)<[^>]+>" "")
      decoded
      (str/replace #"(?m)^[\t ]+|[\t ]+$" "")
      (str/replace #"\n{3,}" "\n\n")
      str/trim))

(defn- archive-part [archive entry remaining]
  (when-let [part (.getEntry archive entry)]
    (with-open [input (.getInputStream archive part)]
      (let [data (read-prefix input (min max-part @remaining) nil)]
        (vswap! remaining - (alength data))
        (String. data "UTF-8")))))

(defn- epub-order [archive parts read-part]
  (let [container (read-part "META-INF/container.xml")
        manifest-file (some->> container (re-find #"full-path=[\"']([^\"']+)[\"']") second)
        manifest (when manifest-file (read-part manifest-file))
        parent (if manifest-file (or (fs/parent manifest-file) "") "")
        by-id (into {}
                    (keep (fn [tag]
                            (when-let [id (second (re-find #"\bid=[\"']([^\"']+)[\"']" tag))]
                              (when-let [file (second (re-find #"\bhref=[\"']([^\"'#]+)[\"']" tag))]
                                [id (str (fs/normalize (fs/path parent (decoded file))))]))))
                    (re-seq #"<(?:\w+:)?item\s+[^>]*>" (or manifest "")))
        ordered (keep (fn [[_ id]] (get by-id id))
                      (re-seq #"<(?:\w+:)?itemref\s+[^>]*\bidref=[\"']([^\"']+)[\"']" (or manifest "")))]
    (or (seq ordered)
        (filter #(and (re-find #"(?i)\.x?html?$" %) (some? (.getEntry archive %))) parts))))

(defn- spreadsheet-text [xml shared]
  (->> (re-seq #"(?s)<c\b([^>]*?)(?:/>|>(.*?)</c>)" xml)
       (keep (fn [[_ attrs content]]
               (when (seq content)
                 (let [ref (second (re-find #"\br=\"([^\"]+)\"" attrs))
                       value (some->> content (re-find #"(?s)<v>(.*?)</v>") second decoded)
                       display (if (str/includes? attrs "t=\"s\"")
                                 (get shared (or (some-> value parse-long) -1) value)
                                 (or value (plain-markup (str/replace content #"(?s)<f\b[^>]*>.*?</f>" ""))))]
                   (when (seq display) (str ref "  " display))))))
       (take 60)
       (str/join "\n")))

(defn- document-preview [path ext]
  (when (> (fs/size path) max-archive)
    (throw (ex-info "Document exceeds the 128 MiB preview limit" {})))
  (with-open [archive (java.util.zip.ZipFile. path)]
    (when (> (.size archive) 8192)
      (throw (ex-info "Document has too many archive entries" {})))
    (let [parts (into [] (map #(.getName %)) (enumeration-seq (.entries archive)))
          remaining (volatile! (* 16 1024 1024))
          start (System/nanoTime)
          read-part (fn [entry]
                      (when (and (pos? @remaining)
                                 (< (- (System/nanoTime) start) (* 2 1000 1000 1000)))
                        (archive-part archive entry remaining)))
          sorted-parts #(sort-by (fn [name] (or (some-> (re-find #"\d+" name) parse-long) 0))
                                 (filter (partial re-matches %) parts))]
      (case ext
        "epub" (->> (epub-order archive parts read-part) (take 8) (keep read-part) (map plain-markup)
                    (str/join "\n\n"))
        ("docx" "docm" "dotx") (some-> (read-part "word/document.xml") plain-markup)
        ("xlsx" "xlsm" "xltx")
        (let [shared (mapv (comp plain-markup first)
                           (re-seq #"(?s)<si\b[^>]*>.*?</si>" (or (read-part "xl/sharedStrings.xml") "")))]
          (->> (sorted-parts #"xl/worksheets/sheet\d+\.xml") (take 3) (keep read-part)
               (map #(spreadsheet-text % shared)) (str/join "\n\n")))
        ("pptx" "pptm" "potx")
        (->> (sorted-parts #"ppt/slides/slide\d+\.xml") (take 8) (keep read-part)
             (map plain-markup) (str/join "\n\n"))
        ("odt" "ods" "odp") (some-> (read-part "content.xml") plain-markup)))))

(def ^:private documents
  #{"epub" "docx" "docm" "dotx" "xlsx" "xlsm" "xltx" "pptx" "pptm" "potx" "odt" "ods" "odp"})

(defn- renderer! [path mime output]
  (let [args (if (= mime "application/pdf")
               ["pdftoppm" "-f" "1" "-l" "1" "-singlefile" "-scale-to" "720" "-png" path
                (str/replace output #"\.png$" "")]
               ["ffmpeg" "-v" "error" "-nostdin" "-protocol_whitelist" "file,pipe"
                "-threads" "1" "-i" path "-frames:v" "1" "-vf"
                "scale=720:720:force_original_aspect_ratio=decrease"
                "-threads" "1" "-f" "image2" "-update" "1" "-y" output])]
    (when-not (zero? (:exit (apply p/sh {:continue true :in ""}
                                  "timeout" "-k" "1" "5" args)))
      (throw (ex-info "Could not decode a preview" {})))
    (when-not (and (fs/regular-file? output) (<= (fs/size output) (* 8 1024 1024)))
      (throw (ex-info "Preview exceeds the image limit" {})))))

(defn- render-thumbnail [path mime]
  (when (and (str/starts-with? mime "image/") (> (fs/size path) max-image))
    (throw (ex-info "Image exceeds 64 MiB" {})))
  (let [dest (fs/create-temp-file {:prefix "cloncher-preview-" :suffix ".png"})]
    (try
      (renderer! path mime (str dest))
      (with-open [in (io/input-stream (str dest))]
        (read-prefix in (* 8 1024 1024) nil))
      (finally (fs/delete-if-exists dest)))))

(defn load-preview [path]
  (try
    (when-not (fs/regular-file? path)
      (throw (ex-info "The file is no longer available" {})))
    (let [{:keys [out exit]} (p/sh {:continue true} "timeout" "2" "file" "--brief" "--mime-type" "--" path)
          mime (if (zero? exit) (str/trim out) "application/octet-stream")
          ext (some-> (re-find #"\.([^.]+)$" (str (fs/file-name path))) second str/lower-case)
          detail (str (fs/size path) " B  ·  " mime)]
      (cond
        (documents ext)
        (let [text (or (document-preview path ext) "")]
          {:detail detail :lines (vec (take 60 (str/split-lines (subs text 0 (min max-sample (count text))))))})
        (or (= mime "application/pdf") (str/starts-with? mime "video/")
            (str/starts-with? mime "image/"))
        {:bytes (render-thumbnail path mime) :detail detail}
        :else
        (with-open [in (io/input-stream path)]
          (let [sample (read-prefix in max-sample 60)
                text (String. sample "UTF-8")]
            {:detail detail
             :lines (when-not (or (some zero? sample) (str/includes? text "�"))
                      (str/split-lines text))}))))
    (catch Exception error
      {:notice (or (ex-message error) "Preview unavailable")})))
