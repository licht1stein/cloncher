(ns cloncher.note
  "Quick capture: a translucent editor that appends a timestamped entry to the scratchpad."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [cloncher.cui :as cui]
            [cloncher.palette :as palette]
            [cloncher.process :as p]
            [cloncher.socket :as socket]
            [jolt.ffi :as ffi]))

(def width 720)
(def height 232)

;; ── entry ───────────────────────────────────────────────────────────────────

(defn- title-case [value length]
  (let [text (str/lower-case (subs (str value) 0 length))]
    (str (str/upper-case (subs text 0 1)) (subs text 1))))

(defn- org-timestamp
  "An inactive Org timestamp, `[2026-11-22 Sun 14:05]`."
  [^java.time.LocalDateTime at]
  (format "[%04d-%02d-%02d %s %02d:%02d]" (.getYear at) (.getMonthValue at) (.getDayOfMonth at)
          (title-case (.getDayOfWeek at) 3) (.getHour at) (.getMinute at)))

(defn- clock [^java.time.LocalDateTime at]
  (format "%s %d %s · %02d:%02d" (title-case (.getDayOfWeek at) 3) (.getDayOfMonth at)
          (title-case (.getMonth at) 3) (.getHour at) (.getMinute at)))

(defn entry
  "The Org text appended for `text` written at `at`, or nil when there is nothing to keep."
  [text at]
  (when-let [body (not-empty (str/trim (str/replace (str text) "\r\n" "\n")))]
    (str "* " (org-timestamp at) "\n" body "\n")))

(defn append!
  "Append `text` to the Org file at `path` under a timestamp heading. Return the entry."
  [path text at]
  (when-let [entry (entry text at)]
    (let [file (fs/path path)]
      (some-> (fs/parent file) fs/create-dirs)
      (let [existing (when (fs/exists? file) (slurp (str file)))
            separator (if (or (empty? existing) (str/ends-with? existing "\n")) "" "\n")]
        (spit (str file) (str separator entry) :append true)))
    entry))

;; ── CUI structs ─────────────────────────────────────────────────────────────
;; Cloncher's CUI build patches cui_color to five bytes {r g b a token}: it is
;; passed by value in one integer register and has byte alignment in structs.

(ffi/defcfn ^:private color-of "cui_color_of" [:pointer :uint64] :uint64)
(ffi/defcfn ^:private box-begin "cui_box_begin" [:pointer :pointer] :uint32)
(ffi/defcfn ^:private text* "cui_text_ex" [:pointer :string :pointer] :void)
(ffi/defcfn ^:private icon* "cui_icon" [:pointer :string :float :uint64] :void)
(ffi/defcfn ^:private icon-button* "cui_icon_button_ex" [:pointer :string :pointer] :bool)
(ffi/defcfn ^:private kbd! "cui_kbd" [:pointer :string] :void)
(ffi/defcfn ^:private theme "cui_theme_get" [:pointer] :pointer)
(ffi/defcfn ^:private font-role! "cui_font_set_role" [:pointer :int :int] :void)

;; (35 tokens + 7 tones) × 5-byte colors, three radii, three control heights, text_xs, text_sm.
(def ^:private text-body-offset 244)

;; `ffi/layout` takes a literal descriptor, so each color is spelled out as its five bytes.
(def ^:private box-layout
  (ffi/layout [:struct [[:unused :int] [:id :pointer] [:dir :int]
                        [:width-kind :int] [:width :float] [:height-kind :int] [:height :float]
                        [:min-width :float] [:max-width :float] [:min-height :float] [:max-height :float]
                        [:pad :float] [:pad-x :float] [:pad-y :float]
                        [:pad-top :float] [:pad-right :float] [:pad-bottom :float] [:pad-left :float]
                        [:gap :float] [:columns :int] [:min-column-width :float] [:wrap :bool]
                        [:align :int] [:justify :int]
                        [:bg-0 :uint8] [:bg-1 :uint8] [:bg-2 :uint8] [:bg-3 :uint8] [:bg-4 :uint8]
                        [:border-color-0 :uint8] [:border-color-1 :uint8] [:border-color-2 :uint8]
                        [:border-color-3 :uint8] [:border-color-4 :uint8]
                        [:border :float] [:radius :float]
                        [:radius-tl :float] [:radius-tr :float] [:radius-br :float] [:radius-bl :float]
                        [:elevation :int] [:scroll-x :bool] [:scroll-y :bool] [:clip :bool]
                        [:opacity :float] [:direction :int]]]))

(def ^:private text-layout
  (ffi/layout [:struct [[:unused :int] [:font :int] [:size :float]
                        [:color-0 :uint8] [:color-1 :uint8] [:color-2 :uint8] [:color-3 :uint8] [:color-4 :uint8]
                        [:align :int] [:nowrap :bool] [:tabular :bool] [:selectable :bool]
                        [:width-kind :int] [:width :float] [:line-height :float] [:direction :int]]]))

(def ^:private button-layout
  (ffi/layout [:struct [[:unused :int] [:variant :int] [:size :int] [:icon :pointer] [:icon-right :pointer]
                        [:disabled :bool] [:loading :bool] [:full-width :bool] [:selected :bool]
                        [:tooltip :pointer] [:shortcut :pointer] [:width-kind :int] [:width :float]
                        [:color-0 :uint8] [:color-1 :uint8] [:color-2 :uint8] [:color-3 :uint8] [:color-4 :uint8]]]))

(def ^:private sizes {:fit 0 :fill 1 :px 2})
(def ^:private aligns {:start 1 :center 2 :end 3 :stretch 4})
(def ^:private fonts {:body 0 :medium 1 :display 2 :mono 3})
(def ^:private tokens
  {:background 1 :surface 2 :foreground 5 :muted 7 :border-subtle 9
   :accent 11 :accent-foreground 14 :danger 17})

(def ^:private icons
  {:notebook "\uE596" :close "\uE1B2"})

(defn- color
  "Resolve a theme token and give it `alpha`. Resolve first: CUI encodes a token as a color."
  [ui token alpha]
  (let [encoded (bit-or (tokens token) (bit-shift-left 0xC5 8) (bit-shift-left 0x1D 16))]
    (bit-or (bit-and (color-of ui encoded) 0xFFFFFF) (bit-shift-left alpha 24))))

(defn- with-struct
  "Call `f` with a zeroed struct holding `fields`. Strings are copied for the call;
  keys named in `colors` take packed colors."
  [layout fields f]
  (let [size (ffi/sizeof layout)
        memory (ffi/alloc size)
        strings (volatile! [])]
    (try
      (dotimes [i size] (ffi/write memory :uint8 0 i))
      (doseq [[field value] fields]
        (cond
          (string? value)
          (let [pointer (ffi/string->ptr value)]
            (vswap! strings conj pointer)
            (ffi/write-field memory layout field pointer))
          (and (vector? value) (= :color (first value)))
          (dotimes [i 5]
            (ffi/write-field memory layout (keyword (str (name field) "-" i))
                             (bit-and 0xFF (bit-shift-right (second value) (* 8 i)))))
          :else (ffi/write-field memory layout field value)))
      (f memory)
      (finally
        (run! ffi/free @strings)
        (ffi/free memory)))))

(def ^:private float-fields
  [:pad :pad-x :pad-y :pad-top :pad-right :pad-bottom :pad-left :gap :border :radius
   :radius-tl :radius-tr :radius-br :radius-bl])

(defn- box!
  "Open a CUI box; close it with `cui/end!`. `:dir` is :column, :row or :stack."
  [ui {:keys [dir width height align bg border-color clip] :as options}]
  (let [[width-kind width] (or width [:fit 0])
        [height-kind height] (or height [:fit 0])]
    (with-struct box-layout
      (cond-> (into {:dir ({:column 0 :row 1 :stack 2} (or dir :column))
                     :width-kind (sizes width-kind) :width (double width)
                     :height-kind (sizes height-kind) :height (double height)
                     :clip (boolean clip)}
                    (keep (fn [field] (some->> (options field) double (vector field))))
                    float-fields)
        align (assoc :align (aligns align))
        bg (assoc :bg [:color bg])
        border-color (assoc :border-color [:color border-color]))
      #(box-begin ui %))))

(defmacro ^:private in-box [ui options & body]
  `(do (box! ~ui ~options)
       (try ~@body (finally (cui/end! ~ui)))))

(defn- text! [ui value {:keys [font size color nowrap? tabular?]}]
  (with-struct text-layout {:font (fonts (or font :body)) :size (double size) :color [:color color]
                            :nowrap (boolean nowrap?) :tabular (boolean tabular?)}
    #(text* ui value %)))

(defn- close-button! [ui tooltip]
  (with-struct button-layout {:variant 4 :tooltip tooltip}   ; CUI_BUTTON_GHOST
    #(icon-button* ui (icons :close) %)))

(defn- editor!
  "The text field at a reading size. CUI sizes fields from the theme's body text."
  [ui buffer]
  (let [theme (theme ui)
        body (ffi/read theme :float text-body-offset)]
    (ffi/write theme :float 17.0 text-body-offset)
    (try (cui/editor-field! ui "note-editor" buffer "What’s on your mind?")
         (finally (ffi/write theme :float body text-body-offset)))))

;; ── card ────────────────────────────────────────────────────────────────────

(defn render!
  "Draw the note card, filling its parent. Return :save on Ctrl+Enter, :cancel on Escape or close."
  [ui buffer {:keys [title at error]}]
  ;; Body text in the launcher's monospace, as its prompt and rows are.
  (font-role! ui 0 3)
  (let [foreground (color ui :foreground 255)
        muted (color ui :muted 255)
        accent (color ui :accent 255)
        escape? (contains? (cui/take-keys! ui #{:escape}) :escape)
        radius 22.0
        action (volatile! (when escape? :cancel))]
    (in-box ui {:width [:fill 1] :height [:fill 1] :radius radius :clip true
                :bg (color ui :surface 222) :border 1 :border-color (color ui :border-subtle 170)}
      (in-box ui {:dir :row :width [:fill 1] :gap 10 :align :center
                  :pad-top 14 :pad-left 18 :pad-right 12}
        (in-box ui {:width [:px 28] :height [:px 28] :radius 8 :align :center :pad-top 6 :bg accent}
          (icon* ui (icons :notebook) 16.0 (color ui :accent-foreground 255)))
        (text! ui title {:font :medium :size 15 :color foreground :nowrap? true})
        (text! ui (clock at) {:size 13 :color muted :tabular? true :nowrap? true})
        (cui/spacer! ui)
        (when (close-button! ui "Cancel · Esc")
          (vreset! action :cancel)))
      (in-box ui {:width [:fill 1] :height [:fill 1] :pad-left 22 :pad-right 22 :pad-top 4}
        (when (:submitted? (editor! ui buffer))
          (vreset! action :save)))
      (in-box ui {:dir :row :width [:fill 1] :gap 10 :align :center :pad-y 10 :pad-left 22 :pad-right 14
                  :bg (color ui :background 120) :radius-bl radius :radius-br radius}
        (if error
          (text! ui error {:size 12 :color (color ui :danger 255) :nowrap? true})
          (text! ui (let [words (count (re-seq #"\S+" (cui/input-value buffer)))]
                      (str words (if (= 1 words) " word" " words")))
                 {:size 12 :color muted :tabular? true :nowrap? true}))
        (cui/spacer! ui)
        (kbd! ui "Ctrl Enter")
        (text! ui "save" {:size 12 :color muted :nowrap? true})))
    @action))

;; ── overlay process ─────────────────────────────────────────────────────────

(def ^:private unavailable
  "The note editor needs a Wayland compositor with layer-shell support and a current Cloncher CUI host.")

(defn- endpoint [] (str (fs/path (fs/parent (socket/path)) "note.sock")))

(defn start!
  "Open the editor for the Org file at `path` in its own process. An open editor stays as it is."
  [path]
  (when-not (contains? #{"ready" "busy"} (p/start-helper! ["--note-editor" (str path)] 6000))
    (throw (ex-info unavailable {}))))

(defn worker!
  "Run the editor window until the note is saved or dismissed. The socket lock keeps one editor open."
  [path load-cui!]
  (if-let [listener (socket/listen! (endpoint))]
    (try
      (load-cui!)
      (let [ready? (volatile! false)
            colors (volatile! {})
            reload-at (volatile! 0)
            error (volatile! nil)]
        (cui/window! {:app-id "cloncher.note" :title "Cloncher Note" :width width :height height}
          (fn [ui buffer]
            (let [now (System/currentTimeMillis)
                  at (java.time.LocalDateTime/now)]
              (when (>= now @reload-at)
                (vreset! colors (palette/load-colors))
                (vreset! reload-at (+ now 1000)))
              (cui/apply-colors! ui @colors)
              (case (render! ui buffer {:title "Scratchpad" :at at :error @error})
                ;; A failed write keeps the window and its text.
                :save (try (append! path (cui/input-value buffer) at)
                           (cui/quit!)
                           (catch Exception failure
                             (vreset! error (str "Not saved · " (ex-message failure)))
                             (cui/redraw! ui)))
                :cancel (cui/quit!)
                nil)
              (when-not @ready?
                (vreset! ready? true)
                (println "ready")
                (flush))
              ;; Once a second: the header clock turns over and the palette follows theme changes.
              (cui/wake-in! ui 1.0)))))
      (finally (socket/stop! listener)))
    (do (println "busy") (flush))))
