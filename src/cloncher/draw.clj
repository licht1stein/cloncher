(ns cloncher.draw
  "Boxes, text, keycaps and icons over CUI's public API, for Cloncher's own cards.

  Cloncher's CUI build patches cui_color to five bytes {r g b a token}: it is passed
  by value in one integer register and has byte alignment in structs."
  (:require [cloncher.cui :as cui]
            [jolt.ffi :as ffi]))

(ffi/defcfn ^:private color-of "cui_color_of" [:pointer :uint64] :uint64)
(ffi/defcfn ^:private box-begin "cui_box_begin" [:pointer :pointer] :uint32)
(ffi/defcfn ^:private text* "cui_text_ex" [:pointer :string :pointer] :void)
(ffi/defcfn ^:private icon* "cui_icon" [:pointer :string :float :uint64] :void)
(ffi/defcfn ^:private icon-button* "cui_icon_button_ex" [:pointer :string :pointer] :bool)
(ffi/defcfn kbd! "cui_kbd" [:pointer :string] :void)
(ffi/defcfn font-role! "cui_font_set_role" [:pointer :int :int] :void)

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
(def ^:private justifies {:start 0 :center 1 :end 2 :between 3})
(def ^:private fonts {:body 0 :medium 1 :display 2 :mono 3})
(def ^:private tokens
  {:background 1 :surface 2 :foreground 5 :secondary 6 :muted 7 :border-subtle 9
   :accent 11 :accent-foreground 14 :danger 17})

(defn color
  "Resolve a theme token and give it `alpha`. Resolve first: CUI encodes a token as a color."
  [ui token alpha]
  (let [encoded (bit-or (tokens token) (bit-shift-left 0xC5 8) (bit-shift-left 0x1D 16))]
    (bit-or (bit-and (color-of ui encoded) 0xFFFFFF) (bit-shift-left alpha 24))))

(defn- with-struct
  "Call `f` with a zeroed struct holding `fields`. Strings are copied for the call;
  `[:color c]` values fill a field's five color bytes."
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

(defn box!
  "Open a CUI box; close it with `cui/end!`. `:dir` is :column, :row or :stack."
  [ui {:keys [dir width height align justify bg border-color clip] :as options}]
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
        justify (assoc :justify (justifies justify))
        bg (assoc :bg [:color bg])
        border-color (assoc :border-color [:color border-color]))
      #(box-begin ui %))))

(defmacro in-box
  "Draw `body` inside a box with `options`, closing it even when `body` throws."
  [ui options & body]
  `(do (box! ~ui ~options)
       (try ~@body (finally (cui/end! ~ui)))))

(defn text! [ui value {:keys [font size color nowrap? tabular?]}]
  (with-struct text-layout {:font (fonts (or font :body)) :size (double size) :color [:color color]
                            :nowrap (boolean nowrap?) :tabular (boolean tabular?)}
    #(text* ui value %)))

(defn icon! [ui glyph size color]
  (icon* ui glyph (double size) color))

(defn icon-button!
  "A ghost icon button with a tooltip; return whether it was pressed."
  [ui glyph tooltip]
  (with-struct button-layout {:variant 4 :tooltip tooltip}   ; CUI_BUTTON_GHOST
    #(icon-button* ui glyph %)))
