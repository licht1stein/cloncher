(ns cloncher.panel
  "Compact panel primitives over CUI's public box, text, and image ABI."
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn ^:private box* "cui_box_begin" [:pointer :pointer] :uint32)
(ffi/defcfn ^:private text* "cui_text_ex" [:pointer :string :pointer] :void)
(ffi/defcfn ^:private image* "cui_image_ex" [:pointer :uint64 :pointer] :void)
(ffi/defcfn ^:private mouse* "cui_mouse_pos" [:pointer]
  [:by-value [:struct [[:x :float] [:y :float]]]])
(ffi/defcfn ^:private rect* "cui_get_rect" [:pointer :uint32]
  [:by-value [:struct [[:x :float] [:y :float] [:w :float] [:h :float]]]])
(ffi/defcfn ^:private mouse-clicked? "cui_mouse_clicked" [:pointer :int] :bool)


(def ^:private box-layout
  (ffi/layout
    [:struct [[:unused :int] [:id :pointer] [:dir :int] [:width [:struct [[:kind :int] [:value :float]]]] [:height [:struct [[:kind :int] [:value :float]]]]
              [:min-width :float] [:max-width :float] [:min-height :float] [:max-height :float]
              [:pad :float] [:pad-x :float] [:pad-y :float] [:pad-top :float] [:pad-right :float]
              [:pad-bottom :float] [:pad-left :float] [:gap :float] [:columns :int] [:min-column-width :float]
              [:wrap :bool] [:align :int] [:justify :int] [:bg [:struct [[:r :uint8] [:g :uint8] [:b :uint8] [:a :uint8] [:token :uint8]]]] [:border-color [:struct [[:r :uint8] [:g :uint8] [:b :uint8] [:a :uint8] [:token :uint8]]]]
              [:border :float] [:radius :float] [:radius-tl :float] [:radius-tr :float] [:radius-br :float]
              [:radius-bl :float] [:elevation :int] [:scroll-x :bool] [:scroll-y :bool] [:clip :bool]
              [:opacity :float] [:direction :int]]]))

(def ^:private text-layout
  (ffi/layout
    [:struct [[:unused :int] [:font :int] [:size :float] [:color [:struct [[:r :uint8] [:g :uint8] [:b :uint8] [:a :uint8] [:token :uint8]]]] [:align :int]
              [:nowrap :bool] [:tabular :bool] [:selectable :bool] [:width [:struct [[:kind :int] [:value :float]]]]
              [:line-height :float] [:direction :int]]]))

(def ^:private image-layout
  (ffi/layout [:struct [[:unused :int] [:width [:struct [[:kind :int] [:value :float]]]] [:height [:struct [[:kind :int] [:value :float]]]]
                        [:radius :float] [:tint [:struct [[:r :uint8] [:g :uint8] [:b :uint8] [:a :uint8] [:token :uint8]]]]]]))

(def ^:private rect-layout (ffi/layout [:struct [[:x :float] [:y :float] [:w :float] [:h :float]]]))
(def ^:private mouse-layout (ffi/layout [:struct [[:x :float] [:y :float]]]))

(def ^:private tokens
  {:background 1 :surface 2 :raised 3 :foreground 5 :secondary 6 :muted 7 :border 9
   :accent 11 :accent-text 14 :badge 15 :badge-text 16 :danger 17 :selected-text 18
   :active 26 :hover 25})

(defn- token [color]
  {:r (get tokens color 0) :g 197 :b 29 :a 0 :token 0})

(defn- write-size! [pointer layout field size]
  (let [[kind value] (cond (= :fill size) [1 1] (number? size) [2 size] :else [0 0])]
    (ffi/write pointer (ffi/place layout field) {:kind kind :value (double value)})))

(defn begin!
  "Open a box; close it with `cui/end!`. A `:stack?` box overlaps its children.
  `:radius-bl` and `:radius-br` round a bottom corner on their own."
  [ui id {:keys [row? stack? width height pad-x pad-y gap background border radius radius-bl radius-br clip?]
          :or {width :fill height :fit pad-x 0 pad-y 0 gap 0}}]
  (let [options (ffi/alloc (ffi/sizeof box-layout))
        c-id (ffi/string->ptr id)]
    (try
      (ffi/write-field options box-layout :id c-id)
      (ffi/write-field options box-layout :dir (cond stack? 2 row? 1 :else 0))
      (write-size! options box-layout :width width)
      (write-size! options box-layout :height height)
      (doseq [[field value] [[:pad-x pad-x] [:pad-y pad-y] [:gap gap]
                             [:border (or border 0)] [:radius (or radius 0)]
                             [:radius-bl (or radius-bl 0)] [:radius-br (or radius-br 0)]]]
        (ffi/write-field options box-layout field (double value)))
      (ffi/write-field options box-layout :align (if row? 2 4))
      (when background (ffi/write options (ffi/place box-layout :bg) (token background)))
      (when border (ffi/write options (ffi/place box-layout :border-color) (token :border)))
      (when clip? (ffi/write-field options box-layout :clip true))
      (box* ui options)
      (finally (ffi/free options) (ffi/free c-id)))))

(defn text!
  [ui text {:keys [color width size center? right? font]
            :or {size 13 width :fit font 3}}]
  (let [options (ffi/alloc (ffi/sizeof text-layout))]
    (try
      (ffi/write-field options text-layout :font font)
      (ffi/write-field options text-layout :size (double size))
      (ffi/write-field options text-layout :align (cond right? 2 center? 1 :else 0))
      (ffi/write-field options text-layout :nowrap true)
      (ffi/write-field options text-layout :line-height 1.16)
      (write-size! options text-layout :width width)
      (when color (ffi/write options (ffi/place text-layout :color) (token color)))
      (text* ui text options)
      (finally (ffi/free options)))))

(defn image! [ui texture size]
  (let [options (ffi/alloc (ffi/sizeof image-layout))]
    (try
      (write-size! options image-layout :width size)
      (write-size! options image-layout :height size)
      (image* ui texture options)
      (finally (ffi/free options)))))

(defn clicked? [ui id]
  (when (mouse-clicked? ui 0)
    (let [mouse (ffi/alloc (ffi/sizeof mouse-layout))
          rect (ffi/alloc (ffi/sizeof rect-layout))]
      (try
        ;; Jolt passes caller-owned return storage as the first argument.
        #_{:clj-kondo/ignore [:invalid-arity]}
        (mouse* mouse ui)
        #_{:clj-kondo/ignore [:invalid-arity]}
        (rect* rect ui id)
        (let [{mx :x my :y} (ffi/read mouse mouse-layout)
              {:keys [x y w h]} (ffi/read rect rect-layout)]
          (and (<= x mx) (< mx (+ x w)) (<= y my) (< my (+ y h))))
        (finally (ffi/free mouse) (ffi/free rect))))))

(ffi/defcfn ^:private virtual-list* "cui_virtual_list_begin"
  [:pointer :string :int :float [:by-value [:struct [[:kind :int] [:value :float]]]] :pointer :pointer]
  :void)
(ffi/defcfn end-list! "cui_virtual_list_end" [:pointer] :void)
(ffi/defcfn scroll-to! "cui_scroll_to" [:pointer :string :float] :void)

(def ^:private list-size (ffi/layout [:struct [[:kind :int] [:value :float]]]))

(defn virtual-list! [ui id count row-height viewport-height]
  (let [size (ffi/alloc (ffi/sizeof list-size))
        first (ffi/alloc 4)
        last (ffi/alloc 4)]
    (try
      (ffi/write size list-size {:kind 2 :value (double viewport-height)})
      (virtual-list* ui id count (double row-height) size first last)
      [(ffi/read first :int) (ffi/read last :int)]
      (finally (ffi/free size) (ffi/free first) (ffi/free last)))))
