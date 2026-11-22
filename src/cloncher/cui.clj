(ns cloncher.cui
  "CUI host ABI and native ownership."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [jolt.ffi :as ffi]))

(ffi/defcfn ^:private run-app "cui_host_run_app_wake"
  [:pointer :pointer :pointer :int :int :pointer :pointer :pointer] :int :blocking)
(ffi/defcfn ^:private create-waker "cui_host_frame_waker_new" [] :pointer)
(ffi/defcfn ^:private free-waker "cui_host_frame_waker_free" [:pointer] :void)
(ffi/defcfn ^:private wake-window! "cui_host_request_frame" [:pointer] :bool :blocking)
(ffi/defcfn process-id "getpid" [] :int)
(ffi/defcfn quit! "cui_host_quit" [] :void)
(ffi/defcfn ^:private app-size! "cui_app_set_size" [:pointer :int :int] :void)
(ffi/defcfn ^:private app-backend "cui_app_backend" [:pointer] :string)
(ffi/defcfn ^:private native-window "cui_app_native_window" [:pointer] :pointer)
(ffi/defcfn ^:private sdl-minimum-size! "SDL_SetWindowMinimumSize" [:pointer :int :int] :bool)
(ffi/defcfn ^:private layer-active* "cloncher_layer_active" [] :bool)
(ffi/defcfn ^:private prepare-launcher* "cloncher_prepare_launcher" [:int :int] :void :blocking)
(ffi/defcfn ^:private use-prepared-launcher* "cloncher_use_prepared_launcher" [] :void)

(defn prepare-launcher! [width height]
  (try (prepare-launcher* width height) (catch Exception _ nil)))

(defn layer-active? []
  (try (layer-active*) (catch Exception _ false)))

(defn allow-compact-window!
  "Override the desktop host's default 280-point minimum on Wayland."
  [width height]
  (when (= "wayland" (app-backend ffi/null))
    (sdl-minimum-size! (native-window ffi/null) width height)))

(defn resize-window!
  "Resize the CUI host's current window in logical points. Call on its frame thread."
  [width height]
  (app-size! ffi/null width height))
(ffi/defcfn focused? "cui_host_window_focused" [:pointer] :bool)
(ffi/defcfn wake-in! "cui_request_wakeup" [:pointer :float] :void)
(ffi/defcfn redraw! "cui_request_redraw" [:pointer] :void)
(ffi/defcfn ^:private field* "cui_host_field"
  [:pointer :string :pointer :int :string :string :string :string :int] :int)
(ffi/defcfn ^:private make-id "cui_make_id" [:pointer :string] :uint32)
(ffi/defcfn ^:private bare-field* "cui__text_field"
  [:pointer :uint32 :pointer :size_t :pointer :bool :int :pointer] :int)
(ffi/defcfn ^:private font-role! "cui_font_set_role" [:pointer :int :int] :void)
(ffi/defcfn ^:private container* "cui_host_container_begin"
  [:pointer :string :int :int :int :int :int :int] :void)
(ffi/defcfn end! "cui_host_box_end" [:pointer] :void)
(ffi/defcfn caption! "cui_host_caption" [:pointer :string] :void)
(ffi/defcfn text! "cui_host_text" [:pointer :string] :void)
(ffi/defcfn heading! "cui_host_heading" [:pointer :string :int] :void)
(ffi/defcfn separator! "cui_host_separator" [:pointer] :void)
(ffi/defcfn spacer! "cui_spacer" [:pointer] :void)
(ffi/defcfn spinner! "cui_spinner" [:pointer :float] :void)
(ffi/defcfn ^:private button* "cui_host_button_ex" [:pointer :string :int :int :int] :bool)
(ffi/defcfn height "cui_host_viewport_height" [:pointer] :int)
(ffi/defcfn width "cui_host_viewport_width" [:pointer] :int)
(ffi/defcfn icon-action! "cui_host_icon_action" [:pointer :int :string :int] :bool)
(ffi/defcfn ^:private list-row* "cui_host_list_row"
  [:pointer :string :string :string :bool :int :string :string] :int)
(ffi/defcfn ^:private key-pressed? "cui_key_pressed" [:pointer :int] :bool)
(ffi/defcfn ^:private field-caret "cui_host_composer_caret" [:pointer :string] :int)
(ffi/defcfn ^:private restore-caret! "cui_host_composer_set_caret" [:pointer :string :int] :void)
(ffi/defcfn ^:private modifiers "cui_key_mods" [:pointer] :uint32)
(ffi/defcfn ^:private memset "memset" [:pointer :int :size_t] :pointer)
(ffi/defcfn ^:private setenv "setenv" [:string :string :int] :int)
(ffi/defcfn ^:private take-keys* "cui_host_take_keys" [:pointer :int] :int)
(ffi/defcfn scroll! "cui_host_scroll_begin" [:pointer :string] :void)
(ffi/defcfn end-scroll! "cui_host_scroll_end" [:pointer] :void)
(ffi/defcfn markdown! "cui_host_markdown" [:pointer :string :string] :void)
(ffi/defcfn ^:private image-info "cui_host_image_file_info" [:pointer :string :pointer] :uint64)
(ffi/defcfn ^:private svg-texture "cui_host_svg_texture" [:pointer :string :int] :uint64)
(ffi/defcfn ^:private image-file "cui_host_image_file" [:pointer :string] :uint64)
(ffi/defcfn ^:private thumbnail* "cui_host_attachment"
  [:pointer :string :int :string :string :uint64 :int :int :float :int] :int)
(ffi/defcfn destroy-texture! "cui_host_texture_destroy" [:pointer :uint64] :void)
(ffi/defcfn ^:private theme-colors "cui_theme_get" [:pointer] :pointer)
(ffi/defcfn capture! "cui_host_capture" [:string] :void)
(ffi/defcfn capture-result "cui_host_capture_result" [] :int)

(defn apply-colors! [ui colors]
  (when (seq colors)
    (let [target (theme-colors ui)]
      (doseq [[token rgba] colors
              [component value] (map-indexed vector rgba)]
        (let [offset (+ (* token 5) component)]
          (when (not= value (ffi/read target :uint8 offset))
            (ffi/write target :uint8 value offset)))))))

(defn load! [path]
  (ffi/load-library path))

(defn column!
  ([ui id] (column! ui id {}))
  ([ui id {:keys [width height pad gap surface?]
           :or {width -1 height -1 pad 18 gap 6}}]
   (container* ui id 0 width height pad gap (if surface? 1 0))))

(defn button! [ui label primary?]
  (button* ui label (if primary? 2 3) -1 8))

(defn bar! [ui id]
  (container* ui id 1 -1 0 0 8 0))

(defn result-row! [ui id title subtitle selected? selectable?]
  (if selectable?
    (bit-test (list-row* ui id title subtitle selected? -1 "" "") 0)
    (do (caption! ui (if (seq subtitle) (str title "  ·  " subtitle) title)) false)))

(defn thumbnail! [ui id {:keys [texture width height]} maximum]
  (thumbnail* ui id 0 "" "" texture width height (double maximum) 0)
  nil)

(defn icon-texture! [ui path]
  (try
    (let [texture (if (.endsWith ^String path ".svg")
                    (svg-texture ui (slurp path) 48)
                    (image-file ui path))]
      (when (pos? texture) texture))
    (catch Exception _ nil)))

(defn svg-thumbnail!
  "Rasterize `svg` at `size` points from memory, never through a file."
  [ui svg size]
  (let [texture (svg-texture ui svg size)]
    (when (pos? texture) {:texture texture :width size :height size})))

(defn load-thumbnail! [ui bytes]
  (let [temporary (fs/create-temp-file {:prefix "cloncher-image-" :suffix ".png"})
        size (ffi/alloc 8)]
    (try
      (with-open [out (io/output-stream (str temporary))]
        (.write out bytes 0 (alength bytes)))
      (let [texture (image-info ui (str temporary) size)]
        (when (pos? texture)
          {:texture texture :width (ffi/read size :int) :height (ffi/read size :int 4)}))
      (finally
        (ffi/free size)
        (fs/delete-if-exists temporary)))))

(defn fallback-font! [path]
  (let [existing (System/getenv "TEMPLATE_TEXT_FONTS")]
    (setenv "TEMPLATE_TEXT_FONTS" (if (seq existing) (str path ":" existing) path) 1)))

(defn shortcuts [ui jump?]
  (let [mods (modifiers ui)
        control? (bit-test mods 1)
        alt? (bit-test mods 2)
        pressed? #(key-pressed? ui %)
        letter-pressed #(some (fn [[i ch]] (when (pressed? (+ 15 i)) (str ch)))
                              (map-indexed vector "abcdefghijklmnopqrstuvwxyz0123456789"))
        letter (when (or jump? alt?) (letter-pressed))]
    {:control? control? :alt? alt? :letter letter
     :control-letter (when control? (letter-pressed))
     :shift? (bit-test mods 0)
     :copy? (and control? (pressed? 17))
     :jump-cancel? (and control? (pressed? 21))
     :history? (and control? (pressed? 32))
     :next? (and control? (or (pressed? 24) (pressed? 28)))
     :previous? (and control? (or (pressed? 25) (pressed? 30)))
     :page-up? (pressed? 13)
     :page-down? (pressed? 14)}))

(defn caret
  "CUI uses UTF-8 byte offsets. Return a character offset for Jolt string operations."
  [ui id text]
  (let [n (field-caret ui id)
        bytes (.getBytes ^String text "UTF-8")]
    (when (<= 0 n (alength bytes))
      (count (String. bytes 0 n "UTF-8")))))

(def ^:private capacity 65536)

(defn clear-input! [buffer]
  (memset buffer 0 capacity)
  nil)

(defn set-input! [buffer value]
  (let [bytes (.getBytes value "UTF-8")]
    (when (>= (alength bytes) capacity)
      (throw (ex-info "Input exceeds CUI capacity" {})))
    (dotimes [i (alength bytes)]
      (ffi/write buffer :uint8 (bit-and 255 (aget bytes i)) i))
    (ffi/write buffer :uint8 0 (alength bytes))))

(defn replace-input! [ui id buffer text caret-index]
  (set-input! buffer text)
  (restore-caret! ui id (alength (.getBytes (subs text 0 caret-index) "UTF-8"))))

(defn input-value [buffer]
  (ffi/ptr->string buffer))

(defn field! [ui id buffer prompt secret?]
  (let [events (field* ui id buffer capacity "" prompt "" "" (if secret? 106 104))]
    {:changed? (bit-test events 0) :submitted? (bit-test events 1)}))

(def ^:private field-layout
  (ffi/layout [:struct [[:unused :int] [:label :pointer] [:placeholder :pointer]
                        [:icon :pointer] [:hint :pointer] [:error :pointer]
                        [:password :bool] [:disabled :bool] [:clearable :bool]
                        [:autofocus :bool] [:read-only :bool] [:size :int]
                        [:width [:struct [[:kind :int] [:value :float]]]]
                        [:height :float] [:prefix :pointer] [:suffix :pointer] [:kind :int]]]))

(defn prompt-field! [ui id buffer placeholder secret?]
  (let [options (ffi/alloc (ffi/sizeof field-layout))
        placeholder (ffi/string->ptr placeholder)]
    (try
      (ffi/write-field options field-layout :placeholder placeholder)
      (ffi/write-field options field-layout :autofocus true)
      (ffi/write-field options field-layout :password secret?)
      (ffi/write-field options field-layout :kind 3)
      (font-role! ui 0 3)
      (let [events (bare-field* ui (bit-xor (make-id ui id) 0xF1E1D)
                                buffer capacity options false 1 ffi/null)]
        {:changed? (bit-test events 0) :submitted? (bit-test events 1)})
      (finally (ffi/free placeholder) (ffi/free options)))))

(defn editor-field!
  "A borderless multi-line field that fills its parent. Ctrl+Enter submits."
  [ui id buffer placeholder]
  (let [options (ffi/alloc (ffi/sizeof field-layout))
        placeholder (ffi/string->ptr placeholder)]
    (try
      (memset options 0 (ffi/sizeof field-layout))
      (ffi/write-field options field-layout :placeholder placeholder)
      (ffi/write-field options field-layout :autofocus true)
      (ffi/write-field options field-layout :height -1.0)
      (let [events (bare-field* ui (make-id ui id) buffer capacity options true 1 ffi/null)]
        {:changed? (bit-test events 0) :submitted? (bit-test events 1)})
      (finally (ffi/free placeholder) (ffi/free options)))))

(defn input-buffer
  "An empty field buffer owned by the caller; release it with `ffi/free`."
  []
  (doto (ffi/alloc capacity) (set-input! "")))

(def ^:private keys-mask {:up 1 :down 2 :enter 4 :tab 8 :escape 16 :backspace 32})

(defn take-keys! [ui requested]
  (let [taken (take-keys* ui (reduce + (map keys-mask requested)))]
    (into #{} (filter #(pos? (bit-and taken (keys-mask %)))) requested)))

(defn window!
  "Open a CUI window on the calling thread and release all native resources before returning."
  [{:keys [width height title redraw app-id prepared?]} render!]
  (let [buffer (ffi/alloc capacity)]
    (try
      (set-input! buffer "")
      (let [failure (volatile! nil)
            callback (ffi/foreign-callable
                      (fn [ui _]
                        (try (render! ui buffer)
                             (catch Throwable error
                               (vreset! failure error)
                               (quit!))))
                      [:pointer :pointer] :void :collect-safe)
            title (ffi/string->ptr (or title "Cloncher"))
            app-id (ffi/string->ptr (or app-id "cloncher"))
            waker (create-waker)
            waker-lock (Object.)
            attached? (volatile! true)]
        (try
          (when (ffi/null? waker)
            (throw (ex-info "CUI could not allocate a frame signal" {})))
          (when redraw
            (reset! redraw #(locking waker-lock
                              (when @attached? (wake-window! waker)))))
          (when prepared?
            (try (use-prepared-launcher*) (catch Exception _ nil)))
          (let [status (run-app title app-id ffi/null width height callback ffi/null waker)]
            (when-let [error @failure] (throw error))
            (when-not (zero? status)
              (throw (ex-info "CUI could not open the launcher window" {:status status}))))
          (finally
            (locking waker-lock
              (vreset! attached? false)
              (when redraw (reset! redraw nil))
              (when-not (ffi/null? waker) (free-waker waker)))
            (ffi/free-callable callback)
            (ffi/free app-id)
            (ffi/free title))))
      (finally
        (clear-input! buffer)
        (ffi/free buffer)))))
