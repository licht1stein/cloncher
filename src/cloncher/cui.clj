(ns cloncher.cui
  "CUI host ABI and native ownership."
  (:require [jolt.ffi :as ffi]))

(ffi/defcfn ^:private run-app "cui_host_run_app_wake"
  [:pointer :pointer :pointer :int :int :pointer :pointer :pointer] :int :blocking)
(ffi/defcfn ^:private create-waker "cui_host_frame_waker_new" [] :pointer)
(ffi/defcfn ^:private free-waker "cui_host_frame_waker_free" [:pointer] :void)
(ffi/defcfn ^:private wake-window! "cui_host_request_frame" [:pointer] :bool :blocking)
(ffi/defcfn quit! "cui_host_quit" [] :void)
(ffi/defcfn focused? "cui_host_window_focused" [:pointer] :bool)
(ffi/defcfn wake-in! "cui_request_wakeup" [:pointer :float] :void)
(ffi/defcfn redraw! "cui_request_redraw" [:pointer] :void)
(ffi/defcfn ^:private field* "cui_host_field"
  [:pointer :string :pointer :int :string :string :string :string :int] :int)
(ffi/defcfn ^:private container* "cui_host_container_begin"
  [:pointer :string :int :int :int :int :int :int] :void)
(ffi/defcfn end! "cui_host_box_end" [:pointer] :void)
(ffi/defcfn caption! "cui_host_caption" [:pointer :string] :void)
(ffi/defcfn heading! "cui_host_heading" [:pointer :string :int] :void)
(ffi/defcfn separator! "cui_host_separator" [:pointer] :void)
(ffi/defcfn ^:private button* "cui_host_button_ex" [:pointer :string :int :int :int] :bool)
(ffi/defcfn height "cui_host_viewport_height" [:pointer] :int)
(ffi/defcfn width "cui_host_viewport_width" [:pointer] :int)
(ffi/defcfn icon-action! "cui_host_icon_action" [:pointer :int :string :int] :bool)
(ffi/defcfn ^:private list-row* "cui_host_list_row"
  [:pointer :string :string :string :bool :int :string :string] :int)
(ffi/defcfn ^:private key-pressed? "cui_key_pressed" [:pointer :int] :bool)
(ffi/defcfn ^:private modifiers "cui_key_mods" [:pointer] :uint32)
(ffi/defcfn ^:private memset "memset" [:pointer :int :size_t] :pointer)
(ffi/defcfn ^:private setenv "setenv" [:string :string :int] :int)
(ffi/defcfn ^:private take-keys* "cui_host_take_keys" [:pointer :int] :int)
(ffi/defcfn capture! "cui_host_capture" [:string] :void)
(ffi/defcfn capture-result "cui_host_capture_result" [] :int)

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

(defn fallback-font! [path]
  (let [existing (System/getenv "TEMPLATE_TEXT_FONTS")]
    (setenv "TEMPLATE_TEXT_FONTS" (if (seq existing) (str path ":" existing) path) 1)))

(defn shortcuts [ui]
  (let [mods (modifiers ui)]
    {:control? (bit-test mods 1)
     :shift? (bit-test mods 0)
     :r? (key-pressed? ui 31)
     :page-up? (key-pressed? ui 13)
     :page-down? (key-pressed? ui 14)}))

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

(defn input-value [buffer]
  (ffi/ptr->string buffer))

(defn field! [ui id buffer prompt secret?]
  (let [events (field* ui id buffer capacity "" prompt "" "" (if secret? 106 104))]
    {:changed? (bit-test events 0) :submitted? (bit-test events 1)}))

(def ^:private keys-mask {:up 1 :down 2 :enter 4 :tab 8 :escape 16 :backspace 32})

(defn take-keys! [ui requested]
  (let [taken (take-keys* ui (reduce + (map keys-mask requested)))]
    (into #{} (filter #(pos? (bit-and taken (keys-mask %)))) requested)))

(defn window!
  "Open a CUI window on the calling thread and release all native resources before returning."
  [{:keys [width height title redraw]} render!]
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
            app-id (ffi/string->ptr "cloncher")
            waker (create-waker)
            waker-lock (Object.)
            attached? (volatile! true)]
        (try
          (when (ffi/null? waker)
            (throw (ex-info "CUI could not allocate a frame signal" {})))
          (when redraw
            (reset! redraw #(locking waker-lock
                              (when @attached? (wake-window! waker)))))
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
