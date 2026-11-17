(ns cloncher.view
  "Shared compact launcher panel for production and the dev host."
  (:require [clojure.string :as str]
            [cloncher.cui :as cui]
            [cloncher.panel :as panel]
            [cloncher.search :as search]))

(def preview-height 520)
(def preview-width 640)
(def secret-height 62)

(def ^:private prompt-height 40)
(def ^:private status-height 21)
(def ^:private separator-height 1)
(def ^:private chrome-height (+ prompt-height status-height separator-height))
(def ^:private file-preview-minimum 300)

(defn begin! [ui preview? fit?]
  (panel/begin! ui "launcher" {:width (if preview? (min preview-width (max 1 (- (cui/width ui) 64))) :fill)
                               :height (if fit? :fill :fit)
                               :background :background :border 1 :radius 6 :clip? true}))

(defn- single-line [text]
  (str/replace (or text "") #"[\r\n\t\u0000]" " "))

(defn prompt! [ui id buffer prompt secret?]
  (when secret?
    (panel/begin! ui "secret-label" {:height 22 :pad-x 14 :pad-y 3})
    (panel/text! ui prompt {:color :secondary})
    (cui/end! ui))
  (panel/begin! ui "promptline" {:row? true :height prompt-height :pad-x 13 :gap 5})
  (try
    (panel/text! ui "❯" {:color :accent :size 15 :width 11})
    (cui/prompt-field! ui id buffer (if secret? "" (str/replace (str prompt "…") #"…+$" "…")) secret?)
    (finally (cui/end! ui))))

(defn row-height [{:keys [app? icon]}]
  (cond
    app? 30
    (search/emoji? icon) 41
    :else 24))

(defn section-start? [previous row]
  (and previous (or (and (:live previous) (not (:live row)))
                    (> (or (:section row) 0) (or (:section previous) 0)))))

(defn result-layout
  "Lay out results against a height budget independent of the current window size."
  [height rows]
  (let [row-heights (mapv row-height rows)
        plain-height (+ (reduce + row-heights)
                        (* separator-height (count (filter identity (map section-start? rows (next rows))))))
        available (max 0 (- height chrome-height))]
    (if (<= plain-height available)
      {:page-size (max 1 (count rows))
       :height (+ chrome-height plain-height)}
      (let [tallest (reduce max 24 row-heights)
            page-size (max 1 (quot available tallest))]
        {:page-size page-size
         :height (+ chrome-height (* page-size tallest))
         :virtual-row-height tallest}))))

(defn window-height [layout file-preview?]
  (if file-preview?
    (max (+ prompt-height file-preview-minimum) (:height layout))
    (:height layout)))

(defn preview-body-height [layout]
  (- (window-height layout true) chrome-height))

(defn- mode-label [{:keys [kind key prompt query]}]
  (case kind
    :root (cond (str/starts-with? query ">>>") "HISTORY"
                (str/starts-with? query ">>") "KITTY HISTORY"
                (str/starts-with? query ">") "COMMAND"
                (str/starts-with? query "(") "LISP"
                (str/starts-with? query "=") "CALC"
                :else nil)
    :apps "APPS"
    :files "FILES"
    :projects "PROJECTS"
    :project-add "ADD PROJECT"
    :project "PROJECT"
    :file "FILE"
    :menu (when (keyword? key) (str/upper-case (name key)))
    :dmenu (or (some-> prompt (str/replace #"…+$" "") str/trim not-empty str/upper-case) "PICK")
    nil))

(defn status! [ui {:keys [kind error pending build jump rows] :as view} row count nested?]
  (let [mode (mode-label view)
        hint (cond
               error error
               jump (str "Select label " (:prefix jump) " · Esc cancel")
               (and (zero? count) (or pending build)) "Searching…"
               (and (= :dmenu kind) (zero? count)) "↵ use typed text"
               (zero? count) "No matches"
               :else (str (when-let [action (:hint row)] (str "↵ " action "  "))
                          (when-let [action (:alt-hint row)] (str "^↵ " action "  "))
                          (when nested? "⌫ back  ")
                          ";label select"))]
    (cui/separator! ui)
    (panel/begin! ui "status" {:row? true :height status-height :background :surface})
    (try
      (when (seq mode)
        (panel/begin! ui "status-mode" {:height status-height :width :fit :row? true
                                         :background :accent :pad-x 10})
        (panel/text! ui mode {:color :accent-text})
        (cui/end! ui))
      (panel/begin! ui "status-hint" {:row? true :height status-height :width :fill :pad-x 10})
      (panel/text! ui hint {:width :fill :color (if error :danger :muted)})
      (when (and (pos? count) (= :apps kind))
        (panel/text! ui (str count "/" (clojure.core/count rows)) {:right? true}))
      (cui/end! ui)
      (finally (cui/end! ui)))))

(defn row! [ui id {:keys [icon label display-shortcut warn state subtitle path texture running-progress] :as row}
            selected? actionable? width]
  (let [height (or (:row-height row) (row-height row))
        detail (single-line (or (not-empty subtitle) path))
        detail-width (min (max 0 (quot width 2)) (* 8 (count detail)))
        native-id (panel/begin! ui id {:row? true :height height
                                      :background (cond selected? :active state :surface :else :background)})]
    (try
      (panel/begin! ui "indicator" {:width 2 :height height
                                     :background (if selected? :accent :background)})
      (cui/end! ui)
      (panel/begin! ui "content" {:row? true :height height :pad-x 12 :gap 9})
      (try
        (panel/begin! ui "glyph" {:row? true :width 30 :height height :pad-x 2})
        (cond
          running-progress (do (cui/spacer! ui) (cui/spinner! ui 12.5) (cui/spacer! ui))
          texture (panel/image! ui texture 23)
          :else (panel/text! ui (or icon "") {:width :fill :center? true :color (if state :secondary :accent)
                                              :size (if (= 41 height) 26 13)}))
        (cui/end! ui)
        (panel/text! ui (single-line (or (:label running-progress) label))
                     {:width :fill :color (cond state :secondary selected? :selected-text :else :foreground)})
        (when (pos? detail-width)
          (if state
            (do (panel/begin! ui "badge" {:row? true :height 18 :width :fit :pad-x 7
                                           :background :badge :radius 3})
                (panel/text! ui detail {:color (if warn :danger :badge-text)
                                         :right? true :width detail-width})
                (cui/end! ui))
            (panel/text! ui detail {:width detail-width :right? true :color (if warn :danger :muted)})))
        (panel/text! ui (or display-shortcut "") {:width 28 :right? true :size 10 :color :muted})
        (finally (cui/end! ui)))
      (and actionable? (panel/clicked? ui native-id))
      (finally (cui/end! ui)))))

(defn file-preview! [ui width path {:keys [notice detail image lines working?]}]
  (cui/column! ui "file-preview" {:width width :pad 12 :gap 4 :surface? true})
  (try
    (cui/caption! ui (or detail "Preview"))
    (cui/scroll! ui (str "preview-" path))
    (try
      (cond
        working? (cui/caption! ui "Loading preview…")
        notice (cui/caption! ui notice)
        image (cui/thumbnail! ui "sample" image (- width 34))
        (seq lines) (let [text (str/join "\n" (map-indexed #(format "%2d  %s" (inc %1) %2) lines))]
                      (cui/markdown! ui "contents"
                                     (str "````````text\n"
                                          (str/replace text #"`{7,}" "``````")
                                          "\n````````")))
        :else (cui/caption! ui "No preview available"))
      (finally (cui/end-scroll! ui)))
    (finally (cui/end! ui))))
