(ns cloncher.view
  "Launcher styling and CUI presentation. Safe to reload while a window is open."
  (:require [clojure.string :as str]
            [cloncher.cui :as cui]))

(def preview-height 468)
(def preview-width 680)

(defn begin! [ui preview?]
  (cui/column! ui "launcher" (if preview?
                               {:height preview-height :surface? true}
                               {})))

(defn back! [ui title]
  (cui/bar! ui "navigation")
  (try
    (let [pressed? (cui/icon-action! ui 3 "Back" 0)]
      (cui/caption! ui title)
      pressed?)
    (finally (cui/end! ui))))

(defn- single-line [text]
  (str/replace (or text "") #"[\r\n\t\u0000]" " "))

(defn- detail [{:keys [subtitle path hint]}]
  (str/join "  ·  " (remove str/blank? [(or (not-empty subtitle) path) hint])))

(defn page-size [ui nested? rows preview?]
  (let [height (if preview? (min preview-height (- (cui/height ui) 95)) (cui/height ui))
        row-height (if (some #(seq (detail %)) rows) 58 44)]
    (max 1 (quot (- height (if nested? 146 104)) row-height))))

(defn status! [ui {:keys [kind error pending build]} index count nested?]
  (cui/caption! ui
    (cond
      error error
      (or pending build) "Searching…"
      (and (= :dmenu kind) (zero? count)) "Enter to use typed text · Esc to cancel"
      (zero? count) "No matches"
      :else (str (inc index) " / " count "     ↑↓ Navigate    ↵ Select    Esc "
                 (if nested? "Back" "Close")))))

(defn row! [ui id {:keys [icon label] :as row} selected? actionable?]
  (cui/result-row! ui id
                   (single-line (str (when (seq icon) (str icon "   ")) label))
                   (single-line (detail row))
                   selected? actionable?))
