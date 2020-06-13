(ns static-roam.parser
  (:require [clojure.string :as str-utils]
            [static-roam.core :as core]))

(defn- string-replace
  [data f]
  (if (string? data)
    (f data)
    data))

(defn- replace-ele-in-string
  [string ele-regex replacements]
  (let [string-vec (str-utils/split string ele-regex)]
    (loop [index 0
           string-vec string-vec
           final-vec []
           replacements replacements]
      (if (= (count string-vec) 0)
        (if (empty? replacements)
          final-vec
          (conj final-vec (first replacements)))
        (recur (inc index)
               (if (odd? index)
                 string-vec
                 (rest string-vec))
               (if (odd? index)
                 (conj final-vec (first replacements))
                 (conj final-vec (first string-vec)))
               (if (odd? index)
                 (rest replacements)
                 replacements))))))

(defn- todo-replace
  [string]
  ;; needs to replace the {{[[TODO]]}} in a string
  ;; will need to split the string if it finds it and replace with a vec I guess?
  (replace-ele-in-string string #"\{\{\[\[TODO\]\]\}\}" (repeat (count (re-seq #"\{\{\[\[TODO\]\]\}\}" string)) [:todo])))

(repeat (count (re-seq #"\{\{\[\[TODO\]\]\}\}" "Here's a block with a sample {{[[TODO]]}} in its contents. And here are two more of them. {{[[TODO]]}} {{[[TODO]]}}. That's it")) [:todo])

(defn- page-link-replace
  [string]
  (replace-ele-in-string string #"\[\[.*?\]\]" (map (fn [regex-match] [:page-link (core/remove-double-delimiters regex-match)]) (re-seq #"\[\[.*?\]\]" string))))

(map (fn [regex-match] [:page-link (core/remove-double-delimiters regex-match)]) (re-seq #"\[\[.*?\]\]" "What is a [[Parser]] for the context of [[Static-Roam]]? [[hello]]"))

(concat [:block] (todo-replace "Here's a block with a sample {{[[TODO]]}} in its contents. And here are two more of them. {{[[TODO]]}} {{[[TODO]]}}. That's it"))
(concat [:block] (todo-replace "{{[[TODO]]}} Be able to specify which page is SR metadata so that you can have multiple [[Digital Garden]]s come from the same Roam graph"))

(page-link-replace "What is a [[Parser]] for the context of [[Static-Roam]]? [[hello]][[yes]]")

(defn- content->ast
  [content]
  ;; This is analogous to the role of `roam-web-elements` in `core.clj`
  ;; At each step it maps on all of the strings it can find in the sequence and transforms them according to the rule at that step
  (let [og-seq [:block]
        todos (concat og-seq (string-replace content todo-replace))
        page-links ()]
    page-links))

(content->ast "{{[[TODO]]}} Be able to specify which page is SR metadata so that you can have multiple [[Digital Garden]]s come from the same Roam graph")

(defn- get-youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [string]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube-nocookie.com/embed/"
                      (cond
                        (re-find #"youtube\.com" string) (subs string 32)
                        (re-find #"youtu\.be" string) (subs string 17)
                        :else "NO VALID ID FOUND"))
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"}])

(defn- query
  [query-string]
  ;; TODO write this function
  query-string)

(defn- word-count
  [block-ds-id conn]
  ;; TODO Implement this function
  "0")

(defn- hiccup-of-ele
  [block-ds-id ast-ele conn]
  (let [ele-content (second ast-ele)]
    (case (first ast-ele)
      :page-link (if (included? (remove-double-delimiters ele-content) conn)
                   [:a {:href (page-title->html-file-title (remove-double-delimiters ele-content) :case-sensitive)}
                    (remove-double-delimiters ele-content)]
                   (remove-double-delimiters ele-content))
      :block-ref (if (included? ele-content conn)
                   [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                    (content-find ele-content conn)]
                   "REDACTED")
      :metadata-tag (if (included? ele-content conn)
                      [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                       (str ele-content ":")]
                      (str ele-content ":"))
      :code-line [:code ele-content]
      :query (query ele-content)
      :youtube-embed (get-youtube-vid-embed ele-content)
      :word-count [:p (word-count block-ds-id conn)]
      :hashtag []
      :url-link []
      :bold [:b ele-content]
      :italic [:i ele-content]
      :highlight [:mark ele-content]
      :strikethrough [:s ele-content])))

(defn- ast-ele->hiccup
  [block-ds-id ast-ele conn]
  (cond
    (string? ast-ele) ast-ele
    (= ast-ele :block) :div
    (vector? ast-ele) (hiccup-of-ele block-ds-id ast-ele conn)
    :else ast-ele))

(defn- ast->hiccup
  [block-ds-id content conn]
  (map #(ast-ele->hiccup block-ds-id % conn) content))

(defn block-content->hiccup
  [block-ds-id content conn]
  (->> content
       content->ast
       (#(ast->hiccup block-ds-id % conn))
       vec))

(content->ast "Here's a block with a sample {{[[TODO]]}} in its contents.") ;; -> [:block "Here's a block with a sample " [:todo] " in its contents."]
;; splits at recognized element
;; so I'm going to split
;; then run a function where if the current index is even it appends next element. If odd, append [:todo]

