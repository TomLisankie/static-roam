(ns static-roam.parser
  (:require [instaparse.core :as insta :refer [defparser]]
            [clojure.string :as str-utils]
            [clojure.data.json :as json]
            [static-roam.config :as config]
            [static-roam.utils :as utils]
            [clojure.java.io :as io]))

;; Modified from Athens: https://github.com/athensresearch/athens/blob/master/src/cljc/athens/parser.cljc

(declare block-parser)

;; Instaparse docs: https://github.com/Engelberg/instaparse#readme

(defn- combine-adjacent-strings
  "In a sequence of strings mixed with other values, returns the same sequence with adjacent strings concatenated.
   (If the sequence contains only strings, use clojure.string/join instead.)"
  [coll]
  (mapcat (fn [subseq]
            (if (string? (first subseq))
              (list (apply str subseq))
              subseq))
          (partition-by string? coll)))

(defn- transform-to-ast
  "Transforms the Instaparse output tree to an abstract syntax tree for SR markup."
  [tree]
  (insta/transform
    {:block                  (fn [& raw-contents]
                                ;; use combine-adjacent-strings to collapse individual characters from any-char into one string
                               (into [:block] (combine-adjacent-strings raw-contents)))
     :url-link               (fn [text-contents url]
                               (into [:url-link {:url url}] text-contents))
     :url-link-text-contents (fn [& raw-contents]
                               (combine-adjacent-strings raw-contents))
     :url-link-url-parts     (fn [& chars]
                               (clojure.string/join chars))
     :text                   (fn [s] s)
     }
    tree))

(def parser-file (io/resource "parser.ebnf"))

(defparser block-parser
  (slurp parser-file))

(defn parse-to-ast
  "Converts a string of block syntax to an abstract syntax tree for SR markup."
  [block-content]
  {:pre [(string? block-content)]}
  (transform-to-ast (block-parser block-content)))

(declare block-content->hiccup)         ;allow recursion on this

;;; TODO "alias" seems like a misnomer, these are external links.
(defn- format-alias
  [alias-content]
  (let [alias-text (utils/remove-n-surrounding-delimiters 1 (re-find #"\[.+?\]" alias-content))
        alias-dest (utils/remove-n-surrounding-delimiters 1 (re-find #"\(.+?\)" alias-content))
        alias-link (if (or (= \( (first alias-dest)) (= \[ (first alias-dest)))
                     (utils/page-title->html-file-title alias-dest :case-sensitive)
                     alias-dest)]
    [:a {:href alias-link} (block-content->hiccup alias-text {})])) 

(defn- format-image
  [image-ref-content]
  (let [alt-text (utils/remove-n-surrounding-delimiters 1 (re-find #"\[.*?\]" image-ref-content))
        image-source (utils/remove-n-surrounding-delimiters 1 (re-find #"\(.*?\)" image-ref-content))]
    [:img {:src image-source :alt alt-text :style "max-width: 90%"}]))

;;; TODO this really belongs in html gen, not in parsing, yes?
(defn get-youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [string]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube-nocookie.com/embed/"
                      (if (re-find #"\[\[youtube\]\]" string)
                        (cond
                          (re-find #"youtube\.com" string) (subs string 47 (- (count string) 2))
                          (re-find #"youtu\.be" string) (subs string 32 (- (count string) 2))
                          :else "NO VALID ID FOUND")
                        (cond
                          (re-find #"youtube\.com" string) (subs string 43 (- (count string) 2))
                          (re-find #"youtu\.be" string) (subs string 28 (- (count string) 2))
                          :else "NO VALID ID FOUND")))
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
            :allowfullscreen ""}])

(defn- make-link-from-url
  [string]
  [:a {:href string} string])

;;; Tried to build this into instaparse parser, but couldn't make it take precedence over :bare-url
(defn- twitter-url?
  [url]
  (re-matches #"https:\/\/twitter.com\/\S*" url))

(defn- embed-twitter
  [url]
  (try
    (let [oembed (json/read-str (slurp (str "https://publish.twitter.com/oembed?url=" url)) :key-fn keyword)]
      (:html oembed))
    (catch Throwable e
        (make-link-from-url url))))

(defn- make-content-from-url
  [url]
  (if (twitter-url? url)                ;TODO if there are more
    (embed-twitter url)
    (make-link-from-url url)))

(defn page-link [ele-content]
  [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
   (block-content->hiccup               ;ensure formatting in links works
    (utils/remove-double-delimiters ele-content) {})])


(defn unspan
  "Remove :span elts that are basically no-ops. Would be cleaner to not generate"
  [hiccup]
  (if (and (vector? hiccup)
           (= :span (first hiccup))
           (= 2 (count hiccup)))
    (second hiccup)
    hiccup))

(defn generate-hiccup
  [block block-map]
  (if (:content block)
    (let [basic (block-content->hiccup (:content block) block-map)]
      (if (> (:heading block) 0)
        [(keyword (str "h" (:heading block))) basic]
        basic))
    (do
      (prn "Missing content: " (:id block))
      nil)))

(defn ele->hiccup ;; TODO: have code to change behavior if page/block is not included
  [ast-ele block-map]
  (let [recurse (fn [s]                 ;TODO probably needs a few more uses
                  (ele->hiccup (parse-to-ast s) block-map))]
    (if (string? ast-ele)
      ast-ele
      (let [ele-content (second ast-ele)]
        (unspan
         (case (first ast-ele)
           :metadata-tag [:b [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                              (subs ele-content 0 (dec (count ele-content)))]]
           :page-link (page-link ele-content)
           ;; NOTE this is the only thing that needs the block-map passed in
           :block-ref (let [ref-block (get block-map (utils/remove-double-delimiters ele-content))
                            ref-page nil] ;TODO database/block-page but namespace problem
                        [:div.block-ref #_ {:onclick (format "location.href='%s';" (page-url ref-page))}
                         (generate-hiccup ref-block block-map)])
           :hashtag [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                     (utils/format-hashtag ele-content)]
           :strikethrough [:s (recurse (utils/remove-double-delimiters ele-content))]
           :highlight [:mark (recurse (utils/remove-double-delimiters ele-content))]
           :italic [:i (recurse (utils/remove-double-delimiters ele-content))]
           :bold [:b (recurse (utils/remove-double-delimiters ele-content))]
           :alias (format-alias ele-content)
           :image (format-image ele-content)
           :todo [:input {:type "checkbox" :disabled "disabled"}]
           :done [:input {:type "checkbox" :disabled "disabled" :checked "checked"}]
           :code-line [:code (utils/remove-n-surrounding-delimiters 1 ele-content)]
           :code-block [:code.codeblock (utils/remove-n-surrounding-delimiters 3 ele-content)] ;TODO parse out language indicator, or better yet use it
           :youtube (get-youtube-vid-embed ele-content)
           :bare-url (make-content-from-url ele-content)
           :blockquote [:blockquote (ele->hiccup ele-content block-map)]
                                        ;ast-ele
           :block `[:span ~@(map #(ele->hiccup % block-map) (rest ast-ele))]
           :block-embed `[:pre "Unsupported: " (str ast-ele)] ;TODO temp duh
           ))))))

      ;#Private

(defn block-content->hiccup
  "Convert Roam markup to Hiccup"
  [content block-map]
  (ele->hiccup (parse-to-ast content) block-map))
