(ns static-roam.parser
  (:require [instaparse.core :as insta :refer [defparser]]
            [clojure.string :as str-utils]
            [static-roam.utils :as utils]))

;; Modified from Athens: https://github.com/athensresearch/athens/blob/master/src/cljc/athens/parser.cljc

(declare block-parser)

;; Instaparse docs: https://github.com/Engelberg/instaparse#readme

(defn- combine-adjacent-strings
  "In a sequence of strings mixed with other values, returns the same sequence with adjacent strings concatenated.
   (If the sequence contains only strings, use clojure.string/join instead.)"
  [coll]
  (reduce
    (fn [elements-so-far elmt]
      (if (and (string? elmt) (string? (peek elements-so-far)))
        (let [previous-elements (pop elements-so-far)
              combined-last-string (str (peek elements-so-far) elmt)]
          (conj previous-elements combined-last-string))
        (conj elements-so-far elmt)))
    []
    coll))


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
     :any-chars              (fn [& chars]
                               (clojure.string/join chars))}
    tree))

(defparser block-parser
  (slurp "src/static_roam/parser.ebnf"))

(defn parse-to-ast
  "Converts a string of block syntax to an abstract syntax tree for SR markup."
  [block-content]
  (transform-to-ast (try
                      (block-parser block-content)
                      (catch Exception e (str "Exception when parsing content: " block-content)))))

(def parsed (parse-to-ast "Metadata is here:: According to [[BJ Fogg]], we have [[motivation waves]].  Tie that in with the [[Fogg Behavior Model]] and you find that when people have high motivation, you should ask them to do something big and impactful, because if people are motivated to do more than the task that we ask them to do, it would be a waste for us not to prompt them to do so.  On the flip side, if people aren't particularly motivated, we shouldn't ask them to do something hard. ((j598fj6)) This is similar to the premise of #[[difficulty matching]] #yessereebob `this is a line of code` {{query: {and: [[note]] [[January]] }}} {{youtube: https://youtu.be/5iI_0wnwIpU}} [fasfa]([[Hello page]]) **IMPORTANT** __emphasis__ ^^pop out^^ ~~old news~~"))

(parse-to-ast "(((hello)))")

(parse-to-ast "[hello]([[Hello]])")

(parse-to-ast "I hope to combine my expertise in behavioral science and gamification to help users improve their lives through products. Your users hire you to help them achieve some goal, but loving your product is a result of your teamwork with the user.[++]([[The role of the user and the role of the app]]) Both players need to play.")

(defn remove-n-surrounding-delimiters
  "Removes n surrounding characters from both the beginning and end of a string"
  [n string]
  (if (<= (count string) (* 2 n))
    string
    (subs string n (- (count string) n))))

(defn remove-double-delimiters
  "Removes 2 surrounding characters from both the beginning and end of a string"
  [string]
  (remove-n-surrounding-delimiters 2 string))

(defn- strip-chars
  "Removes every character of a given set from a string"
  [chars collection]
  (reduce str (remove #((set chars) %) collection)))

(defn- format-hashtag
  [hashtag]
  (if (= \[ (second hashtag))
    (remove-double-delimiters (subs hashtag 1))
    (subs hashtag 1)))

(defn- format-alias
  [alias-content]
  (let [alias-text (remove-n-surrounding-delimiters 1 (re-find #"\[.+?\]" alias-content))
        alias-dest (remove-n-surrounding-delimiters 1 (re-find #"\(.+?\)" alias-content))
        alias-link (if (or (= \( (first alias-dest)) (= \[ (first alias-dest)))
                     (utils/page-title->html-file-title alias-dest :case-sensitive)
                     alias-dest)]
    [:a {:href alias-link} alias-text]))

(defn- format-image
  [image-ref-content]
  (let [alt-text (remove-n-surrounding-delimiters 1 (re-find #"\[.+?\]" image-ref-content))
        image-source (remove-n-surrounding-delimiters 1 (re-find #"\(.+?\)" image-ref-content))]
    [:img {:src image-source :alt alt-text}]))

(defn get-youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [string]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube-nocookie.com/embed/"
                      (cond
                        (re-find #"youtube\.com" string) (subs string 43 (- (count string) 2))
                        (re-find #"youtu\.be" string) (subs string 28 (- (count string) 2))
                        :else "NO VALID ID FOUND"))
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
            :allowfullscreen ""}])

(defn element-vec->hiccup ;; TODO: have code to change behavior if page/block is not included
  [ast-ele]
  (let [ele-content (second ast-ele)]
    (case (first ast-ele)
      :metadata-tag [:b [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                         (subs ele-content 0 (dec (count ele-content)))]]
      :page-link [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                  (remove-double-delimiters ele-content)]
      :block-ref [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                  (remove-double-delimiters ele-content)]
      :hashtag [:a {:href (utils/page-title->html-file-title ele-content :case-sensitive)}
                (format-hashtag ele-content)]
      :strikethrough [:s (remove-double-delimiters ele-content)]
      :highlight [:mark (remove-double-delimiters ele-content)]
      :italic [:i (remove-double-delimiters ele-content)]
      :bold [:b (remove-double-delimiters ele-content)]
      :alias (format-alias ele-content)
      :image (format-image ele-content)
      :todo [:input {:type "checkbox" :disabled "disabled"}]
      :done [:input {:type "checkbox" :disabled "disabled" :checked "checked"}]
      :code-line [:code (remove-n-surrounding-delimiters 1 ele-content)]
      :youtube (get-youtube-vid-embed ele-content)
      ast-ele)))

(defn ele->hiccup
  [ele]
  (cond
    (string? ele) ele
    (= ele :block) :div
    (vector? ele) (element-vec->hiccup ele)))

(vec (map ele->hiccup parsed))

(defn entry-point?
  "Determines whether or not a given page is tagged with #EntryPoint in its first child block"
  [page]
  (if (= (count (:children page)) 0)
    false
    (if (and (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children page))))
             (str-utils/includes?
              (:string (first (:children page))) "#EntryPoint"))
      true
      false)))

(defn block-content->hiccup
  "Convert Roam markup to Hiccup"
  [block-ds-id content conn]
  (vec (map ele->hiccup (parse-to-ast content))))

parsed
