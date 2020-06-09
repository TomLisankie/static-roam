(ns static-roam.parser
  (:require [instaparse.core :as insta :refer [defparser]]))

;;Modified from Athens: https://github.com/athensresearch/athens/blob/master/src/cljc/athens/parser.cljc

(declare block-parser)


;; Instaparse docs: https://github.com/Engelberg/instaparse#readme

(defparser block-parser
  (slurp "src/static_roam/parser.ebnf"))


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


(defn parse-to-ast
  "Converts a string of block syntax to an abstract syntax tree for Athens markup."
  [block-content]
  (transform-to-ast (block-parser block-content)))

(parse-to-ast "[[asdfasdf]] [[assd]]")
