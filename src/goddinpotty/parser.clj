(ns goddinpotty.parser
  (:require [instaparse.core :as insta :refer [defparser]]
            [clojure.string :as str]
            [goddinpotty.config :as config]
            [clojure.java.io :as io]))

;;; Turns Roam block content into an intermediate form (AST)

;;; TODO Maybe walk this over parser output
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
    ;; TODO this shit is ugly but I couldn't figure out better way
    :text                   (fn [s] s)
    :textier                (fn [s] s)
    ;; This makes up for the fact that there are now two alternative italic syntaxes
    :italic                 (fn [s] `[:italic ~@(rest s)]) 
    }
   tree))

(def parser-file (io/resource "parser.ebnf"))

(defparser block-parser
  (slurp parser-file))

;;; Turned off memoization, shouldn't need it 
(defn parse-to-ast
  "Converts a string of block syntax to an abstract syntax tree for SR markup."
  [block-content]
  (when block-content 
    (transform-to-ast (block-parser block-content))))

