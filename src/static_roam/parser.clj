(ns static-roam.parser
  (:require [instaparse.core :as insta :refer [defparser]]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.string :as str]
            [static-roam.config :as config]
            [static-roam.utils :as utils]
            [org.parkerici.multitool.core :as u]
            [clojure.java.io :as io]))

;;; Turns Roam block content into an intermediate form (AST)

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

;;; Performance hack, 115 → 84 sec
;;; Should turn off in repl → TODO idea for multitool
(u/defn-memoized parse-to-ast
  "Converts a string of block syntax to an abstract syntax tree for SR markup."
  [block-content]
  {:pre [(have? string? block-content)]}
  (transform-to-ast (block-parser block-content)))














