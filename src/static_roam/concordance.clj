(ns static-roam.concordance
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [static-roam.database :as db]
            [org.parkerici.multitool.nlp :as nlp]
            [org.parkerici.multitool.core :as u])
  )

;;; Find overexpressed words in a graph; use to generate something like https://omniorthogonal.blogspot.com/2015/01/whats-on-my-mind.html
;;; Status: not wired into main program; not clear this is a good idea in the static-roam context

(defn capitalized?
  [s]
  (java.lang.Character/isUpperCase (first s)))

(defn tokens
  [s]
  (->> s
       (re-seq #"[\p{L}\d]+")
       (remove capitalized?)
       (map s/lower-case)))

;;; TODO filter out blockquotes maybe? 
(defn all-tokens
  [bm]
  (->> bm
       (db/displayed-pages)
       (map (partial render/block-full-text bm))
       (mapcat (u/safely tokens))
       nlp/remove-ruthlessly))

(defn overexpressed
  [bm]
  (nlp/overexpressed (frequencies (all-tokens bm))
                     (freq-table  "count_1w.txt")))
  

(defn spell-correct
  [bm]
  (set/difference (set (all-tokens bm))
                  (set (keys (freq-table  "count_1w.txt")))))
