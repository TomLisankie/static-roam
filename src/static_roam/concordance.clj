(ns static-roam.concordance
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [static-roam.database :as db]
            [org.parkerici.multitool.nlp :as nlp]
            [org.parkerici.multitool.core :as u])
  )

;;; Find overexpressed words in a graph; use to generate something like https://omniorthogonal.blogspot.com/2015/01/whats-on-my-mind.html
;;; Status: not wired into main program; not clear this is a good idea in the static-roam context

;;; TODO filter out blockquotes maybe? And urls and such.
(defn all-tokens
  [bm]
  (->> bm
       (db/displayed-pages)
       (map (partial db/block-text bm))
       (mapcat (u/safely nlp/tokens))
       nlp/remove-ruthlessly))

;;; → multitool/nlp
(defn norvig-file
  [file]
  (str "http://norvig.com/ngrams/" file))

(defn url-lines
  [url]
  (-> url
      io/reader
      line-seq))

(defn read-norvig-freqs [file]
  (reduce (fn [map line]
            (let [[word count] (s/split line #"\t")
                 count (Long. count)]
              (assoc map word count)))
          {} (url-lines (norvig-file file))))

(u/defn-memoized freq-table [name]
  (read-norvig-freqs name))

(defn overexpressed
  [bm]
  (nlp/overexpressed (frequencies (all-tokens bm))
                     (freq-table  "count_1w.txt")))
  

