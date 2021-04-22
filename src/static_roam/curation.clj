(ns static-roam.curation
    (:require [static-roam.config :as config]
              [static-roam.utils :as utils]
              [static-roam.database :as database]
              [static-roam.core :as core]
              [clojure.set :as set]
              [org.parkerici.multitool.core :as u]
              ))

;;; Curation functions. For use via REPL; not wired into main program.

(defn bm
  []
  @core/last-bm)

;;; Finds where the same ref appears >1 time in a block, usually the result
;;; of overagressive Roam auto-linking.
#_
(def weirdos
  (filter #(and (:content %)
                (not (= (count (database/content-refs (:content %)))
                        (count (database/content-refs-0 (:content %))))))
          (vals (bm))))

;;; Trying to find substitutions within urls; happens quite frequently
(def bunkos
  (filter #(and (:content %)
                (re-find #"[\.]\[\[" (:content %)))
          (vals (bm))))


;;; This Roam syntax (block embeds?) not supported, and I think it's functionallyt the same as the plain block include?

;;; {{[[embed]]: ((rRHEjBfYg))}}

;;; Unmatched refs, should be empty except for certain parse issues
(defn wankos
  []
  (set/difference
   (reduce clojure.set/union (map :refs (vals (bm))))
   (set (keys (bm)))))

(defn wankos2
  []
  (let [included (u/clean-map (comp not :include?) (bm))]
    (clojure.set/difference
     (reduce clojure.set/union (map :refs (vals included)))
     (set (keys included)))))

; Ah that's actually useful, shows dead links in output, due to #Private pages I assume.

#{"meditation" "Stances, a Catalog" "Buddhism" "William Blake" "Infinite Jest" "YMCYL Kindle Notes"
  "Free Play"}


;;; Find bad links

(defn check-link
  [url]
  (clj-http.client/head url
                        {:cookie-policy :standard
                         :trace-redirects true
                         :redirect-strategy :graceful}))

(defn check-links
  [bm]
  (let [bads (atom nil)]
    (doseq [l (database/all-external-links bm)]
      (future-call
       #(try
          (prn l)
          (check-link l)
          (catch Throwable e (swap! bads conj [l e])))))
    bads))

;;; OK...next step is to generate archive.org links where possible, andd substitute them...how to do that? Write json for import? Can that do modifications? Need the Roam API...


(defn prettify
  [bads]
  (for [b bads]
    [(first b) (:status (ex-data (second b))) (:reason-phrase (ex-data (second b)))]))
