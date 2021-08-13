(ns static-roam.curation
    (:require [static-roam.config :as config]
              [static-roam.utils :as utils]
              [static-roam.database :as database]
              [static-roam.batadase :as bd]
              [static-roam.core :as core]
              [clojure.set :as set]
              [org.parkerici.multitool.core :as u]
              [me.raynes.fs :as fs]
              [clojure.string :as str]
              [clj-http.client :as client]
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

(defn block-links
  [block]
  (u/walk-collect #(and (string? %)
                        (str/starts-with? % "http")
                        %)
                  (:parsed block)))

(defn all-links
  [block-map]
  (distinct (mapcat block-links (bd/displayed-blocks block-map))))

(defn check-link
  [url]
  (client/head url
               {:cookie-policy :standard
                :trace-redirects true
                :redirect-strategy :graceful}))

(defn check-links
  [bm]
  (let [bads (atom nil)]
    (doseq [l (all-links bm)]
      (future-call
       #(try
          (check-link l)
          (catch Throwable e (swap! bads conj [l e])))))
    bads))

;;; OK...next step is to generate archive.org links where possible, andd substitute them...how to do that? Write json for import? Can that do modifications? Need the Roam API...


(defn prettify
  [bads]
  (for [b bads]
    [(first b) (:status (ex-data (second b))) (:reason-phrase (ex-data (second b)))]))


;;; Check output and ensure all local html links are valid.
;;; This finds a lot of missing things due to Zoetero import, plus
; :missing "Topics.html" :in "play.html"
; :missing "Topics.html" :in "agency.html"
; :missing "Mastery-of-Non-Mastery.html" :in "mimesis.html"
(defn check-output-links
  []
  (doseq [f (fs/list-dir "output/pages")]
    (doseq [link (map second (re-seq #"href=\"(.*?)\"" (slurp f)))]
      (if (str/starts-with? link "http")
        nil                             ;ignore external links
        (when-not (fs/exists? (str "output/pages/" link))
          (prn :missing link :in (.getName f)))))))


(defn wayback
  [url]
  (let [resp (client/get "http://archive.org/wayback/available"
                         {:query-params {:url url}})]
    (clojure.data.json/read-str (:body resp) :keyword-fn keyword)))

;;; TODO convert bare URLs into [label](url)


;;; Highly connected nodes
(defn fan
  [bm]
  (let [bm (u/map-values #(assoc % :fan (count (bd/page-refs bm %))) bm)]
    (map (juxt :content :fan) (take 50 (reverse (sort-by :fan (filter :fan (vals bm))))))))
