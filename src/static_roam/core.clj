(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.pprint :as pprint]))

(defn block-map
  [path-to-zip]
  (prn :reading-from path-to-zip)       ;TODO I suppose real logger is called for
  (-> path-to-zip
      utils/read-roam-json-from-zip
      (database/setup-static-roam-block-map)))

(def last-bm (atom nil))

;;; Dump blocks in readable file
#_
(org.parkerici.multitool.cljcore/schppit "blocks.edn" @last-bm)

(defn pages
  []
  (keys (u/dissoc-if (fn [[_ v]] (not (:page? v))) @last-bm)))

;;; SLoooow. Dumps pages including dchildren
(defn dump
  []
  (ju/schppit "blocks.edn" (u/map-filter (fn [[k b]] (and (:page? b) b))
                                         @last-bm)))

(defn tap
  [bm]
  (reset! last-bm bm)
  bm)

(defn -main
  [& [path-to-zip output-dir]]
  (-> (or path-to-zip (utils/latest-export))
      block-map
      tap
      (html-gen/generate-static-roam-html (or output-dir "output")))
  (prn (database/stats @last-bm))
  #_ (dump))

(defn gen-page
  [page]
  (html-gen/export-page
   (html-gen/generate-page-hiccup @last-bm page)
   (utils/html-file-title page)
   "output/pages"))

(defn gen-pages
  []
  (html-gen/generate-static-roam-html @last-bm "output"))

#_
(def pages (map #(select-keys % [:content :depth :refs :linked-by]) (filter :page? (vals bm))))
#_
(def g (group-by :depth pages))


