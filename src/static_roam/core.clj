(ns static-roam.core
  (:require [static-roam.config :as config]
            [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]))

(defn block-map
  [path-to-zip]
  (prn :reading-from path-to-zip)       ;TODO I suppose real logger is called for
  (-> path-to-zip
      utils/read-roam-json-from-zip
      (database/setup-static-roam-block-map)))

(def last-bm (atom nil))


(defn pages
  []
  (keys (u/dissoc-if (fn [[_ v]] (not (:page? v))) @last-bm)))

;;; Sloooow. Dumps pages including dchildren
(defn page-dump
  []
  ;; TODO output-dir
  (ju/schppit
   "pages.edn"
   (u/map-filter (fn [b] (and (:page? b) b))
                 (vals @last-bm))))

;;; Dumps included blocks in order; the idea is this should be diffable. Also slow.
(defn block-dump
  []
  (ju/schppit
   "blocks.edn"
   (into (sorted-map)
         (u/map-values #(dissoc % :dchildren)
                       (u/clean-map @last-bm (comp not :include?))))))

(defn tap
  [bm]
  (reset! last-bm bm)
  bm)

(defn -main
  [& [path-to-zip]]
  (prn config/config)
  (-> (or path-to-zip (utils/latest-export))
      block-map
      tap
      (html-gen/generate-static-roam-html (:output-dir config/config)))
  (prn (database/stats @last-bm))
  #_ (dump))

(defn gen-page
  [page]
  (html-gen/export-page
   (html-gen/generate-page-hiccup @last-bm page)
   (utils/html-file-title page)
   "output/pages"))                     ;TODO

(defn gen-pages
  []
  (html-gen/generate-static-roam-html @last-bm (:output-dir config/config)))

#_
(def pages (map #(select-keys % [:content :depth :refs :linked-by]) (filter :page? (vals bm))))
#_
(def g (group-by :depth pages))


