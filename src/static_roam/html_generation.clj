(ns static-roam.html-generation
  (:require [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [static-roam.database :as db]
            [static-roam.recent :as recent]
            [static-roam.index :as index]
            [static-roam.config :as config]))

(defn page-hiccup
  [block-map output-dir block]
  (let [block-id (:id block)]
    (templating/page-hiccup
     (templating/block-page-template block-id block-map output-dir)
     block-id                           ;TODO htmlize
     block-map
     ;; TODO should be under conditional
     [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.0"}]
      [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.16.0"}]]
     )))

(defn export-page
  "Write out a single page. Content is hiccup. " 
  [content fname output-dir]
  (spit (str output-dir fname)
        (hiccup/html content)))

(defn export-pages
  "Write out pages. Content is map of filenames → hiccup." 
  [content output-dir]
  (doseq [[fname hiccup] content]
    (export-page hiccup fname output-dir)))

(defn generate-content-page
  [block-map output-dir block]
  (prn :generate-page (:id block))
  (if (:special? block)                 ;I miss OOP
    ((:generator block) block-map output-dir)
    (export-page (page-hiccup block-map output-dir block)
                 (str "/pages/" (utils/html-file-title (:id block)))
                 output-dir)))

(defn generate-content-pages
  [block-map output-dir]
  (doseq [page (db/displayed-pages block-map)]
    (generate-content-page block-map output-dir page)))

(defn generate-home-page
  [block-map output-dir]
  (let [entry-points (db/entry-points block-map)]
    (export-page
     (templating/home-page-hiccup
      entry-points
      block-map)
     "/pages/index.html"
     output-dir)))

(defn generate-recent-page
  [block-map output-dir]
  (export-page
   (templating/page-hiccup
    (recent/recent-page-content block-map)
    "Recently changed"
    block-map)
   "/pages/recent-changes.html"
   output-dir))

(defn generate-index-pages
  [block-map output-dir]
  (export-pages
   (index/make-index-pages block-map)
   output-dir))

(defn generate-global-map
  [bm output-dir]
  (export-page
   (templating/map-page bm output-dir)
   "/pages/map.html"
   output-dir))

(defn generate-static-roam
  [block-map output-dir]
  (generate-content-pages block-map output-dir))

(defn generated-page
  "Add a generated page to the block map"
  [block-map name generator]
  (assoc block-map name
         {:id name
          :special? true                ;I miss OOP
          :generator generator
          :include? true
          :page? true
          }))
