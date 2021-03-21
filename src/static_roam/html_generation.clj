(ns static-roam.html-generation
  (:require [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [static-roam.database :as db]
            [static-roam.recent :as recent]
            [static-roam.index :as index]
            [static-roam.graph :as graph]
            [static-roam.config :as config]
            [stasis.core :as stasis]))

;;; → multitool, should replace dissoc-if
(defn map-filter-by-value
  [f m]
  (apply dissoc m (keep #(if (f (val %))
                           nil
                           (key %))
                        m)))

;;; This is relative to output TODO probably copy into output?
(defn generate-page-html
  [block-map block]
  (let [block-id (:id block)]
    (prn :generate-page block-id)
    (hiccup/html
     (templating/page-hiccup
      (templating/block-page-template block-id block-map)
      block-id                           ;TODO htmlize
      block-map
      ))))

(defn generate-page-hiccup
  [block-map block-id]
  (templating/page-hiccup
   (templating/block-page-template block-id block-map)
   block-id                           ;TODO htmlize
   block-map
   ))

(defn generate-pages-html
  [block-map output-dir]
  (let [pages (db/displayed-pages block-map)
        file-name-to-content
        (zipmap (map (comp utils/html-file-title :id) pages)
                (map (partial generate-page-html block-map)
                     pages))]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn export-hiccup-pages
  "Write out pages. Content is map of filenames → hiccup." 
  [content output-dir]
  (stasis/export-pages
   (u/map-values #(hiccup/html %) content)
   output-dir))

(defn export-page
  "Write out a single page. Content is hiccup. " 
  [content fname output-dir]
    (stasis/export-pages
     {fname (hiccup/html content)}
     output-dir))

(defn generate-home-page-html
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
   "/pages/recent-changes.html"         ;TODO should be up a level, but links need adjusting
   output-dir))

(defn generate-index-pages
  [block-map output-dir]
  (export-hiccup-pages
   (index/make-index-pages block-map)
   output-dir))

(defn generate-global-map
  [bm output-dir]
  (export-page
   (graph/generate-map bm output-dir {:name "ammdi"
                                      :include-all? (:unexclude? config/config)
                                      })
   "/pages/map.html"
   output-dir))

(defn generate-static-roam-html
  [block-map output-dir]
  (generate-pages-html block-map (str output-dir "/pages"))
  (generate-home-page-html block-map output-dir)
  (generate-recent-page block-map output-dir)
  (generate-index-pages block-map output-dir)
  (generate-global-map block-map output-dir)
  )




