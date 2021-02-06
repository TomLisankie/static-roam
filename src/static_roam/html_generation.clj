(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [org.parkerici.multitool.core :as u]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [static-roam.recent :as recent]
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
  [block-map block-id]
  [(utils/html-file-title block-id)     ;TODO bullshit
   (hiccup/html
    (templating/page-hiccup
     (templating/block-page-template block-id block-map)
     block-id                           ;TODO htmlize
     block-map
     ))])

(defn generate-page-hiccup
  [block-map block-id]
  (templating/page-hiccup
   (templating/block-page-template block-id block-map)
   block-id                           ;TODO htmlize
   block-map
   ))

(defn generate-pages-html
  [block-map output-dir]
  (let [page-map (map-filter-by-value :page? block-map)  ;Filter to pages only TODO maybe optionl
        file-name-to-content
        (into {}
              (map (partial generate-page-html block-map)
                   (keys page-map)))]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn export-page
  "Write out a single page. Content is hiccup. TODO use this more" 
  [content name output-dir]
  (let [fname (utils/html-file-title name)]
    (stasis/export-pages
     {fname (hiccup/html content)}
     output-dir)))

(defn generate-home-page-html
  [block-map output-dir]
  (let [entry-points (u/clean-map block-map (comp not :entry-point))]
    (export-page
     (templating/home-page-hiccup
      entry-points
      block-map)
     "/index.html"
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

(defn generate-static-roam-html
  [block-map output-dir]
  (let [included-block-map (u/clean-map block-map (comp not :included))]
    (generate-pages-html included-block-map (str output-dir "/pages"))
    (generate-home-page-html included-block-map output-dir)
    (generate-recent-page included-block-map output-dir)
    ))
