(ns static-roam.html-generation
  (:require [org.parkerici.multitool.core :as u]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [static-roam.batadase :as bd]
            [static-roam.recent :as recent]
            [static-roam.index :as index]
            [static-roam.search :as search]
            [static-roam.config :as config]))

;;; Translate Hiccup into actual pages.
;;; NOTE: I wish Hiccup had an option to prettyprint HTML, but it doesn't
;;; This works https://prettydiff.com/?m=beautify&html
;;;  actually no, it introduces spaces that have an effect on rendering! Fuck!

(defn page-hiccup
  [block-map output-dir block]
  (let [block-id (:id block)]
    (templating/block-page-hiccup block-id block-map output-dir)
    ))

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

(defn generate-index-redir
  [output-dir]
  (export-page
   [:meta {:http-equiv "refresh"
           :content (format "0; url=%s" (str "pages/"  (utils/html-file-title (config/config :main-page))))}]
   "/index.html"
   output-dir))

(defn generate-content-pages
  [block-map output-dir]
  (doseq [page (bd/displayed-pages block-map)]
    (generate-content-page block-map output-dir page)))

;;; If there are any more special pages; should get more declarative about these generators.

;;; Yeah this sucks, each special page has like 3 parts where 1 would do (here, templating, and core).

;;; Out of service for Logseq
(defn generate-recent-page
  [block-map output-dir]
  (export-page
   (templating/page-hiccup
    (recent/recent-page-content block-map)
    "Recently changed"
    "Recently changed"
    block-map)
   "/pages/New.html"
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
   "/pages/Map.html"
   output-dir))

(defn generate-static-roam
  [block-map output-dir]
  (generate-content-pages block-map output-dir)
  (search/write-index block-map output-dir)
  )

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
