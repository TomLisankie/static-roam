(ns goddinpotty.html-generation
  (:require [org.parkerici.multitool.core :as u]
            [hiccup.core :as hiccup]
            [me.raynes.fs :as fs]
            [goddinpotty.utils :as utils]
            [goddinpotty.templating :as templating]
            [goddinpotty.batadase :as bd]
            [goddinpotty.recent :as recent]
            [goddinpotty.index :as index]
            [goddinpotty.search :as search]
            [goddinpotty.config :as config]))

;;; Translate Hiccup into actual pages.
;;; NOTE: I wish Hiccup had an option to prettyprint HTML, but it doesn't
;;; This works https://prettydiff.com/?m=beautify&html
;;;  actually no, it introduces spaces that have an effect on rendering! Fuck!

(defn page-hiccup
  [block-map output-dir block]
  (let [block-id (:id block)]
    (templating/block-page-hiccup block-id block-map output-dir)
    ))

;;; Unaccountably not in fs
(defn ensure-directories
  [path]
  (let [dir (subs path 0 (- (count path) (count (fs/base-name path))))]
    (fs/mkdirs dir)))

(defn export-page
  "Write out a single page. Content is hiccup. " 
  [content fname output-dir]
  (ensure-directories (str output-dir fname))
  (spit (str output-dir fname)
        (hiccup/html content)))

(defn export-pages
  "Write out pages. Content is map of filenames → hiccup." 
  [content output-dir]
  (doseq [[fname hiccup] content]
    (export-page hiccup fname output-dir)))

(defn generate-content-page
  [block-map output-dir block]
  (prn :generate-page (:title block))
  (let [fname (str "/" (utils/html-file-title (:title block)))]
    (export-page (if (:special? block)
                   ((:generator block) block-map)
                   (page-hiccup block-map output-dir block))
                 fname
                 output-dir)))

(defn generate-index-redir
  [output-dir]
  (export-page
   [:meta {:http-equiv "refresh"
           :content (format "0; url=%s" (str (utils/html-file-title (config/config :main-page))))}]
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
   "/New.html"
   output-dir))

(defn generate-index-pages
  [block-map output-dir]
  (export-pages
   (index/make-index-pages block-map)
   output-dir))

(defn generate-global-map
  [bm]
   (templating/map-page bm)
  )

(defn generate-goddinpotty
  [block-map output-dir]
  (generate-content-pages block-map output-dir)
  (search/write-index block-map output-dir)
  )

(defn generated-page
  "Add a generated page to the block map"
  [block-map name generator]
  (assoc block-map name
         {:id name
          :title name
          :special? true                ;I miss OOP
          :generator generator
          :include? true
          :page? true
          }))

#_
(defn generated-pages
  "Add a generated page to the block map"
  [block-map pages]
  (reduce (fn [bm [name page]]
            (assoc bm name
                   {:id name
                    :title name
                    :special? true                ;I miss OOP
                    :generator generator
                    :include? true
                    :page? true
                    }))
          pages))
