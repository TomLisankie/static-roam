(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [stasis.core :as stasis]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.edn :as edn-utils]))

(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(str-utils/split % #":: ") metadata))))

(defn- content-for-block-id
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (:content block-props)))

(defn- site-metadata
  [block-map]
  (let [property-block-ids (:children (get block-map "SR Metadata"))
        property-block-content (map #(content-for-block-id % block-map) property-block-ids)
        prop-val-dict (metadata-properties property-block-content)]
    prop-val-dict))

(defn- create-nav-bar-page-dict
  [site-metadata-dict]
  (let [nav-bar-page-string (get site-metadata-dict "Nav Bar")
        nav-bar-pages-uncleaned (into
                                 (utils/find-content-entities-in-string nav-bar-page-string)
                                 (utils/find-hashtags-in-string nav-bar-page-string))
        nav-bar-pages (map utils/remove-double-delimiters nav-bar-pages-uncleaned)
        nav-bar-hrefs (map #(utils/page-title->html-file-title % true) nav-bar-pages)
        nav-bar-page-dict (zipmap nav-bar-hrefs nav-bar-pages)]
    nav-bar-page-dict))

(defn generate-pages-html
  [block-map output-dir]
  (let [html-file-names (utils/html-file-titles (keys block-map))
        generated-html (map #(hiccup/html (templating/page-hiccup % (get (site-metadata block-map) "Title") (create-nav-bar-page-dict (site-metadata block-map)) "../assets/css/static-roam.css" "../assets/js/extra.js"))
                            (map #(templating/block-page-template %1 %2 block-map)
                                 (keys block-map) (map :content (vals block-map))))
        file-name-to-content (zipmap
                              html-file-names
                              generated-html)]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn generate-index-of-pages-html
  [block-map output-dir]
  (let [html-file-name "/index.html"
        generated-html (hiccup/html
                        (templating/page-index-hiccup
                         (templating/list-of-page-links
                          (sort (keys block-map)) ".")
                         "../assets/css/static-roam.css"
                         "../assets/js/extra.js"))
        file-name-to-content {html-file-name generated-html}]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn- is-entry-point?
  [block-kv]
  (true? (:entry-point (second block-kv))))

(defn generate-home-page-html
  [block-map output-dir]
  (let [html-file-name "/index.html"
        entry-points (into (hash-map) (filter is-entry-point? block-map))
        generated-html (hiccup/html
                        (templating/home-page-hiccup
                         (templating/list-of-page-links
                          (sort (keys entry-points)) "pages" "entry-point-link")
                         (get (site-metadata block-map) "Title")
                         (create-nav-bar-page-dict (site-metadata block-map))
                         "./assets/css/static-roam.css"
                         "./assets/js/extra.js"))
        file-name-to-content {html-file-name generated-html}]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn- included?
  [block-kv]
  (let [block-props (second block-kv)]
    (true? (:included block-props))))

(defn generate-static-roam-html
  [roam-db output-dir]
  (let [included-block-map (into
                            (hash-map)
                            (map
                             vec
                             (filter included? block-map)))]
    (generate-pages-html included-block-map (str output-dir "/pages"))
    (generate-index-of-pages-html included-block-map (str output-dir "/pages"))
    (generate-home-page-html included-block-map output-dir)))
