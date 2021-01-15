(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [stasis.core :as stasis]))

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
        ;; TODO ugly but works to get italics in titles rendered properly. Should do same for backlinks
        nav-bar-pages-r (map #(static-roam.parser/block-content->hiccup % {}) 
                            nav-bar-pages)
        nav-bar-hrefs (map #(utils/page-title->html-file-title % true) nav-bar-pages)
        nav-bar-page-dict (zipmap nav-bar-hrefs nav-bar-pages-r)]
    nav-bar-page-dict))

;;; → multitool, should replace dissoc-if
(defn map-filter-by-value
  [f m]
  (apply dissoc m (keep #(if (f (val %))
                           nil
                           (key %))
                        m)))

;;; This is relative to output TODO probably copy into output?
(def site-css "../../resources/public/hyper-roam.css") ;TODO make this user settable somehow

(defn generate-page-html
  [block-map block-id]
  [(utils/html-file-title block-id)
   (hiccup/html
    (templating/page-hiccup
     (templating/block-page-template block-id block-map)
     ;(get (site-metadata block-map) "Title")
     block-id                           ;TODO htmlize
     (create-nav-bar-page-dict (site-metadata block-map))
     [[:link {:rel "stylesheet"
              :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
              :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
              :crossorigin "anonymous"}]
      [:link {:rel "stylesheet" :href site-css}] 
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Martel:wght@400;600&display=swap"}]]))])

(defn generate-pages-html
  [block-map output-dir]
  (let [page-map (map-filter-by-value :page block-map)  ;Filter to pages only TODO maybe optionl
        file-name-to-content
        (into {}
              (map (partial generate-page-html block-map)
                   (keys page-map)))]
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
  [block-map output-dir]
  (let [included-block-map (into
                            (hash-map)
                            (map
                             vec
                             (filter included? block-map)))]
    (generate-pages-html included-block-map (str output-dir "/pages"))
    (generate-index-of-pages-html included-block-map (str output-dir "/pages"))
    (generate-home-page-html included-block-map output-dir)))
