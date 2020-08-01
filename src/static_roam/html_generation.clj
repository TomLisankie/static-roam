(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [datascript.core :as ds]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [stasis.core :as stasis]))

(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(str-utils/split % #":: ") metadata))))

(defn- site-metadata
  [conn]
  (let [properties (first (ds/q '[:find ?children
                                  :where
                                  [?id :block/id "SR Metadata"]
                                  [?id :block/children ?children]]
                                @conn))
        metadata (map #(:block/content (ds/entity @conn [:block/id %])) properties)
        prop-val-dict (metadata-properties metadata)]
    prop-val-dict))

(defn generate-pages-html
  [conn output-dir]
  (let [html-file-names (utils/html-file-titles
                         (sort-by
                          #(first %)
                          (ds/q '[:find ?included-id ?block-title
                                  :where
                                  [?included-id :block/included true]
                                  [?included-id :block/id ?block-title]]
                                @conn)))
        generated-html (map #(hiccup/html (templating/page-hiccup % "../assets/css/main.css" "../assets/js/extra.js"))
                            (map #(templating/block-page-template % conn)
                                 (sort-by
                                  #(first %)
                                  (ds/q '[:find ?included-id ?content
                                          :where
                                          [?included-id :block/included true]
                                          [?included-id :block/content ?content]]
                                        @conn))))
        file-name-to-content (zipmap
                              html-file-names
                              generated-html)]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn generate-index-of-pages-html
  [conn output-dir]
  (let [html-file-name "/index.html"
        generated-html (hiccup/html
                        (templating/page-index-hiccup
                         (templating/list-of-page-links
                          (sort (ds/q '[:find ?included-page-title
                                        :where
                                        [?id :block/page true]
                                        [?id :block/included true]
                                        [?id :block/id ?included-page-title]]
                                      @conn)) ".")
                         "../assets/css/main.css"
                         "../assets/js/extra.js"))
        file-name-to-content {html-file-name generated-html}]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn generate-home-page-html
  [conn output-dir]
  (let [html-file-name "/index.html"
        generated-html (hiccup/html
                        (templating/home-page-hiccup
                         (templating/list-of-page-links
                          (sort (ds/q '[:find ?entry-point-content
                                        :where
                                        [?id :block/included true]
                                        [?id :block/entry-point true]
                                        [?id :block/content ?entry-point-content]]
                                      @conn)) "pages" "entry-point-link")
                         (get (site-metadata conn) "Title")
                         "./assets/css/main.css"
                         "./assets/js/extra.js"))
        file-name-to-content {html-file-name generated-html}]
    (stasis/export-pages
     file-name-to-content
     output-dir)))

(defn generate-static-roam-html
  [conn output-dir]
  (generate-pages-html conn (str output-dir "/pages"))
  (generate-index-of-pages-html conn (str output-dir "/pages"))
  (generate-home-page-html conn output-dir))
