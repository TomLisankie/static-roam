(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [stasis.core :as stasis]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.edn :as edn-utils]))

;; (defn- metadata-properties
;;   [metadata]
;;   (into (hash-map) (filter #(= 2 (count %)) (map #(str-utils/split % #":: ") metadata))))

;; (defn- content-for-block-id
;;   [block-id block-map]
;;   (let [block-props (get block-map block-id)]
;;     (:content block-props)))

;; (defn- site-metadata
;;   [block-map]
;;   (let [property-block-ids (:children (get block-map "SR Metadata"))
;;         property-block-content (map #(content-for-block-id % block-map) property-block-ids)
;;         prop-val-dict (metadata-properties property-block-content)]
;;     prop-val-dict))

;; (defn- create-nav-bar-page-dict
;;   [site-metadata-dict]
;;   (let [nav-bar-page-string (get site-metadata-dict "Nav Bar")
;;         nav-bar-pages-uncleaned (into
;;                                  (utils/find-content-entities-in-string nav-bar-page-string)
;;                                  (utils/find-hashtags-in-string nav-bar-page-string))
;;         nav-bar-pages (map utils/remove-double-delimiters nav-bar-pages-uncleaned)
;;         nav-bar-hrefs (map #(utils/page-title->html-file-title % true) nav-bar-pages)
;;         nav-bar-page-dict (zipmap nav-bar-hrefs nav-bar-pages)]
;;     nav-bar-page-dict))

;; (defn generate-pages-html
;;   [block-map output-dir]
;;   (let [html-file-names (utils/html-file-titles (keys block-map))
;;         generated-html (map #(hiccup/html (templating/page-hiccup % (get (site-metadata block-map) "Title") (create-nav-bar-page-dict (site-metadata block-map)) "../assets/css/static-roam.css" "../assets/js/extra.js"))
;;                             (map #(templating/block-page-template %1 %2 block-map)
;;                                  (keys block-map) (map :content (vals block-map))))
;;         file-name-to-content (zipmap
;;                               html-file-names
;;                               generated-html)]
;;     (stasis/export-pages
;;      file-name-to-content
;;      output-dir)))

;; (defn generate-index-of-pages-html
;;   [block-map output-dir]
;;   (let [html-file-name "/index.html"
;;         generated-html (hiccup/html
;;                         (templating/page-index-hiccup
;;                          (templating/list-of-page-links
;;                           (sort (keys block-map)) ".")
;;                          "../assets/css/static-roam.css"
;;                          "../assets/js/extra.js"))
;;         file-name-to-content {html-file-name generated-html}]
;;     (stasis/export-pages
;;      file-name-to-content
;;      output-dir)))

;; (defn- is-entry-point?
;;   [block-kv]
;;   (true? (:entry-point (second block-kv))))

;; (defn generate-home-page-html
;;   [block-map output-dir]
;;   (let [html-file-name "/index.html"
;;         entry-points (into (hash-map) (filter is-entry-point? block-map))
;;         generated-html (hiccup/html
;;                         (templating/home-page-hiccup
;;                          (templating/list-of-page-links
;;                           (sort (keys entry-points)) "pages" "entry-point-link")
;;                          (get (site-metadata block-map) "Title")
;;                          (create-nav-bar-page-dict (site-metadata block-map))
;;                          "./assets/css/static-roam.css"
;;                          "./assets/js/extra.js"))
;;         file-name-to-content {html-file-name generated-html}]
;;     (stasis/export-pages
;;      file-name-to-content
;;      output-dir)))

;; (defn- included?
;;   [block-kv]
;;   (let [block-props (second block-kv)]
;;     (true? (:included block-props))))

;; (defn generate-static-roam-html
;;   [roam-db output-dir config]
;;   ;; the roam-db passed in here (should) only consists now of blocks and pages that were marked for inclusion
;;   (let [template-info (:template-info config)
;;         included-block-map (into
;;                             (hash-map)
;;                             (map
;;                              vec
;;                              (filter included? block-map)))]
;;     (generate-pages-html included-block-map (str output-dir "/pages"))
;;     (generate-index-of-pages-html included-block-map (str output-dir "/pages"))
;;     (generate-home-page-html included-block-map output-dir)))

;; (defn- fill-out-template
;;   [roam-db sr-info-eid template-kv]
;;   (let [template-name (first template-kv)
;;         template-metadata (second template-kv)
;;         tag-eid (first
;;                  (first
;;                   (ds/q '[:find ?eid
;;                           :in $ ?tag
;;                           :where
;;                           [?eid :node/title ?tag]]
;;                         @roam-db-conn (:tagged-as template-metadata))))
;;         page-eids (map #(first %)
;;                        (ds/q '[:find ?parent-eid
;;                                :in $ ?tag-eid
;;                                :where
;;                                [?eid :block/refs sr-info-eid]
;;                                [?eid :block/refs ?tag-eid]
;;                                [?eid :block/parents ?parent-eid]]
;;                              @roam-db-conn tag-eid))
;;         page-uids (map #(get-page-uid roam-db %) page-eids)
;;         template-fn (get templates/template-fns template-name)
;;         filled-out-template-instances (map #(template-fn roam-db %) page-eids)
;;         uid-hiccup-map (zipmap page-uids filled-out-template-instances)]
;;     [template-name uid-hiccup-map]))

;; (defn- fill-out-templates
;;   [roam-db template-info]
;;   (let [sr-info-eid (first (first (ds/q '[:find ?eid
;;                                           :where
;;                                           [?eid :node/title "Static-Roam Info"]]
;;                                         @roam-db-conn)))]
;;     (into (hash-map) (map #(fill-out-template roam-db sr-info-eid %) template-info))))

;; (defn- make-path-template-map
;;   [folder uid-template-map]
;;   (into (hash-map) (map #(fn [kv] [(str folder "/" (first kv) ".html") (second kv)]) uid-template-map)))

;; (defn- create-and-save-html-from-hiccup-template
;;   [template-instance-kv template-info output-dir]
;;   ;; need to find folder to save the html in
;;   (let [template-name (first template-instance-kv)
;;         template-metadata (get template-info template-name)
;;         folder (str output-dir (:folder template-metadata))
;;         path-template-map (if (true? (:index template-metadata))
;;                             {(str folder "/index.html") (second (second template-instance-kv))}
;;                             (make-path-template-map folder (second template-instance-kv)))]
;;     )
;;   ;; need to generate html
;;   )

;; (defn- create-and-save-html-from-hiccup-templates
;;   [filled-out-template-map template-info output-dir]
;;   (map #(create-and-save-html-from-hiccup-template % template-info output-dir) filled-out-template-map))

(defn- get-path-html-pair
  [path-template-pair]
  (let [path (first path-template-pair)
        html (hiccup/html (second path-template-pair))]
    [path html]))

(defn- get-path-html-map
  [path-template-map]
  (into {} (map get-path-html-pair path-template-map)))

(defn generate-site
  [filled-out-templates output-dir]
  (let [path-html-map (get-path-html-map filled-out-templates)]
    (stasis/export-pages path-html-map output-dir)))
