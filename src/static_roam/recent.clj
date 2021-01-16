(ns static-roam.recent
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]
            [static-roam.templating :as templating]
            [clojure.pprint :as pprint]))

(defn recents
  [block-map]
  ;; TODO presumably this should be limitied to visible blocks!
  (take 100 (reverse (sort-by :edit-time (vals block-map)))))

(defn log-page-content
  [block-map]
  (templating/linked-references-template
   (map :id (recents block-map))
   block-map))

(defn log-page
  [block-map output-dir]
  (html-gen/export-page
   (templating/page-hiccup
   (log-page-content block-map)
   "Recently changed"
   {}
   nil)
   "/recent-changes.html"
   output-dir))
