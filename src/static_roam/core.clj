(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]
            [clojure.pprint :as pprint]))

(defn block-map
  [path-to-zip degree]
  (-> path-to-zip
      utils/read-roam-json-from-zip
      (database/setup-static-roam-block-map degree)))

(def last-bm (atom nil))

;;; Dump blocks in readable file
#_
(org.parkerici.multitool.cljcore/schppit "blocks.edn" @last-bm)

(defn tap
  [bm]
  (reset! last-bm bm)
  bm)

(defn -main
  [& [path-to-zip output-dir degree]]
  (-> (or path-to-zip (utils/latest-export))
      (block-map degree)
      tap
      (html-gen/generate-static-roam-html (or output-dir "output"))))




