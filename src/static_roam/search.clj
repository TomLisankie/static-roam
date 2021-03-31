(ns static-roam.search
  (:require [org.parkerici.multitool.core :as u]
            [static-roam.database :as db]
            [static-roam.utils :as utils])
  )

;;; http://elasticlunr.com/elasticlunr.min.js

(defn index
  [bm]
  (u/for* [page (db/displayed-pages bm)
           index (range)]
    {:id index                          ;TODO not sure this is necessary, we don't use it
     :url (utils/html-file-title (:id page))
     :title (:id page)
     :body (db/block-text bm page)}))

(defn write-index
  [bm output-dir]
  (utils/write-json (str output-dir "/index.js")
                    (index bm)))

