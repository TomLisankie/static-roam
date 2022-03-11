(ns goddinpotty.search
  (:require [org.parkerici.multitool.core :as u]
            [goddinpotty.batadase :as bd]
            [goddinpotty.rendering :as render]
            [goddinpotty.utils :as utils])
  )

;;; http://elasticlunr.com/elasticlunr.min.js

;;; TODO should index aliases

(defn index
  [bm]
  (u/for* [page (bd/displayed-pages bm)
           index (range)]
    {:id index                          ;TODO not sure this is necessary, we don't use it
     :url (utils/html-file-title (:id page))
     ;; TODO should strip markup like __foo__, would be nice if it could be rendered in search results, but that looks tricky
     :title (:title page) 
     :body (render/block-full-text bm page)}))

(defn write-index
  [bm output-dir]
  (utils/write-json (str output-dir "/assets/index.js")
                    (index bm)))

(defn search-head
  []
  [[:script {:src "http://elasticlunr.com/elasticlunr.min.js"}]
   [:script {:src "assets/search.js"}]])
