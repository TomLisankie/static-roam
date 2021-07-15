(ns static-roam.templating-test
  (:require [static-roam.templating :refer :all]
            [static-roam.database :as db]
            [org.parkerici.multitool.core :as u]
            [clojure.test :refer :all]))

;;; → utils
(defn structure-contains?
  [elt struct]
  (u/walk-find #(= % elt) struct))


(deftest formatted-page-title-test
  (let [page 
        (db/parse-block
        '{:include? true,
          :content "__On Purpose__",
          :refs #{},
          :edit-time #inst "2021-05-31T03:47:57.271-00:00"
          :page? true,
          :id "__On Purpose__" ,
          :depth 4,
          :heading -1})
        hiccup (block-page-hiccup "__On Purpose__"  {"__On Purpose__" page} "/output") ]
    (is (structure-contains? [:h1 [:i "On Purpose"]] hiccup))))
