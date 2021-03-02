(ns static-roam.index
  (:require [static-roam.utils :as utils]
            [static-roam.database :as db]
            [static-roam.templating :as templating]
            [clojure.pprint :as pprint]))

(def edit-time
  (memoize
   (fn 
     [page]
     (second (db/date-range page)))))

(defn index-page
  [bm sorter title]
  (let [pages (filter :include? (db/pages bm))
        sorted (sort-by sorter pages)]
    `[:div.main
      ;; TODO prob needs row/col stuff
      [:h1.ptitle ~title]
      [:table
      ~@(for [page sorted]
          `[:tr
            [:td
             ~(templating/page-link (:content page))]
            [:td
             ~(utils/render-time (edit-time page))]])]
      ]))
