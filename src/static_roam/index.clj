(ns static-roam.index
  (:require [static-roam.utils :as utils]
            [static-roam.database :as db]
            [static-roam.templating :as templating]
            [clojure.string :as s]
            [clojure.pprint :as pprint]))

(def edit-time
  (memoize
   (fn 
     [page]
     (second (db/date-range page)))))

(def size
  (memoize
   (fn [page]
     (reduce +
             (count (or (:content page) 0))
             (map size
                  (filter :include? (:dchildren page)))))))

;;; depth tree
;;; by size, # of refs (incoming/outgoing/both)

(def indexes
  [{:title "Title"
    :sort-key (comp s/upper-case :content)
    :render (comp templating/page-link :content)
    }
   {:title "Date"
    :sort-key (comp - inst-ms edit-time)
    :render (comp utils/render-time edit-time)}
   {:title "Depth"
    :sort-key :depth
    :render :depth}
   {:title "Size"
    :sort-key (comp - size)
    :render #(format "%.1fK" (double (/ (size %) 1000)))}
   ])


(defn make-index-pages
  [bm]
  (let [pages (filter :include? (db/pages bm))
        page-loc (fn [col] (format "%s-index.html" (:title col)))]
    (apply
     merge
     (for [{:keys [title sort-key] :as index} indexes]
       (let [hiccup
             [:div.main
              [:div.ptitle
               [:h1 (str "Index by " title)]]
              [:table.table.table-sm.table-hover 
               [:thead
                ;; col headers
                [:tr
                 (for [col indexes]
                   [:th {:scope "col"}
                    (if (= (:title col) title)
                      (:title col)
                      [:a {:href (page-loc col)} (:title col)])])]]
               [:tbody 
                (for [page (sort-by sort-key pages)]
                  [:tr
                   (for [col indexes]
                     [:td
                      ((:render col) page)])])
                ]]]]
         {(str "/pages/" (page-loc index))    ;pkm
          (templating/page-hiccup hiccup (format "Index by %s" title) bm)}
         )))))

