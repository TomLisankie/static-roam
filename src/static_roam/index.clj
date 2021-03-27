(ns static-roam.index
  (:require [static-roam.utils :as utils]
            [static-roam.database :as db]
            [static-roam.templating :as templating]
            [static-roam.parser :as parser]
            [static-roam.config :as config]
            [clojure.string :as s]))



;;; depth tree
;;; by size, # of refs (incoming/outgoing/both)

(def indexes
  [{:name "Title"
    :sort-key (comp s/upper-case :content)
    :render parser/page-link
    :page-title "Index"                 ;kludge to match block-map and links
    }
   {:name "Date"
    :sort-key (comp - inst-ms db/edit-time)
    :render (comp utils/render-time db/edit-time)}
   {:name "Depth"
    :sort-key :depth
    :render :depth}
   {:name "Size"
    :sort-key (comp - db/size)
    :render #(format "%.1fK" (double (/ (db/size %) 1000)))}
   ])

(defn make-index-pages
  [bm]
  (let [pages (remove :special? (db/displayed-regular-pages bm))
        page-loc (fn [col] (str (or (:page-title col)
                                    (format "Index-%s" (:name col)))
                                ".html"))]
    (apply
     merge
     (for [{:keys [name sort-key] :as index} indexes]
       (let [hiccup
             [:div.main
              [:div.ptitle
               [:h1 (str "Index by " name)]]
              [:table.table.table-sm.table-hover 
               [:thead
                ;; col headers
                [:tr
                 (for [col indexes]
                   [:th {:scope "col"}
                    (if (= (:name col) name)
                      (:name col)
                      [:a {:href (page-loc col)} (:name col)])])]]
               [:tbody 
                (for [page (sort-by sort-key pages)]
                  [:tr
                   (for [col indexes]
                     [:td
                      ((:render col) page)])])
                ]]]]
         {(str "/pages/" (page-loc index))    ;pkm
          (templating/page-hiccup hiccup (format "Index by %s" name) bm)}
         )))))

