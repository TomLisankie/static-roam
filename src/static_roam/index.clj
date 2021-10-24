(ns static-roam.index
  (:require [static-roam.utils :as utils]
            [static-roam.batadase :as bd]
            [static-roam.templating :as templating]
            [static-roam.rendering :as render]
            [static-roam.config :as config]
            [org.parkerici.multitool.core :as u]
            [clojure.string :as s]))

;;; depth tree
;;; by size, # of refs (incoming/outgoing/both)

;;; TODO may need changes since we have separate create-time

(def indexes
  [{:name "Title"
    :sort-key (comp s/upper-case :title)
    :render render/page-link
    :page-title "Index"                 ;kludge to match block-map and links
    :col-width "65%"
    }
   {:name "Date"
    :filter-key bd/edit-time
    :sort-key (comp - inst-ms bd/edit-time)
    :render (comp utils/render-time bd/edit-time)}
   {:name "Depth"
    :sort-key :depth
    :render :depth}
   {:name "Size"
    :sort-key (comp - bd/size)
    :render #(format "%.1fK" (double (/ (bd/size %) 1000)))}
   ])

(defn make-index-pages
  [bm]
  (let [pages (remove :special? (bd/displayed-regular-pages bm))
        page-loc (fn [col] (str (or (:page-title col)
                                    (format "Index-%s" (:name col)))
                                ".html"))]
    (apply
     merge
     (for [{:keys [name sort-key filter-key] :as index :or {filter-key identity}} indexes]
       (let [hiccup
             [:table.table.table-sm.table-hover 
              [:thead
               ;; col headers
               [:tr
                (for [col indexes]
                  [:th {:scope "col" :style (when (:col-width col)
                                              (format "width: %s;" (:col-width col)))}
                   (if (= (:name col) name)
                     (:name col)
                     [:a {:href (page-loc col)} (:name col)])])]]
              [:tbody 
               (for [page (sort-by sort-key (filter filter-key pages))]
                 [:tr
                  (for [col indexes]
                    [:td
                     (u/ignore-errors
                      ((:render col) page))])])
               ]]
             title  (format "Index by %s" name)]
         {(str "/pages/" (page-loc index))    ;pkm
          (templating/page-hiccup hiccup title title bm)}
         )))))

