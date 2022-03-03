(ns goddinpotty.index
  (:require [goddinpotty.utils :as utils]
            [goddinpotty.batadase :as bd]
            [goddinpotty.templating :as templating]
            [goddinpotty.rendering :as render]
            [goddinpotty.config :as config]
            [org.parkerici.multitool.core :as u]
            [clojure.string :as s]))

;;; depth tree
;;; by size, # of refs (incoming/outgoing/both)

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

;;; Old one, delete 
#_
(defn make-index-pages
  [bm]
  (let [pages (remove :special? (bd/displayed-regular-pages bm))
        page-loc (fn [col] (str (or (:page-title col)
                                    (format "Index-%s" (:name col)))
                                ))]
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
        {(page-loc index)
         (templating/page-hiccup hiccup title title bm)}
        )))))

;;; Copied with mods from html-gen due to namespace fuck
(defn generated-page
  "Add a generated page to the block map"
  [name generator]
  {:id name
   :title name
   :special? true                ;I miss OOP
   :generator generator
   :include? true
   :page? true
   })

;;; Returns map to merge into bm
(defn make-index-pages
  [bm]
  (let [pages (remove :special? (bd/displayed-regular-pages bm))
        page-id (fn [name] (format "Index-%s" name))
        ]
    (into {}
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
                          [:a {:href (page-id (:name col))} (:name col)])])]]
                   [:tbody 
                    (for [page (sort-by sort-key (filter filter-key pages))]
                      [:tr
                       (for [col indexes]
                         [:td
                          (u/ignore-errors
                           ((:render col) page))])])
                    ]]
                  title  (format "Index by %s" name)]
              [(page-id name)
               (generated-page
                (page-id name)
                (fn [bm]
                  ;; TODO this no longer does the export
                  (templating/page-hiccup hiccup title title bm)
                  ))])))))


