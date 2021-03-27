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
  [{:title "Title"
    :sort-key (comp s/upper-case :content)
    :render parser/page-link
    }
   {:title "Date"
    :sort-key (comp - inst-ms db/edit-time)
    :render (comp utils/render-time db/edit-time)}
   {:title "Depth"
    :sort-key :depth
    :render :depth}
   {:title "Size"
    :sort-key (comp - db/size)
    :render #(format "%.1fK" (double (/ (db/size %) 1000)))}
   ])


(defn make-index-pages
  [bm]
  (let [pages (remove :special? (db/displayed-regular-pages bm))
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

