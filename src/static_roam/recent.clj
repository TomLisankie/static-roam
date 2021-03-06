(ns static-roam.recent
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.parser :as parser]
            [static-roam.templating :as templating]))

;;; TODO filter out empty blocks
;;; TODO link path isnt right, formatting
;;; TODO if there are outline-nested blocks, they will repeat, which looks stupid

(defn recents
  "Groups recently changed blocks by page, returns rev chron seq of seqs"
  [block-map]
  (->> (reverse (sort-by :edit-time (filter :include? (vals block-map))))
       (map #(assoc % :page (:id (database/block-page block-map %))))
       (group-by :page)
       vals
       (sort-by (fn [blocks] (reduce max 0 (map :edit-time blocks))))
       reverse
       (take 10)))                      ;Last 10 pages, TODO maybe take a time period instead?

(defn recent-page-content
  [block-map]
  [:div.main
   ;; TODO prob needs row/col stuff
   [:div.row
    [:div.col-lg-8                     ;TODO no side col I guess
     [:div.ptitle
      [:h1 "Recent changes"]
      [:hr]
      (for [group (recents block-map)
             :let [page-id (:page (first group))
                   edit-time (reduce max 0 (map :edit-time group))]]
         [:div
          [:div.pheader
           "from " (parser/page-link (get block-map page-id))
           " "
           [:span (utils/render-time edit-time)]]
          (for [block (take 3 group)] ;limit to 3 chunks
            [:div.ragged (templating/block-template (:id block) block-map)])])]]]])


