(ns static-roam.recent
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.parser :as parser]
            [static-roam.templating :as templating]
            [org.parkerici.multitool.core :as u]
            ))

;;; TODO should really link to block within page.
;;; TODO filter out empty blocks

(u/defn-memoized real-edit-time
  [block]
  (second (database/date-range block)))

(defn trim-redundants-1
  [blocks]
  (cond (empty? (rest blocks)) blocks
        (some #(database/block-contains? (first blocks) %) (rest blocks))
        (trim-redundants-1 (rest blocks))
        :else
        (cons (first blocks) (trim-redundants-1 (rest blocks)))))

;;; This is not right (it can get pages, or innermost changed blocks if inverted, but that's not what you want)
;;; total hack, but I'm in a lazy mood and these won't be that big
(defn trim-redundants
  [blocks]
  (trim-redundants-1
   (reverse (trim-redundants-1 blocks))))


;;; OK this is a hacky alorithm, but works more or less
(defn recents
  "Groups recently changed blocks by page, returns rev chron seq of seqs"
  [block-map]
  ;; get the 100 latest changed leaf blocks TODO parameterize
  (->> block-map
       vals
       (filter :include?)
       (remove :special?)
       (filter database/leaf?)
       (sort-by :edit-time u/>*)     
       (take 100)
       ;; Expand small ones TODO parameterize
       (map #(database/expand-to block-map % 101))
       ;; remove duplicates
       distinct
       trim-redundants
       ;; group by page
       (map #(assoc % :page (:id (database/block-page block-map %))))
       (group-by :page)
       vals
       (sort-by (fn [blocks] (u/max* (map real-edit-time blocks))))
       reverse))

(defn recent-page-content
  [block-map]
  [:div.main
   [:div.row
    [:div.col-lg-8                     ;TODO no side col I guess
     [:div.ptitle
      [:h1 "Recent changes"]
      [:hr]
      (for [group (recents block-map)
             :let [page-id (:page (first group))
                   edit-time (u/max* (map real-edit-time group))]]
         [:div
          [:div.pheader
           "from " (parser/page-link (get block-map page-id))
           " "
           [:span (utils/render-time edit-time)]]
          (for [block (take 3 group)] ;limit to 3 chunks
            [:div.ragged (templating/block-template (:id block) block-map)])])]]]])


