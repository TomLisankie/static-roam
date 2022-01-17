(ns goddinpotty.recent
  (:require [goddinpotty.utils :as utils]
            [goddinpotty.batadase :as bd]
            [goddinpotty.rendering :as render]
            [goddinpotty.templating :as templating]
            [org.parkerici.multitool.core :as u]
            ))

;;; Broken with Logseq because we don't really have block-level change times
;;; For now, this is disabled and the recent page list will have to do.


;;; TODO should really link to block within page.
;;; TODO filter out empty blocks

(u/defn-memoized real-edit-time
  [block]
  (second (bd/date-range block)))

(defn trim-redundants-1
  [blocks]
  (cond (empty? (rest blocks)) blocks
        (some #(bd/block-contains? (first blocks) %) (rest blocks))
        (trim-redundants-1 (rest blocks))
        :else
        (cons (first blocks) (trim-redundants-1 (rest blocks)))))

;;; This is not right (it can get pages, or innermost changed blocks if inverted, but that's not what you want)
;;; total hack, but I'm in a lazy mood and these won't be that big
(defn trim-redundants
  [blocks]
  (trim-redundants-1
   (reverse (trim-redundants-1 blocks))))


;;; OK this is a hacky algorithm, but works more or less
(defn recents
  "Groups recently changed blocks by page, returns rev chron seq of seqs"
  [block-map]
  ;; get the 100 latest changed leaf blocks TODO parameterize
  (->> block-map
       vals
       (filter bd/block?)               ;shouldn't be necessary but there is some weird shit
       (filter bd/displayed?)
       (remove :special?)
       (filter bd/leaf?)
       (sort-by :edit-time u/>*)     
       (take 100)
       ;; Expand small ones TODO parameterize
       (map #(bd/expand-to block-map % 101))
       ;; remove duplicates
       distinct
       trim-redundants
       ;; group by page
       (map #(assoc % :page (:id (bd/block-page block-map %))))
       (group-by :page)
       vals
       (sort-by (fn [blocks] (u/max* (map real-edit-time blocks))))
       reverse))

(defn recent-page-content
  [block-map]
  [:div
   (for [group (recents block-map)
         :let [page-id (:page (first group))
               edit-time (u/max* (map real-edit-time group))]]
     [:div
      [:div.pheader
       "from " (render/page-link (get block-map page-id))
       " "
       [:span (utils/render-time edit-time)]]
      (for [block group] ;limit to 3 chunks (take 3 group)
        [:div.ragged (render/block-hiccup block block-map)])])])


