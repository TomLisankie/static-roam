(ns static-roam.database
  (:require [static-roam.parser :as parser]
            [static-roam.batadase :as bd]
            [static-roam.utils :as utils]
            [static-roam.config :as config]
            [org.parkerici.multitool.core :as u]
            [taoensso.truss :as truss :refer (have have! have?)]
            ))

;;; Database construction.  See batadase for accessors

(defn- get-block-id
  [block-json]
  (if (:title block-json)
    (:title block-json)
    (:uid block-json)))

(defn- block-properties
  [block-json]
  {:id (get-block-id block-json)       ;TODO this lets everything else be simplified radcially, but haven't gotten aroound to it yet
   :children (map :uid (:children block-json))
   :content (or (:string block-json) (:title block-json))
   :heading (:heading block-json -1)
   :edit-time (when-let [time (or (:edit-time block-json) (:create-time block-json))]
                (java.util.Date. time))
   :page? (contains? block-json :title)
   })

;;; → Multitool
(defn add-parent
  [db children-att parent-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (assoc-in acc [child parent-att] key))
                       acc
                       (children-att item)))
             db
             db))

;;; As above but multivalued
(defn add-parents
  [db children-att parent-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (if (contains? acc child)
                           (update-in acc [child parent-att] conj key)
                           acc))
                       acc
                       (children-att item)))
             db
             db))

(defn- create-block-map-no-links
  "Convert json into basic blocks"
  [roam-json]
  (add-parent
   (u/index-by :id
               (u/walk-collect
                (fn [thing]
                  (when (:uid thing)
                    (block-properties thing)))
                roam-json))
   :children :parent))

(defn block-refs
  [block]
  (letfn [(struct-entities [struct]
            (if (string? struct)
              []
              ;; Would make sense to do some of this in parser/transform-to-ast
              (case (first struct)
                ;; :block-ref – but you don't want to follow those up for inclusion
                :block (mapcat struct-entities (rest struct))
                :hashtag [(utils/format-hashtag (second struct))]
                :page-link [(utils/remove-double-delimiters (second struct))]
                :blockquote (struct-entities (second struct))
                :alias (if-let [v (second (re-find #"\[.*\]\(\[\[(.*)\]\]\)" (second struct)))];kluge alert
                         [v] [])
                [])))]
    (set (struct-entities (:parsed block)))))

;;; New version computes degree as well as acctually the map
;;; Seems to compute the same set as other method
(defn compute-depths
  "Computes depths from entry points"
  [block-map]
  (let [exit-point? (u/memoize-named :exit-point #(bd/exit-point? block-map (get block-map %)))] ;performance hack
    (letfn [(propagate [depth block-map from]
              (let [current-depth (or (get-in block-map [from :depth]) 1000)]
                (if (and (contains? block-map from)
                         (< depth current-depth)
                         (not (exit-point? from)))
                  (reduce (fn [bm r]
                            (propagate (+ depth 1) bm r))
                          (assoc-in block-map [from :depth] depth)
                          (bd/all-refs (get block-map from)))
                  block-map)))]
      (reduce (partial propagate 0) block-map (map :id (bd/entry-points block-map))))))

;;; This is where inclusion is computed.
(defn compute-includes
  [block-map]
  (u/map-values #(assoc % :include? (not (nil? (:depth %)))) block-map))

(defn parse
  [db]
  (u/map-values #(assoc % :parsed (parser/parse-to-ast (:content %))) db))

(defn generate-refs
  [db]
  (u/map-values #(assoc % :refs (block-refs %))
                db))

(defn generate-inverse-refs
  [db]
  (add-parents db :refs :linked-by))    ;I don't like this name, but easier to leave it for now

;;; Trick for memoizing a local recursive fn, see https://quanttype.net/posts/2020-09-20-local-memoized-recursive-functions.html
(defn fix [f] (fn g [& args] (apply f g args)))

(defn add-direct-children
  [block-map]
  (let [direct-children
        (fn [direct-children block]
          (assoc block :dchildren 
                 (map (fn [child-id]
                        (-> child-id
                            block-map
                            (or (prn :not-found child-id)) ;Should't happen if all parts have been downloaded
                            direct-children
                            ))
                      (:children block))))
        direct-children-memoized (fix (u/memoize-named :direct-children direct-children))]
    (u/map-values direct-children-memoized block-map)))

;;; Mostly blocks can be rendered indpendently, but if there are references (and now sidenotes) there are dependencies


(defn roam-db
  [roam-json]
  (-> roam-json
      create-block-map-no-links
      parse
      generate-refs
      generate-inverse-refs
      compute-depths
      compute-includes
      add-direct-children              ;experimental, makes it easier to use, harder to dump. This needs to be last
      ))




