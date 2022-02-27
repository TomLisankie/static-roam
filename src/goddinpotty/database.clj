(ns goddinpotty.database
  (:require [goddinpotty.parser :as parser]
            [goddinpotty.batadase :as bd]
            [goddinpotty.utils :as utils]
            [goddinpotty.config :as config]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.walk :as walk]
            [taoensso.truss :as truss :refer (have have! have?)]
            ))

;;; Database construction.  See batadase for accessors

(defn- get-block-id
  [block-json]
  (or (:title block-json)
      (:uid block-json)))

(defn- block-properties
  [block-json]
  {:id (get-block-id block-json)       ;TODO note: having this in the map can simplify some stuff
   :children (map :uid (:children block-json))
   :content (or (:string block-json) (:title block-json))
   :heading (:heading block-json -1)
   :edit-time (when-let [time (:edit-time block-json)]
                (java.util.Date. time))
   :create-time (when-let [time (:create-time block-json)]
                  (java.util.Date. time))
   :page? (contains? block-json :title)
   })

(defn- create-basic-blocks
  [roam-json]
  (u/walk-collect
   (fn [thing]
     (when (and (get-block-id thing)
                (> (count thing) 1)) ;try to exclude :refs
       (block-properties thing)))
   roam-json))

(defn index-blocks
  [basic-blocks]
  ;; Awkward but works
  (u/self-label                         ;Add ids for newly created blocks
   :id
   (u/add-inverse
    (u/index-by :id basic-blocks)
    :children :parent
    )))

;;; Deal with Logseq page hierarchy feature. This gives pages with titles
;;; like [[Religion/Discordianism]] a link to their parent [[Religion]], for inclusion purposes.

;;; One consequence of this is that if any page of such a hierarchy is published, they all will be,
;;; unless explicitly marked #Private
(defn page-hierarchy-ref
  [page]
  (let [[_ parent _local]
        (and (:title page)           ;temp
             (re-find #"^(.*)/(.*?)$" (:title page)))]
    parent))

(defn block-refs
  [block]
  (letfn [(struct-entities [struct]
            (if (string? struct)
              []
              ;; Would make sense to do some of this in parser/transform-to-ast
              (case (first struct)
                ;; :block-ref – but you don't want to follow those up for inclusion
                :block (mapcat struct-entities (rest struct))
                :hashtag [(utils/parse-hashtag (second struct))]
                :page-link [(utils/remove-double-delimiters (second struct))]
                :blockquote (struct-entities (second struct))
                :alias (if-let [v (second (re-find #"\[.*\]\(\[\[(.*)\]\]\)" (second struct)))];kluge alert
                         [v] [])
                ;; default
                (mapcat struct-entities (rest struct)))))]
    (let [base (set (struct-entities (:parsed block)))]
      (if-let [page-hierarch-ref (page-hierarchy-ref block)]
        (conj base page-hierarch-ref)
        base))))

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

(defn parse-block
  [block]
  (assoc block :parsed (parser/parse-to-ast (:content block)))  )

(defn parse
  [db]
  (ju/pmap-values parse-block db))

(defn generate-refs
  [db]
  (ju/pmap-values #(assoc % :refs (block-refs %))
                  db))

(defn generate-inverse-refs
  [db]
  (u/self-label :id
                (u/add-inverse-multiple db :refs :linked-by)))    ;I don't like this name, but easier to leave it for now

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
    (ju/pmap-values direct-children-memoized block-map)))

;;; Mostly blocks can be rendered indpendently, but if there are references (and now sidenotes) there are dependencies


(defn roam-db-1
  [db]
  (-> db
      parse
      generate-refs
      generate-inverse-refs
      compute-depths
      compute-includes
      add-direct-children))              ; makes it easier to use, harder to dump. This needs to be last

(defn add-uids
  [json]
  (walk/postwalk #(if (and (map? %) (not (contains? % :uid)))
                    (assoc % :uid (name (gensym "bg" )))
                    %)
                 json))

(defn roam-db
  [roam-json]
  (-> roam-json
      add-uids                          ;for logseq export
      create-basic-blocks
      index-blocks
      roam-db-1
      ))






      
