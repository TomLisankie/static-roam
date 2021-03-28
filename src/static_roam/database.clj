(ns static-roam.database
  (:require [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [static-roam.config :as config]
            [org.parkerici.multitool.core :as u]
            [clojure.set :as set]
            [taoensso.truss :as truss :refer (have have! have?)]
            ))

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
                  (when (and (map? thing)
                             (:edit-time thing))
                    (block-properties thing)))
                roam-json))
   :children :parent))

;;; TODO "((foobar))"
(defn- content-refs-0
  [string]
  (letfn [(struct-entities [struct] 
            (if (string? struct)
              []
              (case (first struct)
                :block (mapcat struct-entities (rest struct))
                :hashtag [(utils/format-hashtag (second struct))]
                :page-link [(utils/remove-double-delimiters (second struct))]
                :blockquote (struct-entities (second struct))
                [])))]
    (struct-entities (parser/parse-to-ast string))))

(defn content-refs
  [string]
  (set (and string
            (content-refs-0 string))))

(defn- get-linked-references
  [block-id block-map]
  (filter #(get-in block-map [% :id])      ;trying to filter out bogus entries, not working
          (get-in block-map [block-id :linked-by])))

(defn get-included-linked-references
  [block-id block-map]
  (filter #(get-in block-map [% :include?])
          (get-linked-references block-id block-map)))

;;; TODO included-entities
(defn all-refs [block]
  (set/union
   (set (:children block))
   (set (:refs block))
   (set (:linked-by block))
   (set (and (:parent block) (list (:parent block))))))

;;; Some new accessors

(def descendents
  (u/transitive-closure :dchildren))

(defn block-parent
  [block-map block]
  (and (:parent block)
       (get block-map (:parent block))))

(defn block-children
  [block-map block]
  (map block-map (:children block)))

(defn block-descendents
  [block]
  ((u/transitive-closure :dchildren) block))

(defn block-contains?
  [b1 b2]
  (contains? (set (map :id (descendents b1))) (:id b2)))

(defn forward-page-refs
  "Forward page refs. Returns set of ids"
  [page]
  (apply clojure.set/union
         ;; TODO include? should be a parameter
         (map :refs (filter :include? (block-descendents page)))))

(defn block? [x]
  (and (map? x)
       (string? (:id x))))

(defn block-page
  [block-map block]
  {:pre [(have? block? block)]}
  (if-let [parent (block-parent block-map block)]
    (block-page block-map parent)
    block))

(defn backward-page-refs
  [bm page]
  (map :content
       (filter :include?
               (map (comp (partial block-page bm) bm)
                    (:linked-by page)))))

(defn page-refs
  [bm page]
  (set/union (forward-page-refs page)
             (backward-page-refs bm page)))

(defn- pages
  [block-map]
  (filter :page? (vals block-map)))

(defn included-pages
  [block-map]
  (filter :include? (pages block-map)))

(defn displayed-pages
  [block-map]
  (if (config/config :unexclude?)
    (pages block-map)
    (included-pages block-map)))

(defn displayed-regular-pages
  [block-map]
  (remove :special? (displayed-pages block-map)))

(defn tagged?
  [block-map block tag]
  (or (contains? (:refs block) tag)
      ;; This implements the somewhat weird convention that tags are done in contained elts, eg
      ;; - Some private stuff
      ;;   - #Private
      ;; partly for historical reasons and partly so pages can be tagged
      (some #(contains? (:refs %) tag)
            (block-children block-map block))))

(defn tagged-or-contained?
  [block-map block tag]
  (and block
       (or (tagged? block-map block tag)
           (tagged-or-contained? block-map (block-parent block-map block) tag))))

(defn entry-point?
  "Determines whether or not a given page is tagged with #EntryPoint in its first child block"
  [block-map block]
  (some #(tagged? block-map block %)
        (config/config :entry-tags)))

(def fixed-entry-points #{"SR Metadata"})

(defn entry-points
  [block-map]
  (filter (partial entry-point? block-map) (pages block-map)))

(def daily-notes-regex #"(?:January|February|March|April|May|June|July|August|September|October|November|December) \d+.., \d+")

(defn daily-notes?
  [block-map block]
  (let [page (block-page block-map block)
        title (or (:title page) (:content page))]
    (when title (re-matches daily-notes-regex title))))

(defn exit-point?
  [block-map block]
  (or (some #(tagged-or-contained? block-map block %)
            (config/config :exit-tags))
      (and (not (config/config :daily-notes?))
           (daily-notes? block-map block))))

;;; New version computes degree as well as acctually the map
;;; Seems to compute the same set as other method
(defn compute-depths
  "Computes depths from entry points"
  [block-map]
  (let [exit-point? (memoize #(exit-point? block-map (get block-map %)))] ;performance hack
    (letfn [(propagate [depth block-map from]
              (let [current-depth (or (get-in block-map [from :depth]) 1000)]
                (if (and (contains? block-map from)
                         (< depth current-depth)
                         (not (exit-point? from)))
                  (reduce (fn [bm r]
                            (propagate (+ depth 1) bm r))
                          (assoc-in block-map [from :depth] depth)
                          (all-refs (get block-map from)))
                  block-map)))]
      (reduce (partial propagate 0) block-map (map :id (entry-points block-map))))))

(defn compute-includes
  [block-map]
  (u/map-values #(assoc % :include? (not (nil? (:depth %)))) block-map))

;;; TODO not the way to do this
(defn add-hiccup-for-included-blocks
  [block-map]
  (u/map-values #(if (or (:include? %) (config/config :unexclude?))
                   (assoc % :hiccup (parser/generate-hiccup % block-map))
                   %)
                block-map))

;;; TODO this does a parse but throws away the structure, probably shgould be saved so we don't have to do it again
(defn generate-refs
  [db]
  (u/map-values #(assoc % :refs (content-refs (:content %))) db))

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
        direct-children-memoized (fix (memoize direct-children))]
    (u/map-values direct-children-memoized block-map)))

(defn roam-db
  [roam-json]
  (->> roam-json
       create-block-map-no-links
       generate-refs
       generate-inverse-refs
       compute-depths
       compute-includes
       add-direct-children              ;experimental, makes it easier to use, harder to dump. This needs to be last
       ))

(defn setup-block-map
  [roam-json]
  (-> roam-json
      roam-db
      add-hiccup-for-included-blocks)) ;TODO this unmarks pages, too aggressivel

;;; Temp
(def min* (partial u/min-by identity))
(def max* (partial u/max-by identity))

;;; Prob needs to deal with missing data
;;; Also, to be proper, :create-time should be used as well
;;; I suppose the median time might be more informative – or an Ed Tufte minigraph
(defn date-range [page]
  (let [blocks (block-descendents page)
        visible-blocks (if (config/config :unexclude?)
                          blocks
                          (filter :include? blocks))
        visible-dates (map :edit-time visible-blocks)]
    [(min* visible-dates) (max* visible-dates)]))

(defn stats [bm]
  {:blocks {:total (count bm)
            :published (count (filter :include? (vals bm)))}
   :pages {:total (count (pages bm))
           :published (count (filter :include? (pages bm)))}})


;;; These could be part of the map but it's easier this way

(def edit-time
  (memoize
   (fn 
     [page]
     (second (date-range page)))))

(def size
  (memoize
   (fn [page]
     (reduce +
             (count (or (:content page) 0))
             (map size
                  (filter :include? (:dchildren page)))))))

(defn page-empty?
  [page]
  (< (- (size page)
        (count (:id page)))
        10))


(defn expand-to [block-map block minsize]
  (cond (>= (size block) minsize)
        block
        (nil? (:parent block))
        block
        :else
        (expand-to block-map (get block-map (:parent block)) minsize)))


(defn leaf?
  [block]
  (empty? (:chidlren block)))
