(ns static-roam.database
  (:require [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [static-roam.config :as config]
            [org.parkerici.multitool.core :as u]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.string :as str-utils]))

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
   :edit-time (or (:edit-time block-json) (:create-time block-json)) ;TODO convert to #inst 

;;; Don' think this wants to be done here
;   :entry-point (parser/entry-point? block-json)
;   :exit-point (parser/exit-point? block-json)
   :page? (contains? block-json :title)
   })

#_
(def j (utils/read-roam-json-from-zip "test/resources/static-test.zip"))
#_
(def j (utils/read-roam-json-from-zip  (utils/latest-export)))

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
                         (update-in acc [child parent-att] conj key))
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

;;; Replaced, use the parser
#_
(defn- clean-content-entities
  [string]
  (let [content-entities-found    (utils/find-content-entities-in-string string)
        hashtags-found            (utils/find-hashtags-in-string string) ;TODO needs work; will return #foo in urls
        metadata-found            (utils/find-metadata-in-string string)
        extra-paren-removed       (utils/remove-heading-parens content-entities-found)
        cleaned-content-entities  (map utils/remove-double-delimiters extra-paren-removed)
        cleaned-hashtags          (map utils/remove-leading-char hashtags-found)
        cleaned-metadata          (map utils/remove-double-colon metadata-found)
        all-cleaned-entities      (concat cleaned-content-entities cleaned-hashtags cleaned-metadata)]
    all-cleaned-entities))

;;; TODO "((foobar))"
(defn- cleaner-content-entities
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
    (set (struct-entities (parser/parse-to-ast string)))))

(defn- get-linked-references
  [block-id block-map]
  (get-in block-map [block-id :linked-by]))

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

;;; TODO use this as precondition in more places (and/or easy coerce)
(defn block? [x]
  (and (map? x)
       (string? (:id x))))

(defn block-parent
  [block-map block]
  (and (:parent block)
       (get block-map (:parent block))))

(defn block-children
  [block-map block]
  (map block-map (:children block)))

(defn block-page
  [block-map block]
  {:pre [(have? block? block)]}
  (if-let [parent (block-parent block-map block)]
    (block-page block-map parent)
    block))

(defn pages
  [block-map]
  (filter :page? (vals block-map)))

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
        config/entry-tags))

(def fixed-entry-points #{"SR Metadata"})

(defn entry-points
  [block-map]
  (filter (partial entry-point? block-map) (pages block-map)))

(def daily-log-regex #"(?:January|February|March|April|May|June|July|August|September|October|November|December) \d+.., \d+")

(defn daily-log?
  [block-map block]
  (let [page (block-page block-map block)
        title (or (:title page) (:content page))]
    (when title (re-matches daily-log-regex title))))

(defn exit-point?
  [block-map block]
  {:pre [(have? block? block)]}
  (or (some #(tagged-or-contained? block-map block %)
            config/exit-tags)
      (and config/exclude-daily-logs
           (daily-log? block-map block))))

(defn included-blocks
  [block-map]
  (loop [fringe (map :id (entry-points block-map))
         included #{}
         examined #{}]
    (cond (empty? fringe)
          included
          (contains? examined (first fringe))
          (recur (rest fringe) included examined)
          :else
          (let [current (get block-map (first fringe))]
            (if (exit-point? block-map current)
              (recur (rest fringe) included (conj examined (:id current)))
              (let [refs (all-refs current)
                    new-refs (set/difference refs included)]
                (recur (set/union (rest fringe) new-refs)
                       (conj included (:id current))
                       (conj examined (:id current)))))))))

(defn mark-included-blocks
  [block-map]
  (let [includes (included-blocks block-map)]
    (u/map-values (fn [block]
                    (assoc block :include? (contains? includes (:id block))))
                  block-map)))

(defn add-hiccup-for-included-blocks
  [block-map]
  (u/map-values #(if (:include? %)
                   (assoc % :hiccup (parser/generate-hiccup % block-map))
                   %)
                block-map))

;;; TODO this does a parse but throws away the structure, probably shgould be saved so we don't have to do it again
(defn generate-refs
  [db]
  (u/map-values #(assoc % :refs (cleaner-content-entities (:content %))) db))

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
       mark-included-blocks
       add-direct-children              ;experimental, makes it easier to use, harder to dump. This needs to be last
       ))

;;; Currently unused.

(defn- lkjsafas
  [pair block-map]
  (let [block-id (first pair)
        block-props (second pair)
        block-content (:content block-props)
        block-embed-found (re-find #"\{\{embed: .*?\}\}|\{\{\[\[embed\]\]: .*?\}\}" block-content)]
    (if block-embed-found
      (let [block-ref-uncleaned (re-find #"\(\(.*?\)\)|\[\[.*?\]\]"
                                         (second (str-utils/split block-embed-found #":")))
            referenced-block-id (utils/remove-double-delimiters block-ref-uncleaned)
            referenced-block-props (get block-map referenced-block-id)]
        [block-id
         (assoc block-props :children (into (:children block-props) (reverse (:children referenced-block-props))) :content (:content referenced-block-props))])
      pair)))

(defn- add-children-of-block-embeds
  [block-map]
  (into
   (hash-map)
   (map
    #(lkjsafas % block-map)
    block-map)))

(defn setup-static-roam-block-map
  [roam-json]
  (-> roam-json
      roam-db
      add-hiccup-for-included-blocks)) ;TODO this unmarks pages, too aggressivel


