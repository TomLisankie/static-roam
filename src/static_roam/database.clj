(ns static-roam.database
  (:require [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [org.parkerici.multitool.core :as u]
            [clojure.walk :as walk]
            [clojure.set :as set]
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
   :entry-point (parser/entry-point? block-json)
   :exit-point (parser/exit-point? block-json)
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
  "Conver json into basic bloccks"
  [roam-json]
  (add-parent
   (u/index-by :id
               (u/walk-collect
                (fn [thing]
                  (when (and (map? thing)
                             (:create-time thing))
                    (block-properties thing)))
                roam-json))
   :children :parent))

;;; Why isn't this using parser? Argh I hate this code
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

;;; For testing new entity extracction
#_
(def x
  (remove (fn [[_ old new]]
            (= (disj (set old) "TODO" "DONE") new))
          (remove (comp empty? second)
                  (map (comp (juxt identity clean-content-entities cleaner-content-entities) :content)
                       (vals bm0)))))
;; sometimes people put references to other page titles inside of the title of another page. So pairs of brackets inside of other brackets when they reference them. This breaks things currently, so I'm removing all those instances here TODO: Fix so this is unnecessary
(defn- filter-broken-references
  [pair]
  [(first pair)
   (filter
    #(not (str-utils/includes? % "["))
    (second pair))])


(defn get-linked-references
  [block-id block-map]
  (get-in block-map [block-id :linked-by]))

(def fixed-entry-points #{"SR Metadata"})

(defn- get-entry-point-ids
  [block-map]
  (set/union (set (filter identity (map (fn [[k v]] (when (:entry-point v) k)) block-map)))
             fixed-entry-points))
                  

(defn- get-children-of-block
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (remove #(get-in block-map [% :exit-point]) (:children block-props))))

(defn- get-all-children-of-blocks
  [block-ids block-map]
  (reduce into #{} (map #(get-children-of-block % block-map) block-ids)))

(defn- get-references-for-block
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (:refers-to block-props)))

(defn- get-children-recursively
  [entity-id block-map]
  (let [children (set (get-children-of-block entity-id block-map))]
    (if (empty? (flatten (map #(get-children-of-block % block-map) children)))
      children
      (reduce into children (flatten (map #(get-children-recursively % block-map) children))))))

(defn- get-all-children-recursively
  [examining block-map]
  (map #(get-children-recursively % block-map) examining))

(defn- aggregate-children
  [children-sets]
  (reduce into #{} children-sets))

(defn- get-child-content-references
  [children block-map]
  (map #(get-references-for-block % block-map) children))

(defn- aggregate-references
  [reference-sets]
  (reduce into #{} reference-sets))

(defn- generate-included-entities
  [included-entities children references]
  (reduce into included-entities [children references]))

;;; TODO included-entities
(defn all-refs [block]
  (set/union
   (set (:children block))
   (set (:refs block))
   (set (:linked-by block))))

;;; TODO argh this needs to be condensed; could probably use walker.
;;; Or maybe not, loop is for references, children are handled by get-all-children-recursively
;;; We onlly care about pages anyway, right? And fuck degree
(defn- included-blocks
  [block-map]
  (loop [fringe (get-entry-point-ids block-map)
         included (get-entry-point-ids block-map)]
    (if (empty? fringe)
      included
      (let [current (get block-map (first fringe))]
        (if (:exit-point current)
          (recur (rest fringe)
                 included)
          (let [refs (all-refs current)
                new-refs (set/difference refs included)]
            (recur (set/union (rest fringe) new-refs)
                   (conj included (:id current)))))))))


(defn mark-included-blocks
  [block-map]
  (let [includes (included-blocks block-map)]
    (u/map-values (fn [block]
                    (assoc block :include? (contains? includes (:id block))))
                  block-map)))


#_
(defn- generate-hiccup-if-block-is-included
  [block-kv block-map]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    [block-id
     (when (:included block-props)
       (assoc block-props
              :hiccup
              (let [the-hiccup (parser/block-content->hiccup (:content block-props) block-map)]
                (if (> (:heading block-props) 0)
                  (give-header the-hiccup block-props)
                  the-hiccup)))
       block-props)]))

#_
(defn generate-hiccup-for-included-blocks
  [block-map]
  (into (hash-map)
        (filter #(not= nil (:content (second %)))
                (map #(generate-hiccup-if-block-is-included % block-map)
                     block-map))))

(defn generate-hiccup
  [block block-map]
  (let [basic (parser/block-content->hiccup (:content block) block-map)]
    (if (> (:heading block) 0)
      [(keyword (str "h" (:heading block))) basic]
      basic)))

(defn add-hiccup-for-included-blocks
  [block-map]
  (u/map-values #(if (:included? %)
                   (assoc % :hiccup (generate-hiccup % block-map))
                   %)
                block-map))


;;; TODO this does a parse but throws away the structure, probably shgould be saved so we don't have to do it again
(defn generate-refs
  [db]
  (u/map-values #(assoc % :refs (cleaner-content-entities (:content %))) db))

(defn generate-inverse-refs
  [db]
  (add-parents db :refs :linked-by))    ;I don't like this name, but easier to leave it for now

(defn roam-db
  [roam-json]
  (->> roam-json
       create-block-map-no-links
       generate-refs
       generate-inverse-refs))

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
      mark-included-blocks
#_      mark-content-entities-for-inclusion
#_      add-children-of-block-embeds
      add-hiccup-for-included-blocks)) ;TODO this unmarks pages, too aggressivel

;;; Stolen from incidents/ocr.
;;; Maybe use this early on in processing. Although here I think the hypertextishness means you can't just pass around a block tree. 
(defn direct
  "Turns a set of json blocks into a set of trees, with PAGEs as top elements and sub-blocks on the :children attribute"
  [block-map block-name]
  (letfn [(direct-children [b]
              (-> b
                  (assoc 
                   :dchildren
                   (map (fn [child-id]
                          (-> child-id
                              block-map
                              (or (prn :not-found child-id)) ;Should't happen if all parts have been downloaded
                              direct-children
                              ))
                        (:children b)))
                  ))]
    (direct-children (get block-map block-name))))
