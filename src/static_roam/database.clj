(ns static-roam.database
  (:require [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [clojure.string :as str-utils]
            [clojure.pprint :as pprint]))

(defn- get-block-id
  [block-json]
  (if (:title block-json)
    (:title block-json)
    (:uid block-json)))

(defn get-properties-for-block-id
  [block-id block-map]
  (get block-map block-id))

(defn- get-block-properties
  [block-json]
  {
   :children (map :uid (:children block-json))
   :content (:string block-json (:title block-json))
   :heading (:heading block-json -1)
   :text-align (:text-align block-json "")
   :entry-point (parser/entry-point? block-json)
   :page (if (:title block-json)
           true
           false)
   })

(defn- generate-block-linking-transaction
  [referer-eid reference-id]
  {:block/id reference-id
   :block/linked-by referer-eid})

(defn- create-id-properties-pair
  [block-json]
  [(get-block-id block-json) (get-block-properties block-json)])

(defn- create-id-properties-pairs
  [roam-json]
  (map
   vec
   (partition
    2
    (flatten
     (for [block-json roam-json]
       (conj (create-id-properties-pairs (:children block-json)) (create-id-properties-pair block-json)))))))

(defn- create-block-map-no-links
  "Populate database with relevant properties of pages and blocks"
  [roam-json]
  ;; what I need to happen here is have it so if the block has children, it creates pairs for those as well
  ;; forget what I already implemented, I optimized too early.
  (into (hash-map) (create-id-properties-pairs roam-json)))

(defn- clean-content-entities
  [string]
  (let [content-entities-found
        (utils/find-content-entities-in-string string)
        extra-paren-removed
        (utils/remove-heading-parens content-entities-found)
        cleaned-content-entities
        (map utils/remove-double-delimiters extra-paren-removed)]
    cleaned-content-entities))

;; sometimes people put references to other page titles inside of the title of another page. So pairs of brackets inside of other brackets when they reference them. This breaks things currently, so I'm removing all those instances here TODO: Fix so this is unnecessary
(defn- filter-broken-references
  [pair]
  [(first pair)
   (filter
    #(not (str-utils/includes? % "["))
    (second pair))])

(defn- generate-block-id-reference-pair
  [pair]
  [(first pair)
   (clean-content-entities (second pair))])

(defn- generate-block-linking-transactions-for-entity-reference-pair
  [entity-id-reference-pair]
  (let [referer-eid (first entity-id-reference-pair)
        reference-ids (second entity-id-reference-pair)]
    (map #(generate-block-linking-transaction referer-eid %) reference-ids)))

(defn- generate-transactions-for-linking-blocks
  [entity-id-reference-pairs]
  (map generate-block-linking-transactions-for-entity-reference-pair entity-id-reference-pairs))

(defn- get-block-id-content-pair
  [pair]
  [(first pair) (:content (second pair))])

(defn- get-block-id-content-pairs
  [block-map]
  (map get-block-id-content-pair block-map))

(defn- add-linked-by-property
  [pair]
  [(first pair) (assoc (second pair) :linked-by '())])

(defn- get-block-id-reference-pairs
  [block-map]
  (let [block-map-with-linked-by (map add-linked-by-property block-map)
        block-id-content-pairs (get-block-id-content-pairs block-map)
        block-id-reference-pairs (map generate-block-id-reference-pair block-id-content-pairs)]
    block-id-reference-pairs))

(defn- get-referenced-referer-pair
  [referenced referer]
  [referenced referer])

(defn- get-referenced-referer-pairs
  [referer-referenced-pairs]
  (let [referer (first referer-referenced-pairs)
        referenced (second referer-referenced-pairs)]
    (map #(get-referenced-referer-pair % referer) referenced)))

(defn- generate-links
  [block-id-reference-pairs block-map-no-links]
  (let [individual-referenced-referer-pairs (partition
                                             2
                                             (flatten
                                              (map get-referenced-referer-pairs block-id-reference-pairs)))
        grouped-by-referenced (group-by first individual-referenced-referer-pairs)
        reference-names-stripped (map (fn [kv] [(first kv) (map second (second kv))]) grouped-by-referenced)
        ]
    (into (hash-map) reference-names-stripped)))

(defn- attach-backlinks-to-block
  [id-backlinks-map block]
  (let [block-id (first block)
        block-props (second block)]
    [block-id (assoc block-props :linked-by (set (get id-backlinks-map block-id '())))]))

(defn- attach-backlinks-to-block-map
  [links block-map]
  (map #(attach-backlinks-to-block links %) block-map))

(defn- attach-links-to-block
  [id-reference-map block-kv]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    [block-id (assoc block-props :refers-to (set (get id-reference-map block-id '())))]))

(defn- attach-links-to-block-map
  [block-id-reference-pairs block-map-no-links]
  (let [id-reference-map (into (hash-map) block-id-reference-pairs)]
    (into (hash-map) (map #(attach-links-to-block id-reference-map %) block-map-no-links))))

(defn- generate-links-and-backlinks
  [block-map-no-links]
  (let [block-id-reference-pairs (get-block-id-reference-pairs block-map-no-links)
        broken-references-removed (map filter-broken-references block-id-reference-pairs)
        block-map-with-links (attach-links-to-block-map broken-references-removed block-map-no-links)
        backlinks (generate-links broken-references-removed block-map-no-links)
        block-map-with-backlinks (attach-backlinks-to-block-map backlinks block-map-with-links)]
    block-map-with-backlinks))

(defn get-linked-references
  [block-id block-map]
  (let [block-props (get block-map block-id "BLOCK DOESN'T EXIST")
        linked-refs (:linked-by block-props)]
    linked-refs))

(defn- mark-block-as-included
  [block-kv]
  [(first block-kv) (assoc (second block-kv) :included true)])

(defn- entry-point?
  [block-kv]
  (true? (:entry-point (second block-kv))))

(defn- get-entry-point-ids
  [block-map]
  (map first (filter entry-point? block-map)))

(defn- get-children
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (:children block-props)))

(defn- merge-children
  [children block-map]
  (flatten (map #(get-children % block-map) children)))

(defn- include-children
  [children included current-degree max-degree block-map]
  (if (> current-degree max-degree)
    (into included children)
    (include-children
     (merge-children children block-map)
     (into included children)
     (inc current-degree)
     max-degree
     block-map)))

(defn- get-content-entity-ids-to-include
  [degree block-map]
  (let [entry-point-ids (get-entry-point-ids block-map)
        included-block-ids (into #{} entry-point-ids)
        children-to-include (include-children
                             (merge-children entry-point-ids block-map)
                             #{}
                             0
                             degree
                             block-map)
        blocks-referenced-by-children ()]
    (into included-block-ids
          children-to-include)))

(def example
  {"the [[skill level]] of each user grows over time"
   {:children '(),
    :content "the [[skill level]] of each user grows over time",
    :heading -1,
    :text-align "",
    :entry-point false,
    :page true,
    :linked-by #{}},
   "skill level"
   {:children '("MYOLydOF1" "5r3u0iI4O"),
    :content "skill level",
    :heading -1,
    :text-align "",
    :entry-point true,
    :page true,
    :linked-by
    #{"the [[skill level]] of each user grows over time" "5r3u0iI4O"
      "MYOLydOF1"}},
   "MYOLydOF1"
   {:children '(),
    :content
    "There are [[[[individual difference]]s between people in prior [[skill level]]]] before they open up an app",
    :heading -1,
    :text-align "",
    :entry-point false,
    :page false,
    :linked-by #{}},
   "5r3u0iI4O"
   {:children '("98234nasdok" "0-kjf3jf"),
    :content "[[Problem Text]] [[skill level]]",
    :heading -1,
    :text-align "",
    :entry-point false,
    :page false,
    :linked-by #{}},
   "98234nasdok"
   {
    :children '("eeeq-87234refdu")
    :content "uwerqehjvakdfsakds"
    :heading -1,
    :text-align "",
    :entry-point false,
    :page false,
    :linked-by #{}
    },
   "eeeq-87234refdu"
   {
    :children '()
    :content "[[Just Some Page]]"
    :heading -1,
    :text-align "",
    :entry-point false,
    :page false,
    :linked-by #{}
    },
   "0-kjf3jf"
   {
    :children '()
    :content "fhjdkfqweiurqwe"
    :heading -1,
    :text-align "",
    :entry-point false,
    :page false,
    :linked-by #{}
    },
   "Problem Text"
   {:children '(),
    :content "Problem Text",
    :heading -1,
    :text-align "",
    :entry-point false,
    :page true,
    :linked-by #{"5r3u0iI4O"}}
   "Just Some Page"
   {:children '(),
    :content "Just Some Page",
    :heading -1,
    :text-align "",
    :entry-point false,
    :page true,
    :linked-by #{"eeeq-87234refdu"}}})

(get-content-entity-ids-to-include 2 example)

(pprint/pprint (generate-links-and-backlinks example))

(defn- block-id-included?
  [block-id included-block-ids]
  (contains? included-block-ids block-id))

(defn- mark-block-if-included
  [included-block-ids block-kv]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    (if (block-id-included? block-id included-block-ids)
      [block-id (assoc block-props :included true)]
      block-kv)))

(defn- mark-blocks-to-include-as-included
  [included-block-ids block-map]
  (map #(mark-block-if-included included-block-ids %) block-map))

(defn- mark-content-entities-for-inclusion
  [degree block-map]
  (if (and (int? degree) (>= degree 0))
    (into (hash-map) (mark-blocks-to-include-as-included
                      (get-content-entity-ids-to-include degree block-map)
                      block-map))
    (into (hash-map) (map mark-block-as-included block-map))))

(defn- generate-hiccup-if-block-is-included
  [block-kv block-map]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    [block-id
     (if (true? (:included block-props))
       (assoc block-props :hiccup (parser/block-content->hiccup (:content block-props) block-map))
       block-props)]))

(defn generate-hiccup-for-included-blocks
  [block-map]
  (into (hash-map) (map #(generate-hiccup-if-block-is-included % block-map) block-map)))

(defn replicate-roam-db
  [roam-json]
  (let [block-map-no-links (create-block-map-no-links roam-json)
        block-map-linked-by-and-refers-to (generate-links-and-backlinks block-map-no-links)]
    block-map-linked-by-and-refers-to))

(defn setup-static-roam-block-map
  [roam-json degree]
  (let [replicated-roam-block-map (replicate-roam-db roam-json)
        blocks-tagged-for-inclusion (mark-content-entities-for-inclusion degree replicated-roam-block-map)
        hiccup-for-included-blocks (generate-hiccup-for-included-blocks blocks-tagged-for-inclusion)]
    hiccup-for-included-blocks))
