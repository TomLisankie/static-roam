(ns static-roam.database
  (:require [datascript.core :as ds]
            [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [clojure.string :as str-utils]
            [clojure.pprint :as pprint]))

(defn- get-block-id
  [block-json]
  (if (:title block-json)
    (:title block-json)
    (:uid block-json)))

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

(defn- generate-block-linking-transaction
  [referer-eid reference-id]
  {:block/id reference-id
   :block/linked-by referer-eid})

(defn- generate-block-linking-transactions-for-entity-reference-pair
  [entity-id-reference-pair]
  (let [referer-eid (first entity-id-reference-pair)
        reference-ids (second entity-id-reference-pair)]
    (map #(generate-block-linking-transaction referer-eid %) reference-ids)))

(defn- generate-transactions-for-linking-blocks
  [entity-id-reference-pairs]
  (map generate-block-linking-transactions-for-entity-reference-pair entity-id-reference-pairs))

(defn- link-blocks!
  [db-conn references]
  (let [transactions (generate-transactions-for-linking-blocks references)]
    (ds/transact! db-conn (flatten transactions))))

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

(defn- attach-links-to-block
  [links block]
  (let [block-id (first block)
        block-props (second block)]
    [block-id (assoc block-props :linked-by (set (get links block-id '())))]))

(defn- attach-links-to-block-map
  [links block-map]
  (map #(attach-links-to-block links %) block-map))

(defn- generate-linked-references
  [block-map-no-links]
  (let [block-id-reference-pairs (get-block-id-reference-pairs block-map-no-links)
        broken-references-removed (map filter-broken-references block-id-reference-pairs)
        links (generate-links broken-references-removed block-map-no-links)
        block-map-with-links (attach-links-to-block-map links block-map-no-links)]
    block-map-with-links))

(def example
  (generate-linked-references {"the [[skill level]] of each user grows over time"
                             {:children '(),
                              :content "the [[skill level]] of each user grows over time",
                              :heading -1,
                              :text-align "",
                              :entry-point false,
                              :page true},
                             "skill level"
                             {:children '("MYOLydOF1" "5r3u0iI4O"),
                              :content "skill level",
                              :heading -1,
                              :text-align "",
                              :entry-point false,
                              :page true},
                             "MYOLydOF1"
                             {:children '(),
                              :content
                              "There are [[[[individual difference]]s between people in prior [[skill level]]]] before they open up an app",
                              :heading -1,
                              :text-align "",
                              :entry-point false,
                              :page false},
                             "5r3u0iI4O"
                             {:children '(),
                              :content "[[Problem Text]] [[skill level]]",
                              :heading -1,
                              :text-align "",
                              :entry-point false,
                              :page false},
                             "Problem Text"
                             {:children '(),
                              :content "Problem Text",
                              :heading -1,
                              :text-align "",
                              :entry-point false,
                              :page true}}))

(pprint/pprint example)

(defn linked-references
  [block-id block-map]
  (let [block-props (get block-map block-id)
        linked-refs (:linked-by block-props)]
    linked-refs))

(defn degree-explore!
  [current-level max-level conn]
  (if (= current-level 0)
    (let [entry-points (map first (vec (ds/q '[:find ?entry-point-id
                                               :where
                                               [?id :block/entry-point true]
                                               [?id :block/id ?entry-point-id]]
                                             @conn)))]
      (doseq [block-id entry-points]
        (ds/transact! conn [{:block/id block-id
                             :block/included true}]))
      (doseq [children (map :block/children (map #(ds/entity @conn [:block/id %]) entry-points))]
        ;; now for each of these sequences I gotta mark them for inclusion and then explore them
        (doseq [child children]
          (ds/transact! [{:block/id (first child)
                          :block/included true}]))))
    (if (>= max-level current-level)
      nil
      nil)))

(defn mark-content-entities-for-inclusion
  [degree block-map]
  (if (and (int? degree) (>= degree 0))
    (degree-explore! 0 degree conn)
    (doseq [block-ds-id (vec (ds/q '[:find ?block-id
                                     :where
                                     [_ :block/id ?block-id]]
                                   @conn))]
      (ds/transact! conn [{:block/id (first block-ds-id)
                           :block/included true}]))))

(defn generate-hiccup
  [conn]
  (let [id+content (ds/q '[:find ?id ?content
                           :where [?id :block/included true]
                           [?id :block/content ?content]]
                         @conn)
        transactions (for [[id content] id+content]
                       [:db/add id :block/hiccup (parser/block-content->hiccup id content conn)])]
    (ds/transact! conn transactions)))

(defn replicate-roam-db
  [roam-json db-conn]
  (let [block-map-no-links (create-block-map-no-links roam-json)
        block-map-with-linked-references (generate-linked-references block-map-no-links)]
    block-map-with-linked-references))

(defn setup-static-roam-block-map
  [roam-json degree]
  (let [replicated-roam-block-map (replicate-roam-db roam-json)
        blocks-tagged-for-inclusion (mark-content-entities-for-inclusion degree replicated-roam-block-map)
        hiccup-for-included-blocks (generate-hiccup-for-included-blocks blocks-tagged-for-inclusion)]
    hiccup-for-included-blocks))
