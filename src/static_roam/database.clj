(ns static-roam.database
  (:require [datascript.core :as ds]
            [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [clojure.string :as str-utils]
            [clojure.pprint :as pprint]))

(defn populate-db!
  "Populate database with relevant properties of pages and blocks"
  [roam-json db-conn]
  (doseq [block roam-json]
    (ds/transact! db-conn [{:block/id (if (:title block)
                                        (:title block)
                                        (:uid block))
                            :block/children (map :uid (:children block))
                            :block/content (:string block (:title block))
                            :block/heading (:heading block -1)
                            :block/text-align (:text-align block "")
                            :block/entry-point (parser/entry-point? block)
                            :block/page (if (:title block)
                                          true
                                          false)
                            :block/linked-by []}])
    (populate-db! (:children block) db-conn)))

(defn- get-entity-id-content-pairs
  [db-conn]
  (ds/q
   '[:find ?entity-id ?content
     :where
     [?entity-id :block/content ?content]]
   @db-conn))

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

(defn- clean-pair
  [pair]
  [(first pair)
   (clean-content-entities (second pair))])

(defn- block-id-to-entity-id
  [block-id conn]
  (ds/q
   '[:find ?entity-id
     :in $ ?block-id
     :where
     [?entity-id :block/id ?block-id]]
   @conn))

(defn- block-ids-to-entity-ids
  [block-ids conn]
  (map (block-id-to-entity-id % conn) block-ids))

(defn- convert-block-ids-to-entity-ids
  [reference-pairs conn]
  (map (fn [pair] [(first pair) (block-ids-to-entity-ids (second pair))]) reference-pairs))

(defn- generate-block-linking-transaction
  [referer-eid reference-eid]
  {:db/id reference-eid
   :block/linked-by referer-eid})

(defn- generate-block-linking-transactions-for-entity-reference-pair
  [entity-id-reference-pair]
  (let [referer-eid (first entity-id-reference-pair)
        reference-eids (second entity-id-reference-pair)]
    (map #(generate-block-linking-transaction referer-eid %) reference-eids)))

(defn- generate-transactions-for-linking-blocks
  [entity-id-reference-pairs]
  (let [entity-id-reference-entity-pairs (convert-block-ids-to-entity-ids entity-id-reference-pairs db-conn)]
    (map generate-block-linking-transactions-for-entity-reference-pair entity-id-reference-entity-pairs)))

(defn- link-blocks!
  [db-conn references]
  (let [transactions (generate-transactions-for-linking-blocks references db-conn)]
    (ds/transact! db-conn transactions)))

(defn generate-linked-references!
  [db-conn]
  (let
    [entity-id-and-reference-ids (get-entity-id-content-pairs db-conn)
     cleaned-references (map clean-pair entity-id-and-reference-ids)
     broken-references-removed (map filter-broken-references cleaned-references)]
    (link-blocks! db-conn broken-references-removed)))

(defn linked-references
  [block-ds-id conn]
  ;; TODO: implement
  )

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

(defn mark-content-entities-for-inclusion!
  [degree conn]
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

(defn replicate-roam-db!
  [roam-json db-conn]
  (populate-db! roam-json db-conn)
  (generate-linked-references! db-conn))

(defn setup-static-roam-db
  [roam-json degree]
  (let [schema {:block/id       {:db/unique :db.unique/identity}
                :block/children {:db/cardinality :db.cardinality/many}
                :block/linked-by {:db/cardinality :db.cardinality/many
                                  :db/valueType :db.type/ref}}
        db-conn (ds/create-conn schema)]
    (replicate-roam-db! roam-json db-conn)
    (mark-content-entities-for-inclusion! degree db-conn)
    (generate-hiccup db-conn)
    db-conn))
