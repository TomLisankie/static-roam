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

(defn link-blocks!
  [db-conn]
  (let [;; find every entity id and its content
        entity-id-content-pairs (ds/q
                                 '[:find ?entity-id ?content
                                   :where
                                   [?entity-id :block/content ?content]]
                                 @db-conn)
        page-and-block-references-fn (fn [string]
                                       (let [matches (re-seq #"\[\[.*?\]\]|\(\(.*?\)\)" string)
                                             beginning-parens-with-no-match-removed (map #(if (= "(((" (subs % 0 3)) (subs % 1) %) matches)
                                             page-and-block-names (map utils/remove-double-delimiters beginning-parens-with-no-match-removed)]
                                         page-and-block-names))
        ;; find all references to other blocks in the content `\[\[.*?\]\]`, `\(\(.*?\)\)` (and then if it matches 3 parens in beginning, pop first one off)
        entity-id-reference-pairs (map (fn [pair] [(first pair) (page-and-block-references-fn (second pair))]) entity-id-content-pairs)
        straggling-bracket-instances-removed (map (fn [pair] [(first pair) (filter #(not (str-utils/includes? % "[")) (second pair))]) entity-id-reference-pairs) ;; TODO: Fix so this is unnecessary
        ;; for each of those references, change the `linked-by` property of the referenced block to include the entity ID of the referencing block
        ]
    (pprint/pprint (sort-by first straggling-bracket-instances-removed)))
  )

(defn linked-references
  [block-ds-id conn])

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

(defn mark-blocks-for-inclusion!
  [degree conn]
  (if (and (int? degree) (>= degree 0))
    (degree-explore! 0 degree conn)
    (doseq [block-ds-id (vec (ds/q '[:find ?block-id
                                     :where
                                     [_ :block/id ?block-id]]
                                   @conn))]
      (ds/transact! conn [{:block/id (first block-ds-id)
                           :block/included true}]))))
