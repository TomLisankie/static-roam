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
  (let
    [entity-id-and-content-entities ; "Content entities" are a good term to encapsulate both blocks and pages
     (ds/q
      '[:find ?entity-id ?content
        :where
        [?entity-id :block/content ?content]]
      @db-conn)

     find-content-entities-in-string
     (fn [string]
       (re-seq #"\[\[.*?\]\]|\(\(.*?\)\)" string))

     remove-heading-parens
     (fn [strings]
       (map
        #(if (= "(((" (subs % 0 3))
           (subs % 1)
           %)
        strings))

     clean-content-entities
     (fn [string]
       (let [content-entities-found
             (find-content-entities string)
             extra-paren-removed
             (remove-heading-params content-entities-found)
             cleaned-content-entities
             (map utils/remove-double-delimiters extra-paren-removed)]
         page-and-block-names))

     clean-pair
     (fn
       [pair]
       [(first pair)
        (clean-content-entities (second pair))])

     cleaned-references
     (map
      clean-pair
      entity-id-and-content-entities)

     filter-broken-references
     (fn
       [pair]
       [(first pair)
        (filter
         #(not (str-utils/includes? % "["))
         (second pair))])

     broken-references-removed
     ;; sometimes people put references to other page titles inside of the title of another page. So pairs of brackets inside of other brackets when they reference them. This breaks things currently, so I'm removing all those instances here TODO: Fix so this is unnecessary
     (map
      filter-broken-references
      cleaned-references)

     ]
    (pprint/pprint
     (ds/q
      '[:find ?linked-by
        :where
        [?id :block/linked-by ?linked-by]]
      @db-conn))
    (map transact-links broken-references-removed)
    ;; (pprint/pprint (sort-by first broken-references-removed))
    )
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

(defn generate-hiccup
  [conn]
  (let [id+content (ds/q '[:find ?id ?content
                           :where [?id :block/included true]
                           [?id :block/content ?content]]
                         @conn)
        transactions (for [[id content] id+content]
                       [:db/add id :block/hiccup (parser/block-content->hiccup id content conn)])]
    (ds/transact! conn transactions)))

(defn setup-roam-db
  [roam-json degree]
  (let [schema {:block/id       {:db/unique :db.unique/identity}
                :block/children {:db/cardinality :db.cardinality/many}}
        conn (ds/create-conn schema)]
    (populate-db! roam-json conn)
    (link-blocks! conn)
    (mark-blocks-for-inclusion! degree conn)
    (generate-hiccup conn)
    conn))
