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
        hashtags-found
        (utils/find-hashtags-in-string string)
        metadata-found
        (utils/find-metadata-in-string string)
        extra-paren-removed
        (utils/remove-heading-parens content-entities-found)
        cleaned-content-entities
        (map utils/remove-double-delimiters extra-paren-removed)
        cleaned-hashtags
        (map utils/remove-leading-char hashtags-found)
        cleaned-metadata
        (map utils/remove-double-colon metadata-found)
        all-cleaned-entities
        (concat cleaned-content-entities cleaned-hashtags cleaned-metadata)]
    all-cleaned-entities))

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
  (into #{} (map first (filter entry-point? block-map))))

(defn- get-children-of-block
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (:children block-props)))

(defn- get-all-children-of-blocks
  [block-ids block-map]
  (reduce into #{} (map #(get-children-of-block % block-map) block-ids)))

(defn- get-references-for-block
  [block-id block-map]
  (let [block-props (get block-map block-id)]
    (:refers-to block-props)))

(defn- get-references-for-blocks
  [block-ids block-map]
  (let [children (get-all-children-of-blocks block-ids block-map)
        references (reduce into #{} (map #(get-references-for-block % block-map) children))]
    (reduce into #{} [children references])))

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

(defn- get-content-entity-ids-to-include
  [degree block-map]
  (let [entry-point-ids (into (get-entry-point-ids block-map) #{"SR Metadata"})]
    (loop [entities-to-examine entry-point-ids
           children-for-each-entity (get-all-children-recursively entities-to-examine block-map)
           all-children-of-examined (aggregate-children children-for-each-entity)
           references-for-children (get-child-content-references all-children-of-examined block-map)
           all-references-of-children (aggregate-references references-for-children)
           included-entities (generate-included-entities entities-to-examine all-children-of-examined all-references-of-children)
           current-degree 0
           max-degree degree]
      (if (>= current-degree max-degree)
        included-entities
        (let [entities-to-examine all-references-of-children
              children-for-each-entity (get-all-children-recursively entities-to-examine block-map)
              all-children-of-examined (aggregate-children children-for-each-entity)
              references-for-children (get-child-content-references all-children-of-examined block-map)
              all-references-of-children (aggregate-references references-for-children)
              included-entities (generate-included-entities included-entities all-children-of-examined all-references-of-children)
              current-degree (inc current-degree)
              max-degree max-degree]
          (recur entities-to-examine
                 children-for-each-entity
                 all-children-of-examined
                 references-for-children
                 all-references-of-children
                 included-entities
                 current-degree
                 max-degree))))))

(defn- block-id-included?
  [block-id included-block-ids]
  (contains? included-block-ids block-id))

(defn- mark-block-if-included
  [included-block-ids block-kv]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    (if (block-id-included? block-id included-block-ids)
      [block-id (assoc block-props :included true)]
      [block-id (assoc block-props :included false)])))

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

(defn- give-header
  [the-hiccup block-props]
  [(keyword (str "h" (:heading block-props))) the-hiccup])

(defn- generate-hiccup-if-block-is-included
  [block-kv block-map]
  (let [block-id (first block-kv)
        block-props (second block-kv)]
    [block-id
     (if (true? (:included block-props))
       (assoc block-props
              :hiccup
              (let [heading (:heading block-props)
                    the-hiccup (parser/block-content->hiccup (:content block-props) block-map)]
                (if (> (:heading block-props) 0)
                  (give-header the-hiccup block-props)
                  the-hiccup)))
       block-props)]))

(defn generate-hiccup-for-included-blocks
  [block-map]
  (into (hash-map) (filter #(not= nil (:content (second %))) (map #(generate-hiccup-if-block-is-included % block-map) block-map))))

(defn replicate-roam-db
  [roam-json]
  (let [block-map-no-links (create-block-map-no-links roam-json)
        block-map-linked-by-and-refers-to (generate-links-and-backlinks block-map-no-links)]
    (into (hash-map) block-map-linked-by-and-refers-to)))

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
  [roam-json degree]
  (let [replicated-roam-block-map (replicate-roam-db roam-json)
        blocks-tagged-for-inclusion (mark-content-entities-for-inclusion degree replicated-roam-block-map)
        children-of-embeds-added (add-children-of-block-embeds blocks-tagged-for-inclusion)
        hiccup-for-included-blocks (generate-hiccup-for-included-blocks children-of-embeds-added)]
    hiccup-for-included-blocks))

(defn- mark-refs-as-included
  [roam-db-conn degree entry-point-eids]
  ;; loop over entry-points, gather refs that their children include, mark those as included, repeat with degree lesser and the new eids
  (loop [entry-point-eids entry-point-eids
         current-entry-point (first entry-point-eids)
         refs (get-refs-for-page roam-db-conn current-entry-point)]
    (let [remaining-entry-points (rest entry-point-eids)
          next-entry-point (first (rest entry-point-eids))]
      (if (= (count entry-point-eids) 0)
        (mark-refs-as-included roam-db-conn (dec degree) refs)
        (recur remaining-entry-points
               next-entry-point
               (get-refs-for-page roam-db-conn next-entry-point))))))

(defn- include-children-of-included-pages
  [roam-db-conn])

(defn- include-explicitly-included-blocks
  [roam-db-conn include-tags]
  (let [sr-info-eid (first (first (ds/q '[:find ?eid
                                          :where
                                          [?eid :node/title "Static-Roam Info"]]
                                        @roam-db-conn)))
        explicit-include-eids (map #(first (first %))
                                   (map #(ds/q '[:find ?eid
                                                 :in $ ?tag
                                                 :where
                                                 [?eid :node/title ?tag]]
                                               @roam-db-conn %)
                                        include-tags))
        eids-of-explicit-includes (reduce into []
                                          (map first
                                               (map #(ds/q '[:find ?parent-eid
                                                             :in $ ?explicit-include-tag-eid
                                                             :where
                                                             [?eid :block/refs sr-info-eid]
                                                             [?eid :block/refs ?explicit-include-tag-eid]
                                                             [?eid :block/parents ?parent-eid]]
                                                           @roam-db-conn %)
                                                    eids-of-explicit-includes)))
        transactions (vec (map (fn [eid] [:db/add eid :static-roam/included true]) eids-of-explicit-includes))
        ;; TODO include children as well
        ]
    (ds/transact! roam-db-conn transactions)))

(defn- exclude-explicitly-excluded-blocks
  [roam-db-conn exclude-tags]
  (let [sr-info-eid (first (first (ds/q '[:find ?eid
                                          :where
                                          [?eid :node/title "Static-Roam Info"]]
                                        @roam-db-conn)))
        explicit-exclude-eid (map #(first (first %))
                                  (map #(ds/q '[:find ?eid
                                                :in $ ?tag
                                                :where
                                                [?eid :node/title ?tag]]
                                              @roam-db-conn %)
                                       exclude-tags))
        eids-of-explicit-excludes (reduce into []
                                          (map first
                                               (map #(ds/q '[:find ?parent-eid
                                                             :in $ ?explicit-exclude-tag-eid
                                                             :where
                                                             [?eid :block/refs sr-info-eid]
                                                             [?eid :block/refs ?explicit-exclude-tag-eid]
                                                             [?eid :block/parents ?parent-eid]]
                                                           @roam-db-conn %)
                                                    eids-of-explicit-excludes)))
        transactions (vec (map (fn [eid] [:db/add eid :static-roam/included false]) eids-of-explicit-excludes))
        ;; TODO exclude children as well
        ]
    (ds/transact! roam-db-conn transactions)))

(defn determine-which-content-to-include
  [roam-db-conn degree config]
  ;; mark all entities "included" field as `false` initially
  (let [entry-point-tags (:entry-point-tags config)
        include-tags (:include config)
        exclude-tags (:exclude config)
        eids (map first (ds/q '[:find ?eid :where [?eid]] @roam-db-conn))
        transactions (vec (map (fn [eid] [:db/add eid :static-roam/included false]) eids))]
    (ds/transact! roam-db-conn transactions)) ;; might want to make this more nuanced so only entities with :block/string or :node/title get included
  ;; then determine which content to include
      ;; find pages marked as entry points and mark them as included
  (let [sr-info-eid (first (first (ds/q '[:find ?eid
                                          :where
                                          [?eid :node/title "Static-Roam Info"]]
                                        @roam-db-conn)))
        entry-point-tag-eids (map #(first (first %))
                                  (map #(ds/q '[:find ?eid
                                                :in $ ?tag
                                                :where
                                                [?eid :node/title ?tag]]
                                              @roam-db-conn %)
                                       entry-point-tags))
        eids-of-entry-points (reduce into []
                                     (map first
                                          (map #(ds/q '[:find ?parent-eid
                                                        :in $ ?entry-point-tag-eid
                                                        :where
                                                        [?eid :block/refs sr-info-eid]
                                                        [?eid :block/refs ?entry-point-tag-eid]
                                                        [?eid :block/parents ?parent-eid]]
                                                      @roam-db-conn %)
                                               entry-point-tag-eids)))
        transactions (vec (map (fn [eid] [:db/add eid :static-roam/included true]) eids-of-entry-points))]
    (ds/transact! roam-db-conn transactions)
    ;; find all pages ref'd by the entry point children and mark them as included as well. repeat this up through the degree
    (mark-refs-as-included roam-db-conn degree eids-of-entry-points)
    ;; up to this point, the only entities marked as visible have been the blocks containing the titles of the entry points. So now, aggregate all of the children (recursively) and mark their `included` as `true`
    (include-children-of-included-pages roam-db-conn)
    ;; now that all of the entry point stuff is done, mark all of the pages/blocks (and their children) marked for explicit inclusion `included` to `true`
    (include-explicitly-included-blocks roam-db-conn include-tags)
    ;; and now that all of the pages/blocks that are going to be included have been marked as such, mark the `included` to `false` for all the pages/blocks (and their children) marked for explicit exclusion
    (exclude-explicitly-excluded-blocks roam-db-conn exclude-tags)))
