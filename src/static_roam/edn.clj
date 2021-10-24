(ns static-roam.edn
  (:require [static-roam.utils :as utils]
            [static-roam.batadase :as bd]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]))

;;; Experiments in reading Roam EDN dump
;;; Why? The JSON format is lossy, it turns out. It is missing the UIDs for pages;
;;; needed to make links back to Roam.

(defn read-roam-edn-raw
  [f]
  (with-open [infile (java.io.PushbackReader. (clojure.java.io/reader f))]
    (binding [*in* infile
              *data-readers* (assoc *data-readers* 'datascript/DB identity)]
      (read))))

(def schema (atom nil))

;;; Very unclojurish, sue me.
(defn grab-schema
  [raw-edn]
  (reset! schema (:schema raw-edn))
  raw-edn)

(defn many-valued?
  [att]
  (= :db.cardinality/many (get-in @schema [att :db/cardinality])))

(defn entify
  [datoms]
  (assoc 
   (reduce (fn [acc [_ att v _]]
             (if (many-valued? att)
               (update acc att conj v)
               (assoc acc att v)))
           {}
           datoms)
   :db/id
   (ffirst datoms)))

;;; To multitool, except it is kind of useless since you still have to use special fns like assoc!
(defn reduce-transient
  [f init col]
  (persistent!
   (reduce f (transient init) col)))

;;; -> multitool, unaccountably missing from core
(defn update!
  [map k f & args]
  (assoc! map k (apply f (get map k) args)))

(defn entify
  [datoms]
  (assoc 
   (reduce-transient (fn [acc [_ att v _]]
                       (if (many-valued? att)
                         (update! acc att conj v)
                         (assoc! acc att v)))
                     {}
                     datoms)
   :db/id
   (ffirst datoms)))

;;; TODO do the unzip if we really use this
(defn process-roam-edn
  "Read a Roam EDN export. Produces an indexed map of block entities"
  [datascript]
  (->> datascript
       grab-schema
       :datoms
       (group-by first)
       (map (comp entify second))
       (u/index-by :db/id)
       ))

;;; Tentative idea, match output of database/create-block-map-no-links
(defn- eblock-children
  [edn eblock]
  (->> eblock
       :block/children
       (sort-by #(get-in edn [% :block/order]))
       (map #(get-in edn [% :block/uid]))))

(defn edn->block-map
  [edn]
  (let [eblocks (remove #(and (nil? (:block/string %)) (nil? (:node/title %)))
                        (vals edn))]
    (utils/add-parent
     (u/index-by :id
                 (map (fn [eblock]

                        ;; TODO? Grab other fields that may be useful
                        ;; :create/time :edit/user :create/user (don't care but maybe should keep)
                        ;; :block/open (added)
                        ;; :block/refs (I think we basically recompute these?)
                        ;; :block/parents (can be multiple, but what )

                        ;; So many identities
                        {:id (or (:node/title eblock) ;Yes this is an abomination of nature but that's how the json thing works
                                 (:block/uid eblock))
                         :uid (:block/uid eblock)
                         :db/id (:db/id eblock)      ; for reconstruction
                         :content (or (:block/string eblock) ;this is yechy but mimicks the json export
                                      (:node/title eblock))
                         :edit-time (when (:edit/time eblock) (java.util.Date. (:edit/time eblock)))
                         :create-time (when (:create/time eblock) (java.util.Date. (:create/time eblock)))
                                    
                         :heading (:block/heading eblock)
                         :children (eblock-children edn eblock)
                         :page? (contains? eblock :node/title)
                         :open? (:block/open eblock) ;don't actually use this, but want to preserve it nonetheless

                         }

                        )
                      eblocks))
     :children :parent)))
  

;;; The schema (included in the export, so here just for reference)

#_
{:node/subpages {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :vc/blocks {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :edit/seen-by {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :window/id {:db/unique :db.unique/identity},
 :attrs/lookup {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :node/windows {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :d/v {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :block/clone {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :node/sections {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :harc/v {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :node/title {:db/unique :db.unique/identity},
 :block/refs {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :harc/a {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :block/subpage {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :block/children {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :block/focused-user {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :create/seen-by {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :block/uid {:db/unique :db.unique/identity},
 :d/e {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :d/a {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :node/links {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :link/to {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :user/email {:db/unique :db.unique/identity},
 :query/results {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :harc/e {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :block/parents {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many},
 :block/page {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one},
 :version/id {:db/unique :db.unique/identity}}


(defn edn-for-debugging
  []
  (->> (utils/latest-export)
       utils/unzip-roam
       read-roam-edn-raw
       process-roam-edn
       vals
       (u/index-by #(or (:node/title %)
                        (:block/uid %)))))


;;; Has to be either a loop or a double reduce, seems harder than it should be
;;; → multitool? Needs a bit of generalization, but this would be useful for the general case of mapping sequential data onto a Datomic or similar model
(defn compute-block-orders
  [bm]
  (loop [bm bm
         blocks (vals bm)
         order 0
         children nil]
    (cond (not (empty? children))
          (recur (assoc-in bm [(first children) :order] order)
                 blocks
                 (inc order)
                 (rest children))
          (not (empty? blocks))
          (recur bm
                 (rest blocks)
                 0
                 (:children (first blocks)))
          :else
          bm)))

;;; EDN GENERATION from blockmaps
(defn bm->datoms
  [bm]                                  
  (let [bm (compute-block-orders bm)]
    (mapcat
     (fn [block]
       (let [db-id (:db/id block)
             txn 536871009
             v (fn [value-gen] (if (ifn? value-gen) (value-gen block) value-gen))
             field (fn [field value-gen]
                     (if (nil? (v value-gen))
                       []
                       [[db-id field (v value-gen) txn]]))
             field* (fn [field value-gen]
                      (mapv (fn [ve] [db-id field ve txn]) (v value-gen)))
             page (get-in bm [(:id (bd/block-page bm block)) :db/id])
             page? (= page db-id)]
         (concat
          (field :block/string #(if (:page? block) nil (:content %)))
                                        ;     (field :create/time #(when (:creation %) (.getTime (:creation %)) ))
          (field :edit/time #(when (:edit-time %) (.getTime (:edit-time %))))
          (field :edit/user 1)
          (field :create/time  #(when (:create-time %) (.getTime (:create-time %))))
          (field :create/user 1)
          (field :block/uid :uid)
          (field :block/open :open?)
          (field* :block/children (fn [block] (map #(get-in bm [% :db/id]) (:children block))))
          ;; um no
          ;;     (field :db/id db-id)
          (field :block/order #(if page? nil (get % :order 0)))
          (field :node/title #(if (:page? %) (:content %) nil))

          (field :block/page (when-not page? page))
          (field* :block/parents (fn [block] (map #(get-in bm [(:id %) :db/id]) (bd/ancestors bm block))))

          (field* :block/refs (fn [block]
                                (remove nil?
                                        (map #(get-in bm [% :db/id]) (:refs block))))))))

     (vals bm))))

(defn write-datoms
  [datoms f]
  (let [db  {:schema @schema
             :datoms datoms}]
    (binding [*print-length* nil]
      (with-open [^java.io.Writer w (clojure.java.io/writer f)]
        (.write w "#datascript/DB\n")
        (.write w (str db))))))



(defn athens-export
  [bm outfile]
  (-> bm
      bm->datoms
      (write-datoms outfile)))

(defn subset-bm
  [bm page-pred]
  (let [pages (filter page-pred (bd/pages bm))
        included (mapcat bd/block-descendents pages)]
    (prn :foo (count pages) ( count included) (count bm))
    (u/index-by :id included)))

(defn daily-notes-page?
  [page]
  (let [title (or (:title page) (:content page))]
    (when title
      (re-matches bd/daily-notes-regex title))))
