(ns static-roam.edn
  (:require [static-roam.utils :as utils]
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
(defn read-roam-edn
  "Read a Roam EDN export. Produces an indexed map of block entities"
  [f]
  (->> f
       read-roam-edn-raw
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
                        {:id (or (:node/title eblock) ;Yes this is an abomination of nature but that's how the json thing works
                                 (:block/uid eblock))
                         :content (or (:block/string eblock) ;this is yechy but mimicks the json export
                                      (:node/title eblock))
                         :edit-time (when (:edit/time eblock) (java.util.Date. (:edit/time eblock)))
                         :heading (:block/heading eblock)
                         :children (eblock-children edn eblock)
                         :page? (contains? eblock :node/title)
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
      read-roam-edn
      vals
      (u/index-by #(or (:node/title %)
                       (:block/uid %)))))
