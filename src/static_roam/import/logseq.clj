(ns static-roam.import.logseq
  (:require [static-roam.utils :as utils]
            [static-roam.database :as db]
            [static-roam.rendering :as rendering]
            [static-roam.batadase :as bd]
            [static-roam.config :as config]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            ))

;;; Logseq EDN export + repo ⇒ blockmap

;;; Mainly uses EDN export – but should compute that myself.

(defn logseq-edn->blockmap
  [f]
  (prn :reading f)
  (let [pages (-> f
                  utils/read-edn        ;TODO check version 1
                  :blocks)
        bm (atom {})
        ]
    (letfn [(convert [block]
              (let [b
                    {
                     :title (or (get-in block [:block/properties :title])
                                (:block/page-name block)) ; This is often wrong, eg lowercased, so ony use when we have tof
                     :id (if (:block/page-name block)
                           (or (get-in block [:block/properties :title])
                               (:block/page-name block))
                           (str (:block/id block)))
                     :uid (str (:block/id block))
                     :content (:block/content block) ;TODO strip out properties
                     :edit-time (utils/coerce-time (get-in block [:block/properties :updated-at]))
                     :create-time (utils/coerce-time (get-in block [:block/properties :created-at]))
                     :children (doall (map (comp :id convert) (:block/children block)))
                     :page? (boolean (:block/page-name block))
                     }]
                (swap! bm assoc (:id b) b)
                b))]
      (doseq [p pages]
        (convert p))
      (vals @bm))))

(defn produce-bm
  []
  (let [{:keys [directory file-pattern]} (:source (config/config))]
  (-> (utils/latest directory file-pattern)
      logseq-edn->blockmap
      db/index-blocks    
      db/roam-db-1
      bd/add-empty-pages
      db/generate-inverse-refs ;have to redo this after add-empty-pages
      )))

(defn publish-images
  [logseq-dir]
  (doseq [file @rendering/published-images]
    (u/ignore-report
    ;; this tree-hopping is ugly
     (fs/copy+ (str logseq-dir "/assets/" file)
               (str (:output-dir (config/config)) "/pages/" file)))))

;;; TODO Not Logseq specific
(defn publish-assets
  []
  (doseq [file (fs/list-dir "resources/public")]
    (fs/copy+ file
              (str (:output-dir (config/config)) "/assets/" (fs/base-name file)))))

(defn post-generation
  []
  (publish-images (get-in (config/config) [:source :repo]) )
  (publish-assets))
