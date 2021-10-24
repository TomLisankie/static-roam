(ns static-roam.logseq
  (:require [static-roam.utils :as utils]
            [clojure.walk :as walk]
            [org.parkerici.multitool.core :as u])
  )

(defn walk-blocks
  [struct f]
  (walk/postwalk #(if (:uid %) (f %) %) struct))


(defn augment-json
  [in out]
  (-> in
      utils/read-json
      (walk-blocks (fn [block]
                     (if-let [heading (:heading block)]
                       (update block :string #(str (subs "###" 0 heading) " " %))
                       block)))
      (walk-blocks (fn [{:keys [uid create-time edit-time] :as block}]
                     (update block :string #(str %
                                                 (when uid (str "\nid:: " uid))
                                                 (when create-time (str "\ncreated-at:: " create-time))
                                                 (when edit-time (str "\nupdated-at:: " edit-time))
                                                 ))))
      ((u/invert utils/write-json) out)
      ))

#_
(augment-json "/Users/mtravers/Downloads/hyperphor-roam-export.json" "/Users/mtravers/Downloads/hyperphor-roam-export-augmented.json")


;;; Read Logseq EDN

(defn read-edn
  [f]
  (read-string (slurp f)))

(defn logseq-edn->blockmap
  [f]
  (let [pages (-> f
                  read-edn                          ;TODO check version 1
                  :blocks)
        bm (atom {})
        ]
    (letfn [(convert [block]
              (let [b
                    {:id (str (or (:block/page-name block)
                                  (:block/id block)))
                     :title (or (get-in block [:block/properties :title])
                                (:block/page-name block)) ; There is a block/title but it seems like garbage
                     :uid (str (:block/id block))
                     :content (:block/content block) ;TODO strip out properties
                     :edit-time (get-in block [:block/properties :updated-at])
                     :create-time (get-in block [:block/properties :created-at])
                     :children (doall (map (comp :id convert) (:block/children block)))
                     :page? (boolean (:block/page-name block))
                     ;; I guess not, but then need to parse out Markdown headings
                     ;; :heading nil
                     }]
                ;; TODO no we no loner want to index here
                (swap! bm assoc (:id b) b)
                b))]
      (doseq [p pages]
        (convert p))
      (vals @bm))))

      

(-> "/Users/mtravers/Downloads/ammdi-logseq_1634439646.edn"
    logseq-edn->blockmap
    static-roam.database/index-blocks    
    static-roam.database/roam-db-1
    static-roam.core/add-generated-pages
    static-roam.core/output-bm
    )
