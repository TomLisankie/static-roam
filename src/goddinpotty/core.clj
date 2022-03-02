(ns goddinpotty.core
  (:require [goddinpotty.config :as config]
            [goddinpotty.utils :as utils]
            [goddinpotty.database :as database]
            [goddinpotty.batadase :as bd]
            [goddinpotty.html-generation :as html-gen]
            [goddinpotty.graph :as graph]
            [goddinpotty.index :as index]
            [goddinpotty.import.logseq :as logseq]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]))

;;; TODO this is a mess and should be cleaned up
(defn add-generated-pages
  [bm]
  (-> bm
      (merge (index/make-index-pages bm))
      ;; Out of service; Logseq doesn't generate per-block dates
      ;; Although could pull them from git with some work... TODO
      #_(html-gen/generated-page "New" html-gen/generate-recent-page) 
      (html-gen/generated-page "Map" html-gen/generate-global-map)
      ))

(defn block-map-json
  [path-to-zip]
  (prn :reading-from path-to-zip)       ;TODO I suppose real logger is called for
  (-> path-to-zip
      utils/read-roam-json-from-zip
      database/roam-db
      add-generated-pages
      ))

#_
(defn block-map-edn
  [path]
  (prn :reading-from path)
  (-> path
      database/roam-db-edn
      add-generated-pages
      ))

(defn pp-export
  [path-to-zip]
  (->> path-to-zip
      utils/read-roam-json-from-zip
      (ju/schppit "block-dump.edn")))

;;; BTW I want this available in all namespaces and it should be easy to do that.
(defonce last-bm (atom nil))

(defn pages
  []
  (keys (u/dissoc-if (fn [[_ v]] (not (:page? v))) @last-bm)))

;;; Sloooow. Dumps pages including dchildren
(defn page-dump
  []
  (ju/schppit
   (str (config/config :output-dir) "pages.edn")
   (u/clean-seq (map (fn [b] (and (:page? b) b))
                     (vals @last-bm)))))


(defn block-dump
  [& {:keys [all?]}]
  "Dumps included blocks or all blocks in order; the idea is this should be diffable. Also slow."
  (ju/schppit
   "blocks.edn"
   (into (sorted-map)
         (u/map-values #(dissoc % :dchildren)
                       (if all?
                         @last-bm
                         (u/clean-map @last-bm (comp not :include?)))))))

(defn tap
  [bm]
  (reset! last-bm bm)
  bm)

(defn delete-dir-contents
  "Like fs/delete-dir, but clears rather than deletes"
  [dir]
  (doseq [f (fs/list-dir dir)]
    (fs/delete-dir f)))

(defn reset-output
  []
  (let [pages-dir (config/config :output-dir)]
    (delete-dir-contents pages-dir)
    (fs/mkdir pages-dir)))

(defn gen-page
  [page]
  (u/memoize-reset!)
  (html-gen/generate-content-page @last-bm (config/config :output-dir) (get @last-bm page)))

(defn gen-pages
  []
  (u/memoize-reset!)
  (reset-output)
  (html-gen/generate-goddinpotty @last-bm (config/config :output-dir)))

(defn reset
  []
  (u/memoize-reset!)
  ;; reset render/published-images
  (reset! goddinpotty.rendering/published-images #{})
  )

(defn output-bm
  [bm]
  (reset-output)
  (let [output-dir (config/config :output-dir)]
    (graph/write-page-data bm output-dir)
    (html-gen/generate-goddinpotty bm output-dir))
  ;; TODO options for writing all pages
  ;;; Turning this off for now, Logseqe output is more important
  ;;; Should be rationalized; html and md output should be modules
  #_
  (when (config/config :markdown-output-dir)
    (md/write-displayed-pages @last-bm (config/config :markdown-output-dir)))
  (prn (bd/stats @last-bm))
  #_ (dump))

(defmulti produce-bm (fn [{:keys [source]}] (:type source)) )
  
;;; Sometimes I hate Clojure
(defmethod produce-bm :logseq [_]
  (logseq/produce-bm (config/config)))

(defmulti post-generation (fn [{:keys [source]} _] (:type source)))

(defmethod post-generation :logseq [_ _]
  (logseq/post-generation))

(defn -main
  [& [config-or-path]]
  (if (map? config-or-path)
    (config/set-config-map! config-or-path)
    (config/set-config-path! (or config-or-path "default-config.edn")))
  (reset)
  (let [bm (add-generated-pages (produce-bm (config/config)))]
    (tap bm)
    (output-bm bm)
    (html-gen/generate-index-redir (config/config  :output-dir))
    (post-generation (config/config) bm)
    ))

#_
(defn scarf-images
  [roam-export dir]
  (let [bm (block-map-json roam-export)]
    (goddinpotty.curation/image-copy-script bm dir)))


(set! *print-length* 100)               ;prevent trace from blowing up trying to print bms. Should be elsewhere
