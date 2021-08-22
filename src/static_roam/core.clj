(ns static-roam.core
  (:require [static-roam.config :as config]
            [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.markdown :as md]
            [static-roam.batadase :as bd]
            [static-roam.html-generation :as html-gen]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]))

(defn block-map
  [path-to-zip]
  (prn :reading-from path-to-zip)       ;TODO I suppose real logger is called for
  (-> path-to-zip
      utils/read-roam-json-from-zip
      database/roam-db
      (html-gen/generated-page "Index" html-gen/generate-index-pages)
      (html-gen/generated-page "New" html-gen/generate-recent-page) ;TODO would make sense to put these under a config
      (html-gen/generated-page "Map" html-gen/generate-global-map)
      (html-gen/generated-page "DataViz" html-gen/generate-dataviz)
      ))

(defn pp-export
  [path-to-zip]
  (->> path-to-zip
      utils/read-roam-json-from-zip
      (ju/schppit "block-dump.edn")))

(defonce last-bm (atom nil))

(defn pages
  []
  (keys (u/dissoc-if (fn [[_ v]] (not (:page? v))) @last-bm)))

;;; Sloooow. Dumps pages including dchildren
(defn page-dump
  []
  (ju/schppit
   (str (config/config :output-dir) "pages.edn")
   (u/map-filter (fn [b] (and (:page? b) b))
                 (vals @last-bm))))


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

(defn reset-output
  []
  (let [pages-dir (str (config/config :output-dir) "/pages" )]
    (fs/delete-dir pages-dir)
    (fs/mkdir pages-dir)))

(defn gen-page
  [page]
  (html-gen/generate-content-page @last-bm (config/config :output-dir) (get @last-bm page)))

(defn gen-pages
  []
  (reset-output)
  (html-gen/generate-static-roam @last-bm (config/config :output-dir)))

(defn -main
  [& [config-or-path]]
  (if (map? config-or-path)
    (config/set-config-map config-or-path)
    (config/set-config-path (or config-or-path "default-config.edn")))
  (reset-output)
  (u/memoize-reset)
  (-> (utils/latest-export)
      block-map
      tap
      (html-gen/generate-static-roam (config/config :output-dir)))
  ;; TODO options for writing all pages
  (when (config/config :markdown-output-dir)
    (md/write-displayed-pages @last-bm (config/config :markdown-output-dir)))
  (prn (bd/stats @last-bm))
  #_ (dump))




