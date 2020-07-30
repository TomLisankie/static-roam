(ns static-roam.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str-utils]
            [hiccup.core :as hiccup]
            [stasis.core :as stasis]
            [datascript.core :as ds]
            [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]
            [static-roam.templating :as templating]
            [clojure.pprint :as pprint]))

(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(str-utils/split % #":: ") metadata))))

(defn- site-metadata
  [conn]
  (let [properties (first (ds/q '[:find ?children
                                         :where
                                         [?id :block/id "SR Metadata"]
                                         [?id :block/children ?children]]
                                       @conn))
        metadata (map #(:block/content (ds/entity @conn [:block/id %])) properties)
        prop-val-dict (metadata-properties metadata)]
    prop-val-dict))

(defn generate-static-roam!
  "Takes a ZIP file (`path-to-zip`) of Roam export JSON as input, explores the
   Roam JSON from the entry point pages up through the `degree` specified (see
   the 'How It Works' doc for more info), transforms the included pages and
   blocks to HTML with the specified Hiccup template and CSS, and outputs the
   resulting HTML to `degree`."
  [path-to-zip output-dir degree]
  (let [json-path (utils/unzip-roam-json-archive
                   path-to-zip
                   (->> path-to-zip
                        (#(str-utils/split % #"/"))
                        drop-last
                        (str-utils/join "/") (#(str % "/"))))
        roam-json (json/read-str (slurp json-path) :key-fn keyword)
        schema    {:block/id       {:db/unique :db.unique/identity}
                   :block/children {:db/cardinality :db.cardinality/many}}
        conn      (ds/create-conn schema)]
    (database/populate-db! roam-json conn)
    (database/link-blocks! conn)
    (database/mark-blocks-for-inclusion! degree conn)
    (let [db         @conn
          id+content (ds/q '[:find ?id ?content
                             :where [?id :block/included true]
                             [?id :block/content ?content]]
                           db)
          tx         (for [[id content] id+content]
               [:db/add id :block/hiccup (parser/block-content->hiccup id content conn)])]
      (ds/transact! conn tx))
    (stasis/export-pages
     (zipmap (utils/html-file-titles (sort-by
                                    #(first %)
                                    (ds/q '[:find ?included-id ?block-title
                                            :where
                                            [?included-id :block/included true]
                                            [?included-id :block/id ?block-title]]
                                          @conn)))
             (map #(hiccup/html (templating/page-hiccup % "../assets/css/main.css" "../assets/js/extra.js"))
                  (map #(templating/block-page-template % conn)
                       (sort-by
                        #(first %)
                        (ds/q '[:find ?included-id ?content
                                :where
                                [?included-id :block/included true]
                                [?included-id :block/content ?content]]
                              @conn)))))
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html
                     (templating/page-index-hiccup
                      (templating/list-of-page-links
                       (sort (ds/q '[:find ?included-page-title
                                     :where
                                     [?id :block/page true]
                                     [?id :block/included true]
                                     [?id :block/id ?included-page-title]]
                                   @conn)) ".")
                      "../assets/css/main.css"
                      "../assets/js/extra.js"))}
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html
                     (templating/home-page-hiccup
                      (templating/list-of-page-links
                       (sort (ds/q '[:find ?entry-point-content
                                     :where
                                     [?id :block/included true]
                                     [?id :block/entry-point true]
                                     [?id :block/content ?entry-point-content]]
                                   @conn)) "pages" "entry-point-link")
                      (get (site-metadata conn) "Title")
                      "./assets/css/main.css"
                      "./assets/js/extra.js"))}
     output-dir)
    (pprint/pprint conn)))

(defn -main
  [path-to-zip output-dir degree]
  (generate-static-roam! path-to-zip output-dir degree))
