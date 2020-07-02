(ns static-roam.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str-utils]
            [clojure.set :as set-fns]
            [hiccup.core :as hiccup]
            [stasis.core :as stasis]
            [datascript.core :as ds]
            [static-roam.utils :as utils]
            [static-roam.parser :as parser])
  (:import (java.util.zip ZipFile)))

(defn entry-point?
  "Determines whether or not a given page is tagged with #EntryPoint in its first child block"
  [page]
  (if (= (count (:children page)) 0)
    false
    (if (and (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children page))))
             (str-utils/includes?
              (:string (first (:children page))) "#EntryPoint"))
      true
      false)))

(defn block-content->hiccup
  "Convert Roam markup to Hiccup"
  [block-ds-id content conn]
  (vec (map parser/ele->hiccup (parser/parse-to-ast content))))

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
                            :block/entry-point (entry-point? block)
                            :block/page (if (:title block)
                                          true
                                          false)
                            :block/refers-to []}])
    (populate-db! (:children block) db-conn)))

(defn page-hiccup ;; TODO I think this gets replaced with the user-defined HTML template later
  [body-hiccup css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body body-hiccup]])

(defn children-of-block-template
  [block-id conn]
  [:ul
   [:li (:block/hiccup (ds/entity @conn [:block/id block-id]))]
   (let [children (:block/children (ds/entity @conn [:block/id block-id]))]
     (if (not= 0 (count children))
       ;; recurse on each of the children
       (map #(children-of-block-template % conn) children)
       ;; otherwise, evaluate to empty vector
       [:div]))])

(defn- linked-references-template
  [references conn]
  (concat []
          (map (fn [r] [:li [:a {:href (str "." (utils/page-title->html-file-title (:block/id (ds/entity @conn (first r))) :case-sensitive))} (second r)]])
               references)))

(defn- linked-references
  [block-ds-id conn]
  (linked-references-template
   (ds/q '[:find ?blocks-that-link-here ?blocks-content
           :in $ ?block-ds-id
           :where
           [?block-ds-id :block/id ?block-id]
           [?blocks-that-link-here :block/refers-to ?block-id]
           [?blocks-that-link-here :block/content ?blocks-content]]
         @conn block-ds-id)
   conn))

(defn- context
  [block-ds-id conn]
  (let [parent-ds-id (ds/q '[:find ?parent-ds-id
                             :in $ ?block-ds-id
                             :where
                             [?block-ds-id :block/id ?block-id]
                             [?parent-ds-id :block/children ?block-id]
                             ]
                           @conn block-ds-id)]
    (if (not (empty? parent-ds-id))
      [:a {:href (str "." (utils/page-title->html-file-title (:block/id (ds/entity @conn (first (first parent-ds-id)))) :case-sensitive))}
       (:block/hiccup (ds/entity @conn (first (first parent-ds-id))))]
      [:div])))

(defn block-page-template
  [block-content conn]
  (let [block-content-text (second block-content)
        block-ds-id (first block-content)]
    [:div
     [:div
      [:h3 (context block-ds-id conn)]]
     [:div
      [:h2 (map parser/ele->hiccup (parser/parse-to-ast block-content))] ;; (block-content->hiccup block-ds-id block-content-text conn)
      (children-of-block-template (:block/id (ds/entity @conn block-ds-id)) conn)]
     [:div {:style "background-color:lightblue;"}
      [:h3 "Linked References"]
      (linked-references block-ds-id conn)]]))

(defn page-index-hiccup
  [link-list css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body link-list]])

(defn home-page-hiccup
  [link-list title css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:title title]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body
    [:header.site-header {:role "banner"}
     [:div.wrapper
      [:a.site-title {:rel "author" :href "."} title]]]
    [:main.page-content {:aria-label="Content"}
     [:div.wrapper
      [:div.home
       [:h2.post-list-heading "Entry Points"]
       link-list]]]]])

(defn- list-of-page-links
  "Generate a Hiccup unordered list of links to pages"
  ([page-titles]
   (let [page-titles-vals (map first page-titles)
         page-links (map page-link-from-title page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir]
   (let [page-titles-vals (map first page-titles)
         page-links (map #(page-link-from-title dir %) page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir link-class]
   (let [page-titles-vals (map first page-titles)
         page-links (map #(page-link-from-title dir % link-class) page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links)))))

(defn- degree-explore!
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

(defn- mark-blocks-for-inclusion!
  [degree conn]
  (if (and (int? degree) (>= degree 0))
    (degree-explore! 0 degree conn)
    (doseq [block-ds-id (vec (ds/q '[:find ?block-id
                                     :where
                                     [_ :block/id ?block-id]]
                                   @conn))]
      (ds/transact! conn [{:block/id (first block-ds-id)
                           :block/included true}]))))

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

(defn -main [path-to-zip output-dir degree]
  (let [path-to-zip path-to-zip
        json-path (utils/unzip-roam-json-archive
                   path-to-zip
                   (->> path-to-zip
                        (#(str-utils/split % #"/"))
                        drop-last
                        (str-utils/join "/") (#(str % "/"))))
        roam-json (json/read-str (slurp json-path) :key-fn keyword)
        schema {:block/id {:db/unique :db.unique/identity}
                :block/children {:db/cardinality :db.cardinality/many}}
        conn (ds/create-conn schema)]
    (populate-db! roam-json conn)
    (mark-blocks-for-inclusion! degree conn)
    (let [db @conn
          id+content (ds/q '[:find ?id ?content
                             :where [?id :block/included true]
                             [?id :block/content ?content]]
                           db)
          tx (for [[id content] id+content]
               [:db/add id :block/hiccup (block-content->hiccup id content conn)])]
      (ds/transact! conn tx))
    (stasis/export-pages
     (zipmap (utils/html-file-titles (sort-by
                                    #(first %)
                                    (ds/q '[:find ?included-id ?block-title
                                            :where
                                            [?included-id :block/included true]
                                            [?included-id :block/id ?block-title]]
                                          @conn)))
             (map #(hiccup/html (page-hiccup % "../assets/css/main.css" "../assets/js/extra.js"))
                  (map #(block-page-template % conn)
                       (sort-by
                        #(first %)
                        (ds/q '[:find ?included-id ?content
                                :where
                                [?included-id :block/included true]
                                [?included-id :block/content ?content]]
                              @conn)))))
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html (page-index-hiccup (list-of-page-links (sort (ds/q '[:find ?included-page-title
                                                                                              :where
                                                                                              [?id :block/page true]
                                                                                              [?id :block/included true]
                                                                                              [?id :block/id ?included-page-title]]
                                                                                            @conn)) ".") "../assets/css/main.css" "../assets/js/extra.js"))}
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html (home-page-hiccup (list-of-page-links (sort (ds/q '[:find ?entry-point-content
                                                                                             :where
                                                                                             [?id :block/included true]
                                                                                             [?id :block/entry-point true]
                                                                                             [?id :block/content ?entry-point-content]]
                                                                                           @conn)) "pages" "entry-point-link") (get (site-metadata conn) "Title") "./assets/css/main.css" "./assets/js/extra.js"))}
     output-dir)
    conn))

(def conn (-main "/home/thomas/Desktop/RoamExports/robert-public-roam.zip" "." :all))
