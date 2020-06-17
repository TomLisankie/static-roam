(ns static-roam.core
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str-utils]
            [clojure.set :as set-fns]
            [hiccup.core :as hiccup]
            [stasis.core :as stasis]
            [datascript.core :as ds]
            [static-roam.parser :as parser])
  (:import (java.util.zip ZipFile)))

                                        ; 1) GET PAGES TO INCLUDE ON SITE

(defn unzip-roam-json-archive
  "Takes the path to a zipfile `source` and unzips it to target-dir, returning the path of the target file"
  [source target-dir]
  (str target-dir (with-open [zip (ZipFile. (fs/file source))]
                    (let [entries (enumeration-seq (.entries zip))
                          target-file #(fs/file target-dir (str %))
                          database-file-name (.getName (first entries))]
                      (doseq [entry entries :when (not (.isDirectory ^java.util.zip.ZipEntry entry))
                              :let [f (target-file entry)]]
                        (fs/mkdirs (fs/parent f))
                        (io/copy (.getInputStream zip entry) f))
                      database-file-name))))

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

(defn to-rl-json
  "Filters out irrelevant info from Roam JSON"
  [page]
  {:title (:title page)
   :entry-point (entry-point? page)
   :date (if (entry-point? page)
           (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children page))))
           nil)
   :children (if (entry-point? page)
               (rest (:children page))
               (:children page))})

(defn title-content-pair
  "Creates an ordered pair of the title of the page and the page itself"
  [page]
  [(:title page) page])

(defn remove-n-surrounding-delimiters
  "Removes n surrounding characters from both the beginning and end of a string"
  [n string]
  (subs string n (- (count string) n)))

(defn remove-double-delimiters
  "Removes 2 surrounding characters from both the beginning and end of a string"
  [string]
  (remove-n-surrounding-delimiters 2 string))

(defn remove-triple-delimiters
  "Removes 3 surrounding characters from both the beginning and end of a string"
  [string]
  (remove-n-surrounding-delimiters 3 string))

(defn get-pages-referenced-in-string
  "Returns a sequence of all page references in a string"
  [string]
  (concat
   (map remove-double-delimiters (re-seq #"\[\[.*?\]\]" string))
   (map #(subs % 1) (re-seq #"\#..*?(?=\s|$)" string))
   (map
    #(subs % 0 (- (count %) 2))
    (re-seq #"^.+?::" string))))

(defn get-blocks-referenced-in-string
  "Returns a sequence of all block references in a string"
  [string]
  (map remove-double-delimiters (re-seq #"\(\(.*?\)\)" string)))

(defn pages-mentioned-by-children
  "Finds all pages mentioned in all blocks of a page"
  [entry-point-title page-to-content-map]
  (when (:children (get page-to-content-map entry-point-title))
    (->>
     (map #(->> % first second get-pages-referenced-in-string)
          (tree-seq #(:children %)
                    #(:children %)
                    (get page-to-content-map entry-point-title)))
     flatten
     set)))

(defn find-all-included-pages
  "Finds all pages to be included for a Static-Roam site"
  [unexplored-entry-points max-depth page-to-content-map]
  (loop [explored #{}
         unexplored (set unexplored-entry-points)
         current-depth 0]
    (if (> current-depth max-depth)
      explored
      (recur (set-fns/union unexplored explored)
             (reduce set-fns/union
                     (map #(pages-mentioned-by-children %
                                                        page-to-content-map)
                          unexplored)) (inc current-depth)))))

(defn child-block-ids-content-map
  "Generates a map of block IDs to their content"
  [page]
  (loop [children (:children page)
         id-to-content {}]
    (if (or (= 0 (count children)) (nil? children))
      id-to-content
      (recur
       (rest children)
       (into
        id-to-content
        [{(:uid (first children))
          (:string (first children))}
         (child-block-ids-content-map (first children))])))))

;; 2) STATIC SITE GENERATION

(defn- strip-chars
  "Removes every character of a given set from a string"
  [chars collection]
  (reduce str (remove #((set chars) %) collection)))

(defn page-title->html-file-title
  "Formats a Roam page title as a name for its corresponding HTML page (including '.html' extension)"
  ([string]
   {:pre [(string? string)]}
   (->> string
        (str-utils/lower-case)
        (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
        (#(str-utils/replace % #"\s" "-"))
        (#(str "/" % ".html"))))
  ([string case-sensitive?]
   {:pre [(string? string)]}
   (->> string
        (#(if case-sensitive?
            %
            (str-utils/lower-case %)))
        (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
        (#(str-utils/replace % #"\s" "-"))
        (#(str "/" % ".html")))))

(defn get-youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [string]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube-nocookie.com/embed/"
                      (cond
                        (re-find #"youtube\.com" string) (subs string 32)
                        (re-find #"youtu\.be" string) (subs string 17)
                        :else "NO VALID ID FOUND"))
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
            :allowfullscreen ""}])

(defn html-file-titles
  "Get a sequence of all given page titles as file names for their corresponding HTML"
  ([page-titles]
   (map page-title->html-file-title page-titles))
  ([page-titles case-sensitive?]
   (map #(page-title->html-file-title % case-sensitive?) page-titles)))

(defn- page-link-from-title
  "Given a page and a directory for the page to go in, create Hiccup that contains the link to the HTML of that page"
  ([dir block-content]
   [:a {:href (str dir (page-title->html-file-title block-content :case-sensitive))} block-content])
  ([block-content]
   [:a {:href (page-title->html-file-title block-content :case-sensitive)} block-content])
  ([dir block-content link-class]
   [:a {:class link-class
        :href (str dir (page-title->html-file-title block-content :case-sensitive))}
    block-content]))

(defn included?
  [id-passed conn]
  (true? (:block/included (ds/entity @conn [:block/id id-passed]))))

(defn excluded?
  [block-id conn]
  (not (included? block-id conn)))

(defn content-find
  [id-passed conn]
  (:block/content (ds/entity @conn [:block/id id-passed])))

(defn- replace-double-brackets
  [block-ds-id content conn]
  ;; find all pages it mentions and add transact them to db
  (let [tx (for [reference (map remove-double-delimiters (re-seq #"\[\[.*?\]\]" content))]
             [:db/add block-ds-id :block/refers-to reference])]
    (ds/transact! conn tx))
  ;; then replace them with links
  (str-utils/replace
   content
   #"\[\[.*?\]\]"
   #(if (included? (remove-double-delimiters %) conn)
      (str "[" (remove-double-delimiters %)
           "](." (page-title->html-file-title % :case-sensitive) ")")
      (remove-double-delimiters %))))

(defn- replace-hashtags
  [block-ds-id content conn]
  ;; find all hashtags (pages) mentioned and transact them to db
  ;; (let [tx (for [reference (map remove-double-delimiters (re-seq #"\#..*?(?=\s|$)" content))]
  ;;            [:db/add block-ds-id :block/refers-to reference])]
  ;;   (ds/transact! conn tx))
  ;; then replace them with links
  (str-utils/replace
   content
   #"\#..*?(?=\s|$)"
   #(str "[" (subs % 1) "](." (page-title->html-file-title % :case-sensitive) ")")))

(defn- transclude-block-refs
  [block-ds-id content conn]
  ;; find block references and transact to db
  (let [tx (for [reference (map remove-double-delimiters (re-seq #"\(\(.*?\)\)" content))]
             [:db/add block-ds-id :block/refers-to reference])]
    (ds/transact! conn tx))
  ;; then replace with links
  (str-utils/replace
   content
   #"\(\(.*?\)\)"
   #(str "[" (content-find (remove-double-delimiters %) conn)
         "](." (page-title->html-file-title % :case-sensitive) ")")))

(defn- replace-metadata
  [block-ds-id content conn]
  ;; find metadata tags and transact to db
  (let [tx (for [reference (map remove-double-delimiters (re-seq #"^.+?::" content))]
             [:db/add block-ds-id :block/refers-to reference])]
    (ds/transact! conn tx))
  ;; then replace with links
  (str-utils/replace
   content
   #"^.+?::"
   #(str
     "__[" (subs % 0 (- (count %) 2)) ":](."
     (page-title->html-file-title % :case-sensitive) ")__")))

(defn roam-web-elements
  ([block-ds-id content conn]
   (let [todos-replaced (str-utils/replace
                         content
                         #"\{\{\[\[TODO\]\]\}\}"
                         "<input type=\"checkbox\" disabled>")
         dones-replaced (str-utils/replace
                         todos-replaced
                         #"\{\{\[\[DONE\]\]\}\}"
                         "<input type=\"checkbox\" checked disabled>")
        youtubes-replaced (str-utils/replace
                           dones-replaced
                           #"\{\{youtube: .*?\}\}"
                           #(get-youtube-vid-embed %))
         double-brackets-replaced (replace-double-brackets block-ds-id youtubes-replaced conn)
         hashtags-replaced (replace-hashtags block-ds-id double-brackets-replaced conn)
         block-alias-links (str-utils/replace
                            hashtags-replaced
                            #"\[.*?\]\(\(\(.*?\)\)\)"
                            #(str
                              (re-find #"\[.*?\]" %)
                              "(." (page-title->html-file-title
                                    (remove-triple-delimiters
                                     (re-find #"\(\(\(.*?\)\)\)" %))) ")"))
         block-refs-transcluded (transclude-block-refs block-ds-id block-alias-links conn)
         metadata-replaced (replace-metadata block-ds-id block-refs-transcluded conn)]
     (if (or
          (re-find #"\[\[.*?\]\]" metadata-replaced)
          (re-find #"\#..*?(?=\s|$)" metadata-replaced)
          (re-find #"\(\(.*?\)\)" metadata-replaced)
          (re-find #"^.+?::" metadata-replaced))
       (roam-web-elements block-ds-id metadata-replaced conn)
       metadata-replaced)))
  ([content conn]
   (let [todos-replaced (str-utils/replace
                        content
                        #"\{\{\[\[TODO\]\]\}\}"
                        "<input type=\"checkbox\" disabled>")
        dones-replaced (str-utils/replace
                        todos-replaced
                        #"\{\{\[\[DONE\]\]\}\}"
                        "<input type=\"checkbox\" checked disabled>")
        youtubes-replaced (str-utils/replace
                           dones-replaced
                           #"\{\{youtube: .*?\}\}"
                           #(get-youtube-vid-embed %))
        double-brackets-replaced (str-utils/replace
                                  youtubes-replaced
                                  #"\[\[.*?\]\]"
                                  #(if (included? (remove-double-delimiters %) conn)
                                     (str "[" (remove-double-delimiters %)
                                          "](." (page-title->html-file-title % :case-sensitive) ")")
                                     (remove-double-delimiters %)))
        hashtags-replaced (str-utils/replace
                           double-brackets-replaced
                           #"\#..*?(?=\s|$)"
                           #(str "[" (subs % 1) "](." (page-title->html-file-title % :case-sensitive) ")"))
        block-alias-links (str-utils/replace
                           hashtags-replaced
                           #"\[.*?\]\(\(\(.*?\)\)\)"
                           #(str
                             (re-find #"\[.*?\]" %)
                             "(." (page-title->html-file-title
                                   (remove-triple-delimiters
                                    (re-find #"\(\(\(.*?\)\)\)" %))) ")"))
        block-refs-transcluded (str-utils/replace
                                block-alias-links
                                #"\(\(.*?\)\)"
                                #(str "[" (content-find (remove-double-delimiters %) conn)
                                      "](." (page-title->html-file-title % :case-sensitive) ")"))
        metadata-replaced (str-utils/replace
                           block-refs-transcluded
                           #"^.+?::"
                           #(str
                             "__[" (subs % 0 (- (count %) 2)) ":](."
                             (page-title->html-file-title % :case-sensitive) ")__"))]
    (if (or
         (re-find #"\[\[.*?\]\]" metadata-replaced)
         (re-find #"\#..*?(?=\s|$)" metadata-replaced)
         (re-find #"\(\(.*?\)\)" metadata-replaced)
         (re-find #"^.+?::" metadata-replaced))
      (roam-web-elements metadata-replaced conn)
      metadata-replaced))))

(defn query
  [query-string]
  ;; TODO write this function
  query-string)

(defn word-count
  [block-ds-id conn]
  ;; TODO Implement this function
  "0")

(defn hiccup-of-ele
  [block-ds-id ast-ele conn]
  (let [ele-content (second ast-ele)]
    (case (first ast-ele)
      :page-link (if (included? (remove-double-delimiters ele-content) conn)
                   [:a {:href (page-title->html-file-title (remove-double-delimiters ele-content) :case-sensitive)}
                    (remove-double-delimiters ele-content)]
                   (remove-double-delimiters ele-content))
      :block-ref (if (included? ele-content conn)
                   [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                    (content-find ele-content conn)]
                   "REDACTED")
      :metadata-tag (if (included? ele-content conn)
                      [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                       (str ele-content ":")]
                      (str ele-content ":"))
      :code-line [:code ele-content]
      :query (query ele-content)
      :youtube-embed (get-youtube-vid-embed ele-content)
      :word-count [:p (word-count block-ds-id conn)]
      :hashtag []
      :url-link []
      :bold [:b ele-content]
      :italic [:i ele-content]
      :highlight [:mark ele-content]
      :strikethrough [:s ele-content])))

(defn ast-ele->hiccup
  [block-ds-id ast-ele conn]
  (cond
    (string? ast-ele) ast-ele
    (= ast-ele :block) :div
    (vector? ast-ele) (hiccup-of-ele block-ds-id ast-ele conn)
    :else ast-ele))

(defn ast->hiccup
  [block-ds-id ast conn]
  (map #(ast-ele->hiccup block-ds-id % conn) ast))

(defn block-content->hiccup
  "Convert Roam markup to Hiccup"
  [block-ds-id content conn]
  (->> content
       parser/parse-to-ast
       (#(ast->hiccup block-ds-id % conn))
       vec))

(parser/parse-to-ast "{{youtube: https://youtu.be/5iI_0wnwIpU}}")

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

(defn new-html-file-titles
  [page-titles]
  (let [page-titles-vec (vec page-titles)]
    (map #(page-title->html-file-title % :case-sensitive) (map second page-titles-vec))))

(defn new-page-hiccup ;; TODO I think this gets replaced with the user-defined HTML template later
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
          (map (fn [r] [:li [:a {:href (str "." (page-title->html-file-title (:block/id (ds/entity @conn (first r))) :case-sensitive))} (second r)]])
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
      [:a {:href (str "." (page-title->html-file-title (:block/id (ds/entity @conn (first (first parent-ds-id)))) :case-sensitive))}
       (:block/hiccup (ds/entity @conn (first (first parent-ds-id))))]
      [:div])))

(defn new-block-page-template
  [block-content conn]
  (let [block-content-text (second block-content)
        block-ds-id (first block-content)]
    [:div
     [:div
      [:h3 (context block-ds-id conn)]]
     [:div
      [:h2 (block-content->hiccup block-ds-id block-content-text conn)]
      (children-of-block-template (:block/id (ds/entity @conn block-ds-id)) conn)]
     [:div {:style "background-color:lightblue;"}
      [:h3 "Linked References"]
      (linked-references block-ds-id conn)]]))

(defn new-page-index-hiccup
  [link-list css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body link-list]])

(defn new-home-page-hiccup
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

(defn- new-list-of-page-links
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
        json-path (unzip-roam-json-archive
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
     (zipmap (new-html-file-titles (sort-by
                                    #(first %)
                                    (ds/q '[:find ?included-id ?block-title
                                            :where
                                            [?included-id :block/included true]
                                            [?included-id :block/id ?block-title]]
                                          @conn)))
             (map #(hiccup/html (new-page-hiccup % "../assets/css/main.css" "../assets/js/extra.js"))
                  (map #(new-block-page-template % conn)
                       (sort-by
                        #(first %)
                        (ds/q '[:find ?included-id ?content
                                :where
                                [?included-id :block/included true]
                                [?included-id :block/content ?content]]
                              @conn)))))
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html (new-page-index-hiccup (new-list-of-page-links (sort (ds/q '[:find ?included-page-title
                                                                                              :where
                                                                                              [?id :block/page true]
                                                                                              [?id :block/included true]
                                                                                              [?id :block/id ?included-page-title]]
                                                                                            @conn)) ".") "../assets/css/main.css" "../assets/js/extra.js"))}
     (str output-dir "/pages"))
    (stasis/export-pages
     {"/index.html" (hiccup/html (new-home-page-hiccup (new-list-of-page-links (sort (ds/q '[:find ?entry-point-content
                                                                                             :where
                                                                                             [?id :block/included true]
                                                                                             [?id :block/entry-point true]
                                                                                             [?id :block/content ?entry-point-content]]
                                                                                           @conn)) "pages" "entry-point-link") (get (site-metadata conn) "Title") "./assets/css/main.css" "./assets/js/extra.js"))}
     output-dir)
    conn))

;; (def conn (-main "/home/thomas/Desktop/RoamExports/robert-public-roam.zip" "." :all))
