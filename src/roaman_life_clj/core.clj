(ns roaman-life-clj.core
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str-utils]
            [clojure.set :as set-fns]
            [hiccup.core :as hiccup]
            [stasis.core :as stasis]
            [markdown-to-hiccup.core :as mdh])
  (:import (java.util.zip ZipFile)))

(def ZIP-DIR "/home/thomas/Dropbox/Roam Exports/")
(def ZIP-NAME "roam-test-export.zip")

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
  ;; is this Roam page a entry-point? Is it tagged as such in its first block?
  [entry-point]
  (if (= (count (:children entry-point)) 0)
    false
    (if (and (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children entry-point))))
             (str-utils/includes?
              (:string (first (:children entry-point))) "#EntryPoint"))
      true
      false)))

(defn to-rl-json
  ;; strips Roam JSON of unneeded info and adds relevant info
  [entry-point]
  {:title (:title entry-point)
   :entry-point (entry-point? entry-point)
   :date (if (entry-point? entry-point)
           (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children entry-point))))
           nil)
   :children (if (entry-point? entry-point)
               (rest (:children entry-point))
               (:children entry-point))})

(defn title-content-pair
  [page]
  [(:title page) page])

(defn remove-n-surrounding-delimiters
  [n string]
  (subs string n (- (count string) n)))

(defn remove-double-delimiters
  [string]
  (remove-n-surrounding-delimiters 2 string))

(defn remove-triple-delimiters
  [string]
  (remove-n-surrounding-delimiters 3 string))

(defn get-pages-referenced-in-string
  [string]
  (concat
   (map remove-double-delimiters (re-seq #"\[\[.*?\]\]" string))
   (map #(subs % 1) (re-seq #"\#..*?(?=\s|$)" string))
   (map
    #(subs % 0 (- (count %) 2))
    (re-seq #"^.+?::" string))))

(defn get-blocks-referenced-in-string
  [string]
  (map remove-double-delimiters (re-seq #"\(\(.*?\)\)" string)))

(defn pages-mentioned-by-children
  [entry-point-title page-to-content-map]
  (when (:children (get page-to-content-map entry-point-title))
    (set
     (flatten
      (map get-pages-referenced-in-string
           (map second
                (map first
                     (tree-seq #(:children %)
                               #(:children %)
                               (get page-to-content-map entry-point-title)))))))))

(defn find-all-included-pages
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

(defn children-id-content-map
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
         (children-id-content-map (first children))])))))

;; 2) STATIC SITE GENERATION

(defn- strip-chars
  [chars collection]
  (reduce str (remove #((set chars) %) collection)))

(defn page-title->html-file-title
  ([string]
   (->> string
        (str-utils/lower-case)
        (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
        (#(str-utils/replace % #"\s" "-"))
        (#(str "/" % ".html"))))
  ([string case-sensitive?]
   (->> string
        (#(if case-sensitive?
            %
            (str-utils/lower-case %)))
        (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
        (#(str-utils/replace % #"\s" "-"))
        (#(str "/" % ".html")))))

(defn double-brackets->links
  [string block-id-content-map]
  (let [double-brackets-replaced (str-utils/replace
                                  string
                                  #"\[\[.*?\]\]"
                                  #(str "[" (remove-double-delimiters %)
                                        "](." (page-title->html-file-title %) ")"))
        hashtags-replaced (str-utils/replace
                           double-brackets-replaced
                           #"\#..*?(?=\s|$)"
                           #(str "[" (subs % 1) "](." (page-title->html-file-title %) ")"))
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
                                #(str
                                  (get block-id-content-map
                                       (remove-double-delimiters %) "BLOCK NOT FOUND")
                                  "  [Block Link](."
                                  (page-title->html-file-title % :case-sensitive) ")"))
        metadata-replaced (str-utils/replace
                           block-refs-transcluded
                           #"^.+?::"
                           #(str
                             "__[" (subs % 0 (- (count %) 2)) ":](."
                             (page-title->html-file-title %) ")__"))]
    (if (or
         (re-find #"\[\[.*?\]\]" metadata-replaced)
         (re-find #"\#..*?(?=\s|$)" metadata-replaced)
         (re-find #"\(\(.*?\)\)" metadata-replaced)
         (re-find #"^.+?::" metadata-replaced))
      (double-brackets->links metadata-replaced block-id-content-map)
      metadata-replaced)))

(defn roam-md->hiccup
  [string block-id-content-map]
  (->>
   string
   (#(double-brackets->links % block-id-content-map))
   mdh/md->hiccup
   mdh/component))

(defn children-list-template
  [blockish indent-level block-id-content-map]
  (loop [html []
         children (:children blockish)]
    (if (= (count children) 0)
      html
      (recur (conj html (if (:children (first children))
                          (vec
                           (concat
                            [:ul
                             {:style "list-style-type: none"}
                             [:li (roam-md->hiccup
                                   (:string (first children))
                                   block-id-content-map)]]
                            (children-list-template
                             (first children)
                             (inc indent-level)
                             block-id-content-map)))
                          [:ul
                           [:li (roam-md->hiccup
                                 (:string (first children))
                                 block-id-content-map)]]))
             (rest children)))))

(defn page-template
  [page block-id-content-map] ;; each indent level is a new ul. Each element in an indent level is a new li
  (vec
   (concat
    [:div
     [:title (:title page)]
     [:h1 (:title page)]]
    (children-list-template page 0 block-id-content-map))))

(defn block-page-template
  [block-string block-id-content-map] ;; each indent level is a new ul. Each element in an indent level is a new li
  (vec
   (concat
    [:div
     [:h3 (roam-md->hiccup block-string block-id-content-map)]])))

(defn html-file-titles
  ([page-titles]
   (map page-title->html-file-title page-titles))
  ([page-titles case-sensitive?]
   (map #(page-title->html-file-title % case-sensitive?) page-titles)))

(defn- page-link-from-title
  ([dir page]
   [:a {:href (str dir (page-title->html-file-title (:title page)))} (:title page)])
  ([page]
   [:a {:href (page-title->html-file-title (:title page))} (:title page)])
  ([dir page link-class]
   [:a {:class link-class
        :href (str dir (page-title->html-file-title (:title page)))}
    (:title page)]))

(defn- list-of-page-links
  [links]
  (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) links)))

(defn home-page-hiccup
  [link-list title]
  [:html
   [:head
    [:title title]
    [:link {:rel "stylesheet" :href "./main.css"}]]
   [:body
    [:header.site-header {:role "banner"}
     [:div.wrapper
      [:a.site-title {:rel "author" :href "."} title]]]
    [:main.page-content {:aria-label="Content"}
     [:div.wrapper
      [:div.home
       [:h2.post-list-heading "Entry Points"]
       link-list]]]]])

(defn page-index-hiccup
  [link-list]
  [:html
   [:head
    [:link {:rel "stylesheet" :href "../main.css"}]]
   [:body
    link-list]])

(defn page-hiccup
  [link-list]
  [:html
   [:head
    [:link {:rel "stylesheet" :href "../main.css"}]]
   [:body
    link-list]])

(defn -main
  []
  (let [json-path (unzip-roam-json-archive (str ZIP-DIR ZIP-NAME) ZIP-DIR)
        roam-json (json/read-str (slurp json-path) :key-fn keyword)
        pages-as-rl-json (map to-rl-json roam-json)
        title-to-content-map (zipmap (map #(:title %) pages-as-rl-json) pages-as-rl-json)
        entry-points (filter #(true? (:entry-point %)) pages-as-rl-json)
        included-pages-to-mentioned-pages-map (zipmap
                                               (map #(:title %) entry-points)
                                               (map
                                                #(pages-mentioned-by-children
                                                  % title-to-content-map)
                                                (map #(:title %) entry-points)))
        titles-of-included-pages (find-all-included-pages
                                  (map #(:title %) entry-points)
                                  5 title-to-content-map)
        included-title-to-content-map (zipmap
                                       titles-of-included-pages
                                       (map
                                        #(get title-to-content-map %)
                                        titles-of-included-pages))
        block-id-to-content-map (into {}
                                      (map children-id-content-map pages-as-rl-json))
        mentioned-block-id-to-content-map (into {}
                                                (map
                                                 children-id-content-map
                                                 (vals included-title-to-content-map)))]
    (stasis/export-pages
     (zipmap (html-file-titles (keys included-title-to-content-map))
             (map #(hiccup/html (page-hiccup %))
                  (map
                   #(page-template % block-id-to-content-map)
                   (vals included-title-to-content-map))))
     "./pages")
    (stasis/export-pages
     (zipmap (html-file-titles (keys mentioned-block-id-to-content-map) :case-sensitive)
             (map #(hiccup/html (page-hiccup %))
                  (map
                   #(block-page-template % block-id-to-content-map)
                   (vals mentioned-block-id-to-content-map))))
     "./pages")
    (stasis/export-pages
     {"/index.html" (hiccup/html (page-index-hiccup (list-of-page-links (map #(page-link-from-title "." %) (filter #(not= nil %) (vals included-title-to-content-map))))))}
     "./pages")
    (stasis/export-pages
     {"/index.html" (hiccup/html (home-page-hiccup (list-of-page-links (map #(page-link-from-title "pages" % "entry-point-link") (filter #(:entry-point %) (vals included-title-to-content-map)))) "Part Of My Second Brain"))}
     ".")
    block-id-to-content-map))

(time (-main)) ;; currently around 595 msecs
(-main)
(get (-main) "rAhLBlh68")

