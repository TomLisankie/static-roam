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

(defn post?
  ;; is this Roam page a post? Is it tagged as such in its first block?
  [post]
  (if (= (count (:children post)) 0)
    false
    (if (and (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children post))))
             (str-utils/includes? (:string (first (:children post))) "#EntryPoint"))
      true
      false)))

(defn to-rl-json
  ;; strips Roam JSON of unneeded info and adds relevant info
  [post]
  {:title (:title post)
   :post (post? post)
   :date (if (post? post)
           (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children post))))
           nil)
   :children (if (post? post) (rest (:children post)) (:children post))})

(defn title-content-pair
  [page]
  [(:title page) page])

(defn remove-double-delimiters
  [string]
  (subs string 2 (- (count string) 2)))

(defn get-pages-referenced-in-string
  [string]
  (concat (map remove-double-delimiters (re-seq #"\[\[.*?\]\]" string)) (map #(subs % 1) (re-seq #"\#..*?(?=\s|$)" string))))

(defn get-blocks-referenced-in-string
  [string]
  (map remove-double-delimiters (re-seq #"\(\(.*?\)\)" string)))

(defn pages-mentioned-by-children
  [post-title page-to-content-map]
  ;; needs to recursively visit children
  (when (:children (get page-to-content-map post-title))
    (set (flatten (map get-pages-referenced-in-string (map second (map first (tree-seq #(:children %) #(:children %) (get page-to-content-map post-title)))))))))

(defn find-all-included-pages
  [unexplored-posts max-depth page-to-content-map]
  (loop [explored #{}
         unexplored (set unexplored-posts)
         current-depth 0]
    ;; (json/pprint unexplored)
    (if (> current-depth max-depth)
      explored
      (recur (set-fns/union unexplored explored) (reduce set-fns/union (map #(pages-mentioned-by-children % page-to-content-map) unexplored)) (inc current-depth)))))

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
        [{(:uid (first children)) (:string (first children))} (children-id-content-map (first children))])))))

;; 2) STATIC SITE GENERATION

(defn- strip-chars
  [chars collection]
  (reduce str (remove #((set chars) %) collection)))

(defn page-title->html-file-title
  [string]
  (->> string
       (str-utils/lower-case)
       (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
       (#(str-utils/replace % #"\s" "-"))
       (#(str "/" % ".html"))))

(defn double-brackets->links
  [string block-id-content-map]
  (let [double-brackets-replaced (str-utils/replace
                                  string
                                  #"\[\[.*?\]\]"
                                  #(str "[" (remove-double-delimiters %) "](." (page-title->html-file-title %) ")"))
        hashtags-replaced (str-utils/replace
                           double-brackets-replaced
                           #"\#..*?(?=\s|$)"
                           #(str "[" (subs % 1) "](." (page-title->html-file-title %) ")"))
        block-refs-transcluded (str-utils/replace
                                hashtags-replaced
                                #"\(\(.*?\)\)"
                                #(get block-id-content-map (remove-double-delimiters %) "BLOCK NOT FOUND"))]
    block-refs-transcluded
    (if (or
         (re-find #"\[\[.*?\]\]" block-refs-transcluded)
         (re-find #"\#..*?(?=\s|$)" block-refs-transcluded)
         (re-find #"\(\(.*?\)\)" block-refs-transcluded))
      (double-brackets->links block-refs-transcluded block-id-content-map)
      block-refs-transcluded)))

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
  ;; (when (= (:title page) "RL Blog Post")
  ;; (json/pprint (:children (last page))))
  ;; (println (children-list-template page 0))
  (vec
   (concat
    [:div
     [:title (:title page)]
     [:h1 (:title page)]]
    (children-list-template page 0 block-id-content-map))))

(defn html-file-titles
  [page-titles]
  (map page-title->html-file-title page-titles))

(defn- page-link-from-title
  ([dir page]
   [:a {:href (str dir (page-title->html-file-title (:title page)))} (:title page)])
  ([page]
   [:a {:href (page-title->html-file-title (:title page))} (:title page)])
  ([dir page link-class]
   [:a {:class link-class :href (str dir (page-title->html-file-title (:title page)))} (:title page)]))

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
        posts (filter #(true? (:post %)) pages-as-rl-json)
        included-pages-to-mentioned-pages-map (zipmap (map #(:title %) posts) (map #(pages-mentioned-by-children % title-to-content-map) (map #(:title %) posts)))
        titles-of-included-pages (find-all-included-pages (map #(:title %) posts) 5 title-to-content-map)
        included-title-to-content-map (zipmap titles-of-included-pages (map #(get title-to-content-map %) titles-of-included-pages))
        block-id-to-content-map (into {} (map children-id-content-map pages-as-rl-json))]
    (stasis/export-pages ;; creates a map from file title to HTML for that file title
     (zipmap (html-file-titles (keys included-title-to-content-map))
             ;; supposed to generate HTML for each included page of the db
             (map #(hiccup/html (page-hiccup %)) ;; Makes all of the hiccup to standard HTML for pages
                  (map
                   #(page-template % block-id-to-content-map) ;; this is the problem line. Something is wrong with page template or block-id-to-content-map
                   (vals included-title-to-content-map))))
     "./pages")
    (stasis/export-pages
     {"/index.html" (hiccup/html (page-index-hiccup (list-of-page-links (map #(page-link-from-title "." %) (filter #(not= nil %) (vals included-title-to-content-map))))))}
     "./pages")
    (stasis/export-pages
     {"/index.html" (hiccup/html (home-page-hiccup (list-of-page-links (map #(page-link-from-title "pages" % "post-link") (filter #(:post %) (vals included-title-to-content-map)))) "Part Of My Second Brain"))}
     ".")
    block-id-to-content-map))

(time (-main)) ;; currently around 595 msecs
(-main)
(get (-main) "rAhLBlh68")

