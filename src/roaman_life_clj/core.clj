(ns roaman-life-clj.core
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str-utils]
            [clojure.set :as set-fns]
            [hiccup.core :as hiccup]
            [stasis.core :as stasis])
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
             (str-utils/includes? (:string (first (:children post))) "#RoamanPost"))
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

;; 2) STATIC SITE GENERATION

(defn children-list-template
  [blockish indent-level]
  (loop [html []
         children (:children blockish)]
    (if (= (count children) 0)
      html
      (recur (conj html (if (:children (first children))
                          (vec (concat [:ul [:li (:string (first children))]]
                                       (children-list-template (first children) (inc indent-level))))
                          [:ul [:li (:string (first children))]]))
             (rest children)))))

(defn page-template
  [page] ;; each indent level is a new ul. Each element in an indent level is a new li
  ;; (when (= (:title page) "RL Blog Post")
  ;; (json/pprint (:children (last page))))
  (println (children-list-template page 0))
  (vec (concat [:div [:h1 (:title page)]] (children-list-template page 0))))

(defn- strip-chars
  [chars collection]
  (reduce str (remove #((set chars) %) collection)))

(defn- replace-chars
  [char-convert-map collection]
  (reduce str (map #(if (get char-convert-map %) (get char-convert-map %) %))))

(defn format-page-name
  [string]
  (->> string
       (str-utils/lower-case)
       (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
       (replace-chars {\space \-})
       (#(str % ".html"))))

(defn -main
  []
  (let [json-path (unzip-roam-json-archive (str ZIP-DIR ZIP-NAME) ZIP-DIR)
        roam-json (json/read-str (slurp json-path) :key-fn keyword)
        pages-as-rl-json (map to-rl-json roam-json)
        title-to-content-map (zipmap (map #(:title %) pages-as-rl-json) pages-as-rl-json)
        posts (filter #(true? (:post %)) pages-as-rl-json)
        included-pages-to-mentioned-pages-map (zipmap (map #(:title %) posts) (map #(pages-mentioned-by-children % title-to-content-map) (map #(:title %) posts)))
        titles-of-included-pages (find-all-included-pages (map #(:title %) posts) 5 title-to-content-map)
        included-title-to-content-map (zipmap titles-of-included-pages (map #(get title-to-content-map %) titles-of-included-pages))]
    (stasis/export-pages {"/test.html" (hiccup/html (map page-template (filter #(not= nil (:title %)) (vals included-title-to-content-map))))} ".")
    ;; (json/pprint (vals included-title-to-content-map))
    (get included-title-to-content-map "Fitness")
    ))
