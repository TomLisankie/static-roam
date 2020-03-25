(ns roaman-life-clj.core
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as str-utils])
  (:import (java.util.zip ZipFile)))

(def ZIP-DIR "/home/thomas/Dropbox/Roam Exports/")
(def ZIP-NAME "roam-test-export.zip")

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
  [post]
  (if (= (count (:children post)) 0)
    false
    (if (and (re-find #"\d{2}/\d{2}/\d{4}" (:string (first (:children post))))
             (str-utils/includes? (:string (first (:children post))) "#RoamanPost"))
      true
      false)))

(defn to-rl-json
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
  (set (map remove-double-delimiters (re-seq #"\[\[.*?\]\]" string))))

(defn get-pages-referenced-in-block
  [block]
  )

(defn get-pages-to-include
  [start-pages depth-degree max-degree]
  )

(defn main
  []
  (let [json-path (unzip-roam-json-archive (str ZIP-DIR ZIP-NAME) ZIP-DIR)
        roam-json (json/read-str (slurp json-path) :key-fn keyword)
        pages-as-rl-json (map to-rl-json roam-json)
        title-to-content-map (zipmap (map #(:title %) pages-as-rl-json) pages-as-rl-json)
        posts (filter #(true? (:post %)) pages-as-rl-json)]
    posts))
