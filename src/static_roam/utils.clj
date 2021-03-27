(ns static-roam.utils
  (:require [me.raynes.fs :as fs]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            [clojure.data.json :as json])
  (:import (java.util.zip ZipFile)))


(defn latest-export
  []
  (->> "~/Downloads"
       fs/expand-home
       fs/list-dir
       (filter #(s/includes? (str %) "Roam-Export"))
       (sort-by fs/mod-time)
       last
       str))

;;; TODO timezone correction
(u/def-lazy latest-export-time
  (->> (latest-export)
       fs/base-name
       (re-find #"-(\d*)\.")
       second
       u/coerce-numeric
       java.util.Date.))

(defn unzip-roam-json
  "Takes the path to a zipfile `source` and unzips it to `target-dir`, returning the path of the target file"
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

(defn read-roam-json-from-zip
  [path-to-zip]
  (let [json-path (unzip-roam-json
                   path-to-zip
                   (->> path-to-zip
                        (#(s/split % #"/"))
                        drop-last
                        (s/join "/") (#(str % "/"))))
        roam-json (json/read-str (slurp json-path) :key-fn keyword)]
    roam-json))

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

(defn strip-chars
  "Removes every character of a given set from a string"
  [s removed]
  (reduce str (remove #((set removed) %) s)))

(defn format-hashtag
  [hashtag]
  (if (= \[ (second hashtag))
    (remove-double-delimiters (subs hashtag 1))
    (subs hashtag 1)))

(defn clean-page-title
  [string]
  (-> string
      (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
      (s/replace #"\s" "-")))

(defn page-title->html-file-title
  "Formats a Roam page title as a name for its corresponding HTML page (including '.html' extension)"
  [string]
  {:pre [(have? string? string)]}
  (str (clean-page-title string) ".html"))

;;; TODO hopefully no need for both this and above
(defn html-file-title
  [page-title]
  (page-title->html-file-title page-title))

(def date-formatter
  (java.text.SimpleDateFormat. "dd MMM yyyy hh:mm"))

(defn coerce-time [x]
  (if (inst? x)
    x
    (java.util.Date. x)))

(defn render-time
  [time]
  (.format date-formatter (coerce-time time)))         ;crude for now

