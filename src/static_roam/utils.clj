(ns static-roam.utils
  (:require [me.raynes.fs :as fs]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [static-roam.config :as config]
            )
  (:import (java.util.zip ZipFile)))

(def last-import (atom nil))

(defn latest
  [dir pattern]
  (->> dir
       fs/expand-home
       fs/list-dir
       (filter (comp (partial re-find (re-pattern pattern)) str))
       (sort-by fs/mod-time)
       last
       (reset! last-import)
       str))

#_
(defn latest-export
  []
  (latest #"Roam-Export"))

;;; TODO timezone correction
;;; Previously got from filename, but this is more general
(defn latest-export-time
  []
  (if @last-import
    (fs/mod-time @last-import)
    (ju/now)))

(defn unzip-roam
  "Takes the path to a zipfile `source` and unzips it to `target-dir`, returning the path of the target file"
  [source]
  (let [target-dir (str (fs/parent source) "/")]
    (str target-dir (with-open [zip (ZipFile. (fs/file source))]
                      (let [entries (enumeration-seq (.entries zip))
                            target-file #(fs/file target-dir (str %))
                            database-file-name (.getName (first entries))]
                        (doseq [entry entries :when (not (.isDirectory ^java.util.zip.ZipEntry entry))
                                :let [f (target-file entry)]]
                          (prn :writing f)
                          (fs/mkdirs (fs/parent f))
                          (io/copy (.getInputStream zip entry) f))
                        database-file-name)))))

(defn read-edn
  [f]
  (edn/read-string (slurp f)))

(defn read-json
  [path]
  (json/read-str (slurp path) :key-fn keyword))

(defn read-roam-json-from-zip
  [path-to-zip]
  (let [json-path (unzip-roam path-to-zip)]
    (read-json json-path)))

(defn write-json [f data]
  (fs/mkdirs (fs/parent f))             ;ensure directory exists
  (with-open [s (io/writer f)]
    (json/write data s)))

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

(defn parse-hashtag
  [hashtag]
  (if (= \[ (second hashtag))
    (remove-double-delimiters (subs hashtag 1))
    (subs hashtag 1)))

(defn clean-page-title
  [string]
  (-> string
      (strip-chars #{\( \) \[ \] \? \! \. \@ \# \$ \% \^ \& \* \+ \= \; \: \" \' \/ \\ \, \< \> \~ \` \{ \}})
      (s/replace #"\s" "-")))

(defn html-file-title
  "Formats a Roam page title as a name for its corresponding HTML page (including '.html' extension)"
  [string]
  {:pre [(have? string? string)]}
  (str (clean-page-title string) ".html"))

(def date-formatter
  (java.text.SimpleDateFormat. "dd MMM yyyy hh:mm"))

;;; TODO fix time input
(defn coerce-time [x]
  (cond (inst? x) x
        (int? x) (java.util.Date. x)
        (string? x) (java.util.Date. x)
        :else x))

(defn render-time
  [time]
  (and time (.format date-formatter (coerce-time time))))         ;crude for now

;;; → multitool
(defmacro debuggable [tag captures & body]
  `(try
     ~@body
     (catch Throwable e#
         (throw (ex-info ~(str "Debuggable ex " tag) ~(zipmap (map keyword captures) captures) e#)))))

(comment
(let [x 23]
  (debuggable
   :test [x]
   (/ x 0)))
)

(defn css-style
  [smap]
  (s/join " " (map (fn [[prop val]] (format "%s: %s;" (name prop) val)) smap)))
  
;;; → Multitool
(defn add-parent
  "Given a db (map of maps), and a multi-valued attribute children-at, compute the single-valued inverse relationship as parent-att"
  [db children-att parent-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (assoc-in acc [child parent-att] key))
                       acc
                       (children-att item)))
             db
             db))

;;; → multitool, surely this must exist?
(defn sconj
  [coll elt]
  (conj (set coll) elt))

;;; As above but multivalued
(defn add-parents
  "Given a db (map of maps), and a multi-valued attribute children-at, compute the multi-valued inverse relationship as parent-att"
  [db children-att parent-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (if (contains? acc child)
                           (update-in acc [child parent-att] sconj key)
                           acc))
                       acc
                       (children-att item)))
             db
             db))



