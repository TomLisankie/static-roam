(ns static-roam.utils
  (:require [me.raynes.fs :as fs]
            [taoensso.truss :as truss :refer (have have! have?)]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.data.json :as json]
            [static-roam.config :as config]
            )
  (:import (java.util.zip ZipFile)))

(defn latest-export
  []
  (->> (config/config :source)
       fs/expand-home
       fs/list-dir
       (filter #(s/includes? (str %) "Roam-Export"))
       (sort-by fs/mod-time)
       last
       str))

;;; TODO timezone correction
(u/def-lazy latest-export-time
  (if-let [export-date (->> (latest-export)
                            fs/base-name
                            (re-find #"-(\d*)\.")
                            second
                            u/coerce-numeric)]
    (java.util.Date. export-date)
    (ju/now)))

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

(defn html-file-title
  "Formats a Roam page title as a name for its corresponding HTML page (including '.html' extension)"
  [string]
  {:pre [(have? string? string)]}
  (str (clean-page-title string) ".html"))

(def date-formatter
  (java.text.SimpleDateFormat. "dd MMM yyyy hh:mm"))

(defn coerce-time [x]
  (if (inst? x)
    x
    (java.util.Date. x)))

(defn render-time
  [time]
  (.format date-formatter (coerce-time time)))         ;crude for now

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
  
