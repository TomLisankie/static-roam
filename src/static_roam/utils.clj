(ns static-roam.utils
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str-utils])
  (:import (java.util.zip ZipFile)))

(defn unzip-roam-json-archive
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
        (#(str "./" % ".html")))))

(defn html-file-titles
  [page-titles]
  (let [page-titles-vec (vec page-titles)]
    (map #(subs (page-title->html-file-title % :case-sensitive) 1) (map second page-titles-vec))))

(defn page-link-from-title
  "Given a page and a directory for the page to go in, create Hiccup that contains the link to the HTML of that page"
  ([dir block-content]
   [:a {:href (str dir (page-title->html-file-title block-content :case-sensitive))} block-content])
  ([block-content]
   [:a {:href (page-title->html-file-title block-content :case-sensitive)} block-content])
  ([dir block-content link-class]
   [:a {:class link-class
        :href (str dir (subs (page-title->html-file-title block-content :case-sensitive) 1))}
    block-content]))
