(ns roaman-life-clj.core
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
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

(defn main
  []
  (let [json-path (unzip-roam-json-archive (str ZIP-DIR ZIP-NAME) ZIP-DIR)
        roam-json (json/read-str (slurp json-path) :key-fn keyword)]
    (first roam-json)))

(main)
