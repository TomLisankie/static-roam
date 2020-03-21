(ns roaman-life-clj.core)

(def ZIP-DIR "/home/thomas/Dropbox/Roam Exports/")
(def ZIP-NAME "roam-test-export.zip")

(defn ^File file
  "If `path` is a period, replaces it with cwd and creates a new File object
   out of it and `paths`. Or, if the resulting File object does not constitute
   an absolute path, makes it absolutely by creating a new File object out of
   the `paths` and cwd."
  [path & paths]
  (when-let [path (apply
                   io/file (if (= path ".")
                             *cwd*
                             path)
                   paths)]
    (if (.isAbsolute ^File path)
      path
      (io/file *cwd* path))))

(defn unzip
  "Takes the path to a ZIP file `source` and unzips it to `target-dir`"
  [source target-dir]
  (with-open [])
  )
