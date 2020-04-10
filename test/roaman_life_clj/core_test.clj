(ns roaman-life-clj.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [roaman-life-clj.core :refer :all]))

(deftest test-unzip-roam-json-archive
  (testing "FIXME, I fail."
    (let [zip-dir "/home/thomas/Dropbox/Roam Exports/"
          zip-name "roam-test-export.zip"
          json-name "thomas.json"]
      (is
       ;; does it unzip to correct path?
       (=
        (str zip-dir json-name)
        (unzip-roam-json-archive (str zip-dir zip-name) zip-dir)))
      (is
       ;; does the file it unzipped actually exist?
       (.exists (io/as-file (str zip-dir json-name)))))))
