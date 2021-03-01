(ns static-roam.database-test
  (:require [static-roam.database :refer :all]
            [clojure.test :refer :all]))

(deftest content-refs-test
  ;; Note: Roam will sometimes include .s in hashtags, but it seems inconsistent
  (is (= #{"wizards" "light"}
         ;; This is trick for referring to non-public fn, good to know...
         (#'static-roam.database/content-refs "Do not meddle in the affairs of #wizards, because they become soggy and hard to #light."))))
