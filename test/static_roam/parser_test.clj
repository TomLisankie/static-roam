(ns static-roam.parser-test
  (:require [static-roam.parser :refer :all]
            [clojure.test :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= [:block [:page-link "Physics"] " " [:hashtag "Static-Roam"]]
           (parse-to-ast "[[Physics]] #Static-Roam")
           ))))
