(ns goddinpotty.database-test
  (:require [goddinpotty.database :refer :all]
            [goddinpotty.parser :as parser]
            [clojure.test :refer :all]))

(deftest content-refs-test
  (is (= #{"wizards" "light"}
         ;; This is trick for referring to non-public fn, good to know...
         (#'block-refs {:parsed (parser/parse-to-ast "Do not meddle in the affairs of #wizards, because they become soggy and hard to #light.")} {}  ))))
