(ns static-roam.utils-test
  (:require [clojure.test :refer :all]
            [static-roam.utils :as utils]
            [org.parkerici.multitool.core :as u]
            ))

(deftest add-parent-test
  (let [db {:a {:foo 1 :children [:d]}
            :b {:foo 2}
            :c {:foo 3 :children [:a :b]}
            :d {:foo 4 :children []}
            :e {:foo 5 :children [:x]}
            }]
    (is (= {:a {:foo 1 :children [:d] :parent :c}
            :b {:foo 2 :parent :c}
            :c {:foo 3 :children [:a :b]}
            :d {:foo 4 :children [] :parent :a}
            :e {:foo 5 :children [:x]}
            :x {:parent :e}
            }
           (utils/add-parent db :children :parent)))))

(deftest add-parents-test
  (let [db {:a {:foo 1 :children [:b :c]}
            :b {:foo 2 :children [:c]}
            :c {:foo 3 :children [:d]}
            :d {:foo 4}
            :e {:foo 5 :children [:x] }
            }]
    (is (= {:a {:foo 1 :children [:b :c]}
            :b {:foo 2 :children [:c] :parent #{:a}}
            :c {:foo 3 :children [:d] :parent #{:a :b}}
            :d {:foo 4 :parent #{:c}}
            :e {:foo 5 :children [:x]}}
            (utils/add-parents db :children :parent)))))


 





