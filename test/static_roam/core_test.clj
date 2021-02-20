(ns static-roam.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [static-roam.core :refer :all]
            [static-roam.database :as database]
            [static-roam.utils :as utils]
            [org.parkerici.multitool.core :as u]
            ))

(def test1-block-map
  '{"rXJ2WYFqL"
    {:include? false,
     :edit-time 1609457456508,
     :children (),
     :parent "December 31st, 2020",
     :content "#EntryPoint",
     :refs #{"EntryPoint"},
     :page? false,
     :id "rXJ2WYFqL",
     :heading -1},
    "mJSusMwjf"
    {:include? false,
     :edit-time 1609457408947,
     :children (),
     :parent "December 31st, 2020",
     :content "```And this is a multiline\ncode block```",
     :refs #{},
     :page? false,
     :id "mJSusMwjf",
     :heading -1},
    "87EsDvI3Y"
    {:include? false,
     :edit-time 1609457290335,
     :children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote with some **bold** text",
     :refs #{},
     :page? false,
     :id "87EsDvI3Y",
     :heading -1},
    "OfUnEBdQ9"
    {:include? false,
     :edit-time 1609457263983,
     :children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote",
     :refs #{},
     :page? false,
     :id "OfUnEBdQ9",
     :heading -1},
    "December 31st, 2020"
    {:include? false,
     :edit-time 1609457218846,
     :children
     ("rXJ2WYFqL"
      "W-1MSc0EE"
      "tf_y48ZZ5"
      "OfUnEBdQ9"
      "DRNzZGApK"
      "87EsDvI3Y"
      "GZcQmDYyg"
      "mJSusMwjf"),
     :content "December 31st, 2020",
     :refs #{},
     :page? true,
     :id "December 31st, 2020",
     :heading -1},
    "DRNzZGApK"
    {:include? false,
     :edit-time 1609457273037,
     :children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote\nwith line breaks",
     :refs #{},
     :page? false,
     :id "DRNzZGApK",
     :heading -1},
    "tf_y48ZZ5"
    {:include? false,
     :edit-time 1609457306153,
     :children (),
     :parent "December 31st, 2020",
     :content "This one has some **bold** text",
     :refs #{},
     :page? false,
     :id "tf_y48ZZ5",
     :heading -1},
    "GZcQmDYyg"
    {:include? false,
     :edit-time 1609457384542,
     :children (),
     :parent "December 31st, 2020",
     :content "This has `inline code`",
     :refs #{},
     :page? false,
     :id "GZcQmDYyg",
     :heading -1},
    "EntryPoint"
    {:include? false,
     :edit-time 1609457456519,
     :children (),
     :content "EntryPoint",
     :refs #{},
     :page? true,
     :id "EntryPoint",
     :linked-by ("rXJ2WYFqL"),
     :heading -1},
    "W-1MSc0EE"
    {:include? false,
     :edit-time 1609457245754,
     :children (),
     :parent "December 31st, 2020",
     :content "This is a boring line",
     :refs #{},
     :page? false,
     :id "W-1MSc0EE",
     :heading -1}})

(deftest test-unzip-roam-json-archive
  (testing "read json"
    (let [zip-path "test/resources/static-test.zip"
          roam-json (utils/read-roam-json-from-zip zip-path)]
      (is (vector? roam-json))
      (is (= 2 (count roam-json)))
      (testing "block map"
        (let [block-map (database/setup-static-roam-block-map roam-json)
              clean-block-map (u/map-values #(dissoc % :dchildren) block-map)]
          (is (map? block-map))
          (is (= 10 (count block-map)))
          (is (= test1-block-map clean-block-map))
          )))))


  

