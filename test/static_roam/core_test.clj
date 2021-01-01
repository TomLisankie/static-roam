(ns static-roam.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [static-roam.core :refer :all]
            [static-roam.database :as database]
            [static-roam.utils :as utils]))

(def test1-block-map
  '{"rXJ2WYFqL"
    {:children (),
     :parent "December 31st, 2020",
     :content "#EntryPoint",
     :refers-to #{"EntryPoint"},
     :included true,
     :page false,
     :hiccup [:span [:a {:href "./EntryPoint.html"} "EntryPoint"]],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "mJSusMwjf"
    {:children (),
     :parent "December 31st, 2020",
     :content "```And this is a multiline\ncode block```",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup
     [:span
      [:code "``"]
      "`And this is a multiline\ncode block"
      [:code "``"]
      "`"],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "87EsDvI3Y"
    {:children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote with some **bold** text",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup
     [:span [:blockquote "This one is a quote with some **bold** text"]],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "OfUnEBdQ9"
    {:children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup [:span [:blockquote "This one is a quote"]],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "December 31st, 2020"
    {:children
     ("rXJ2WYFqL"
      "W-1MSc0EE"
      "tf_y48ZZ5"
      "OfUnEBdQ9"
      "DRNzZGApK"
      "87EsDvI3Y"
      "GZcQmDYyg"
      "mJSusMwjf"),
     :parent nil,
     :content "December 31st, 2020",
     :refers-to #{},
     :included true,
     :page true,
     :hiccup [:span "December 31st, 2020"],
     :exit-point false,
     :entry-point true,
     :linked-by #{},
     :heading -1},
    "DRNzZGApK"
    {:children (),
     :parent "December 31st, 2020",
     :content "> This one is a quote\nwith line breaks",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup
     [:span [:blockquote "This one is a quote"] "\nwith line breaks"],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "tf_y48ZZ5"
    {:children (),
     :parent "December 31st, 2020",
     :content "This one has some **bold** text",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup [:span "This one has some " [:b "bold"] " text"],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "GZcQmDYyg"
    {:children (),
     :parent "December 31st, 2020",
     :content "This has `inline code`",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup [:span "This has " [:code "inline code"]],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1},
    "EntryPoint"
    {:children (),
     :parent nil,
     :content "EntryPoint",
     :refers-to #{},
     :included true,
     :page true,
     :hiccup [:span "EntryPoint"],
     :exit-point false,
     :entry-point false,
     :linked-by #{"rXJ2WYFqL"},
     :heading -1},
    "W-1MSc0EE"
    {:children (),
     :parent "December 31st, 2020",
     :content "This is a boring line",
     :refers-to #{},
     :included true,
     :page false,
     :hiccup [:span "This is a boring line"],
     :exit-point false,
     :entry-point false,
     :linked-by #{},
     :heading -1}})

(deftest test-unzip-roam-json-archive
  (testing "read json"
    (let [zip-path "test/resources/static-test.zip"
          roam-json (utils/read-roam-json-from-zip zip-path)]
      (is (vector? roam-json))
      (is (= 2 (count roam-json)))
      (testing "block map"
        (let [block-map (database/setup-static-roam-block-map roam-json nil)]
          (is (map? block-map))
          (is (= 10 (count block-map)))
          (is (= test1-block-map block-map)))))))


  

