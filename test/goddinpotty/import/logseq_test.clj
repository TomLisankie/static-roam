(ns goddinpotty.import.logseq-test
  (:require [clojure.test :refer :all]
            [goddinpotty.import.logseq :as sut]
            [goddinpotty.utils :as utils]
            [goddinpotty.config :as config]
            [org.parkerici.multitool.core :as u]
            ))

;;; These are sort of high-level integration tests, at least proves that the basic machinery is working.

;;; TODO requires test graph be registered with Logseq
(deftest nbb-extract-test
  (let [extract (sut/nbb-extract "logseq-test")]
    (is (seq? extract))
    (is (> (count extract) 7))          ;this is going to grow over time
    (let [block (u/some-thing #(= "A [[page]] link"
                                  (:block/content %))
                              extract)]
      (= 1 (count (:block/refs block))))))

(deftest produce-bm-test
  (let [config (config/read-config "test/logseq-test-config.edn")
        bm (sut/produce-bm config)]
    (is (map? bm))
    (let [block (u/some-thing #(= "A [[page]] link"
                                  (:content %))
                              (vals bm))]
      (is (= #{"page"} (:refs block)))   ;TODO need to figure out how refs work
      (is (= [:block "A " [:page-link "[[page]]"] " link"] (:parsed block))))))

