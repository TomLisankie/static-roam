(ns static-roam.athens
  (:require [clojure.java.io :as io]
            [datascript.core :as dc]
            [datascript.transit :as dt]
            [org.parkerici.multitool.cljcore :as ju]
            ))

;;; "/Users/mtravers/Documents/athens/index.transit"
(defn read-athens
  [file]
  (let [db (-> file
               io/input-stream
               dt/read-transit)]
    ;; Untried!
    {:schema (:schema db)
     :datoms (dc/datoms db :eavt)}))

(defn dump-athens
  [in out]
  (binding [*print-length* nil]
    (ju/schppit out (read-athens in) )))

(dump-athens "/misc/hyperphor 23/index.transit"
             "broken.edn")
