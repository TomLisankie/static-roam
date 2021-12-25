(ns static-roam.athens
  (:require [clojure.java.io :as io]
            [datascript.core :as dc]
            [datascript.transit :as dt]
            [org.parkerici.multitool.cljcore :as ju]
            ))

;;; Old and undeveloped, not used.
;;; Requires adding to project.clj:
;                 [datascript "1.1.0"]
;                [datascript-transit "0.3.0"]


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

#_
(dump-athens "/misc/hyperphor 23/index.transit"
             "broken.edn")
