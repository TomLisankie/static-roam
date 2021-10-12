(ns static-roam.athens
  (:require [clojure.java.io :as io]
            [datascript.core :as dc]
            [datascript.transit :as dt]))

;;; "/Users/mtravers/Documents/athens/index.transit"
(defn read-athens
  [file]
  (let [db (-> file
               io/input-stream
               dt/read-transit)]
    ;; Untried!
    {:schema (:schema db)
     :datoms (dc/datoms db :eavt)}))
