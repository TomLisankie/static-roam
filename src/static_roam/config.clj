(ns static-roam.config
  (:require [aero.core :as aero]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [org.parkerici.multitool.core :as u]
  ))

(defn merge-recursive [m1 m2]
  (cond (and (map? m1) (map? m2))
        (merge-with merge-recursive m1 m2)
;        (and (sequential? m1) (sequential? m2))
;        (map merge-recursive m1 m2)
        (nil? m2) m1
        :else m2))

;; TODO Not sure why need this and resources/config.edn
(def default-config
  {:site-css ["../assets/hyper-roam.css"]
   ;; Some of these want to be customized by extension, not replacement

   ;; this group decides what is tagged :include? or not
   :entry-tags ["EntryPoint"]
   :exit-tags ["ExitPoint" "Private"] 
   :daily-logs false

   :unexclude? false                     ;TODO Show everything, indicating graphically if it is included in regular version.

   :dev-mode false                      ;true turns on links into Roam itself
   :output-dir "output"                 ;TODO should be output-path
   :bullets false                       ;TODO not hooked up
   }
  )

(defmethod aero/reader 'split
  [_ _ [s]]
  (and s (s/split s #",")))

(def config
  (merge-recursive
   default-config
   (aero/read-config
    (io/resource "config.edn"))))



