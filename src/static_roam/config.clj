(ns static-roam.config
  (:require [aero.core :as aero]
            [clojure.string :as s]
            [clojure.java.io :as io]
            ))

(defmethod aero/reader 'split
  [_ _ [s]]
  (and s (s/split s #",")))

(def the-config (atom {}))

(defn set-config
  [path]
  (reset! the-config
          (aero/read-config
           (io/resource path))))

(defn config
  [& [att]]
  (if att
    (att @the-config)
    @the-config))



