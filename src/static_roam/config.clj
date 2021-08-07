(ns static-roam.config
  (:require [aero.core :as aero]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            ))

(defmethod aero/reader 'split
  [_ _ [s]]
  (and s (s/split s #",")))

(def the-config (atom {}))

(defn set-config-map
  [m]
  (reset! the-config m))

(defn set-config-path
  [path]
  (set-config-map 
   (aero/read-config
    (io/resource path)))
  (pprint/pprint @the-config))

(defn config
  [& [att]]
  (if att
    (att @the-config)
    @the-config))



