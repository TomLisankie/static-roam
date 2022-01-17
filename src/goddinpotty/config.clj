(ns goddinpotty.config
  (:require [aero.core :as aero]
            [clojure.string :as s]
            [me.raynes.fs :as fs]
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
   (if-let [resource (io/resource path)]
     (do
       (aero/read-config resource)
       (pprint/pprint @the-config))
     (throw (ex-info "Config found" {:resource path})))))

(defn config
  [& [att]]
  (if att
    (att @the-config)
    @the-config))



