(ns goddinpotty.config
  (:require [aero.core :as aero]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            ))

;;; For config specifics, see resources/default-config.edn

(defmethod aero/reader 'split
  [_ _ [s]]
  (and s (s/split s #",")))

(def the-config (atom {}))

(defn set-config-map!
  [m]
  (reset! the-config m)
  (pprint/pprint @the-config))

(defn set-config-path!
  [path]
   (if-let [resource (io/resource path)]
     (set-config-map! (aero/read-config resource))
     (throw (ex-info "Config not found" {:resource path}))))

(defn config
  [& [att]]
  (if att
    (att @the-config)
    @the-config))



