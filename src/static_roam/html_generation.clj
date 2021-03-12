(ns static-roam.html-generation
  (:require [clojure.string :as str-utils]
            [static-roam.utils :as utils]
            [hiccup.core :as hiccup]
            [static-roam.templating :as templating]
            [stasis.core :as stasis]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.edn :as edn-utils]))

(defn- get-path-html-pair
  [path-template-pair]
  (let [path (first path-template-pair)
        html (hiccup/html (second path-template-pair))]
    [path html]))

(defn- get-path-html-map
  [path-template-map]
  (into {} (map get-path-html-pair path-template-map)))

(defn- clean-up-nodes-directory!
  [output-dir]
  ;; This is necessary to cover the case where a block that was previously included is now excluded. Without this, the block URL will still be valid
  (let [nodes-dir-str (str output-dir "/nodes")
        nodes-dir (io/file nodes-dir-str)]
    (doseq [file (.listFiles nodes-dir)]
      (io/delete-file file))))

(defn generate-site
  [filled-out-templates output-dir]
  (clean-up-nodes-directory! output-dir)
  (let [path-html-map (get-path-html-map filled-out-templates)]
    (stasis/export-pages path-html-map output-dir)))
