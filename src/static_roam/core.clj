(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]
            [clojure.pprint :as pprint]))

(defn generate-static-roam!
  [path-to-zip output-dir degree]
  (let [roam-json (utils/read-roam-json-from-zip path-to-zip)
        static-roam-block-map (database/setup-static-roam-block-map roam-json degree)]
    (html-gen/generate-static-roam-html static-roam-block-map output-dir)))

(defn -main
  [path-to-zip output-dir degree]
  (generate-static-roam! path-to-zip output-dir (if (nil? (re-find #"\d" degree))
                                                  degree
                                                  (Integer. degree))))
