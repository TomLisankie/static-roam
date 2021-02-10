(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.parser :as parser]
            [static-roam.templating :as templating]
            [static-roam.html-generation :as html-gen]
            [clojure.pprint :as pprint]))

(defn generate-static-roam!
  [path-to-zip output-dir degree]
  (let [roam-db-conn (utils/create-roam-edn-db-from-zip path-to-zip)
        config (edn-utils/read-string (slurp (io/resource "config.edn")))]
    (database/determine-which-content-to-include roam-db-conn degree config)
    (parser/parse-entities-in-db-to-hiccup roam-db-conn)
    (html-gen/generate-site (templating/generate-templates roam-db-conn (:template-info config)) output-dir)))

(defn -main
  [path-to-zip output-dir degree]
  (generate-static-roam! path-to-zip output-dir (if (nil? (re-find #"\d" degree))
                                                  degree
                                                  (Integer. degree))))
