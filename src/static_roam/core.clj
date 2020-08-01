(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.html-generation :as html-gen]))

(defn generate-static-roam!
  [path-to-zip output-dir degree]
  (let [roam-json (utils/read-roam-json-from-zip path-to-zip)
        database-connection (database/setup-static-roam-db roam-json degree)]
    (html-gen/generate-static-roam-html database-connection output-dir)))

(defn -main
  [path-to-zip output-dir degree]
  (generate-static-roam! path-to-zip output-dir degree))
