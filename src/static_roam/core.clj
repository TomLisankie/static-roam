(ns static-roam.core
  (:require [static-roam.utils :as utils]
            [static-roam.database :as database]
            [static-roam.parser :as parser]
            [static-roam.templating :as templating]
            [static-roam.html-generation :as html-gen]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.edn :as edn-utils]))

(defn generate-static-roam!
  [path-to-zip output-dir degree]
  (println "---Loading export---")
  (let [roam-db-conn (time (utils/create-roam-edn-db-from-zip path-to-zip))
        config (edn-utils/read-string (slurp (io/resource "config.edn")))]
    (println "---Determining content to include---")
    (time (database/determine-which-content-to-include roam-db-conn degree config))
    (println "---Parsing---")
    (time (parser/parse-entities-in-db-to-hiccup roam-db-conn))
    (println "---Making templates---")
    (let [templates (time
                     (templating/generate-templates
                      roam-db-conn
                      (:template-info
                       config)))]
      (println "---Generating templates---")
      (time (html-gen/generate-site templates output-dir)))))

(defn -main
  [path-to-zip output-dir degree]
  (generate-static-roam! path-to-zip output-dir (if (nil? (re-find #"\d" degree))
                                                  degree
                                                  (Integer. degree))))
