(defproject static-roam "2.0.1"
  :description "A static-site generator for Roam Research"
  :url "https://github.com/mtravers/static-roam"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/data.json "2.0.2"]
                 [org.clojure/data.xml "0.2.0-alpha6"] ;just for blogger
;                 [org.clojars.simon_brooke/html-to-md "0.3.0"] 
;; Not suitable, it only renders html
;;                 [markdown-clj "1.10.7"]
;; Used by logseq-from-md, not there yet
;;                 [org.commonmark/commonmark "0.18.0"]
                 [html-to-md/html-to-md "0.3.0"]
                 [org.parkerici/multitool "0.0.17"] ;TODO publish this version
                 [com.taoensso/truss "1.6.0"]
                 [hiccup "1.0.5"]
                 [instaparse "1.4.10"]
                 [aero "1.1.6"]
                 [metasoarous/oz "1.6.0-alpha6"]
                 [mock-clj "0.2.1"]     ;TODO should be under :test profile
                 ]
  :resource-paths ["resources"]
  :main static-roam.core)
