(defproject static-roam "0.0.22-alpha"
  :description "A static-site generator for Roam Research"
  :url "https://github.com/mtravers/static-roam"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [me.raynes/fs "1.4.6"]
                 [org.parkerici/multitool "0.0.13"]
                 [org.clojure/data.json "1.1.0"]
                 [com.taoensso/truss "1.6.0"]
                 [hiccup "1.0.5"]
                 [stasis "2.5.0"]
                 [instaparse "1.4.10"]
                 [aero "1.1.6"]
                 [metasoarous/oz "1.6.0-alpha6"]
                 [mock-clj "0.2.1"]     ;TODO should be under :test profile
                 ]
  :resource-paths ["resources"]
  :main static-roam.core)
