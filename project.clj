(defproject static-roam "0.0.22-alpha"
  :description "A static-site generator for Roam Research"
  :url "https://github.com/mtravers/static-roam"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [me.raynes/fs "1.4.6"]
                 [org.parkerici/multitool "0.0.11"]
                 [org.clojure/data.json "1.0.0"]
                 [com.taoensso/truss "1.6.0"]
                 [hiccup "1.0.5"]
                 [stasis "2.5.0"]
                 [instaparse "1.4.10"]
                 [metasoarous/oz "1.6.0-alpha6"]]
  :resource-paths ["resources"]
  :main static-roam.core)
