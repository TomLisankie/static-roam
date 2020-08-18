(defproject static-roam "0.0.17-alpha"
  :description "A static-site generator for Roam Research"
  :url "https://github.com/TomLisankie/static-roam"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/data.json "1.0.0"]
                 [hiccup "1.0.5"]
                 [stasis "2.5.0"]
                 [instaparse "1.4.10"]]
  :resource-paths ["resources"]
  :main static-roam.core)
