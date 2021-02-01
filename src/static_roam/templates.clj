(ns static-roam.templates)

(defn about-template
  [roam-db])

(defn contact-template
  [roam-db])

(defn graph-page-template
  [roam-db])

(defn homepage-template
  [roam-db])

(defn mind-template
  [roam-db])

(defn now-template
  [roam-db])

(defn post-template
  [roam-db])

(defn posts-index-template
  [roam-db])

(def template-fns
  {"about"
   about-template
   "contact"
   contact-template
   "graph-page-example"
   graph-page-template
   "index"
   homepage-template
   "mind"
   mind-template
   "now"
   now-template
   "post-example"
   post-template
   "posts"
   posts-index-template
   })
