(ns static-roam.templating
  (:require [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]
            [clojure.pprint :as pprint]))

(defn page-hiccup ;; TODO I think this gets replaced with the user-defined HTML template later
  [body-hiccup css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body body-hiccup]])

(defn children-of-block-template
  [block-id block-map]
  (let [properties (database/get-properties-for-block-id block-id block-map)]
    [:ul
     [:li (:hiccup properties)]
     (let [children (:children properties)]
       (if (not= 0 (count children))
         ;; recurse on each of the children
         (map #(children-of-block-template % block-map) children)
         ;; otherwise, evaluate to empty div
         [:div]))]))

(defn linked-references-template
  [references block-map]
  (concat []
          (map
           (fn
             [r]
             [:li
              [:a
               {:href
                (str ""
                     (utils/page-title->html-file-title
                      r
                      :case-sensitive))}
               (:content (get block-map r))]])
           references)))

(defn- is-parent
  [block-id block-kv]
  (if (some #(= block-id %) (:children (second block-kv)))
    (first block-kv)
    nil))

(defn- find-parent
  [block-id block-map]
  (filter #(not (nil? %)) (map #(is-parent block-id %) block-map)))

(defn- get-parent
  [block-id block-map]
  (let [parent-id (first (find-parent block-id block-map))]
    (if (nil? parent-id)
      ""
      parent-id)))

(defn block-page-template
  [block-id block-content block-map]
  [:div
   [:div
    [:h3 (get-parent block-id block-map)]]
   [:h2 (vec (map #(parser/ele->hiccup % block-map) (parser/parse-to-ast block-content)))]
   [:div (children-of-block-template block-id block-map)]
   [:div {:style "background-color:lightblue;"}
    [:h3 "Linked References"]
    (linked-references-template (database/get-linked-references block-id block-map) block-map)]])

(defn page-index-hiccup
  [link-list css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body link-list]])

(defn home-page-hiccup
  [link-list title css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:title title]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body
    [:header.site-header {:role "banner"}
     [:div.wrapper
      [:a.site-title {:rel "author" :href "."} title]]]
    [:main.page-content {:aria-label="Content"}
     [:div.wrapper
      [:div.home
       [:h2.post-list-heading "Entry Points"]
       link-list]]]]])

(defn list-of-page-links
  "Generate a Hiccup unordered list of links to pages"
  ([page-titles]
   (let [page-links (map utils/page-link-from-title page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir]
   (let [page-links (map #(utils/page-link-from-title %) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir link-class]
   (let [page-links (map #(utils/page-link-from-title dir % link-class) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links)))))
