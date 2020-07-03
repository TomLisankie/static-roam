(ns static-roam.templating
  (:require [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [datascript.core :as ds]
            [static-roam.database :as database]))

(defn page-hiccup ;; TODO I think this gets replaced with the user-defined HTML template later
  [body-hiccup css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body body-hiccup]])

(defn children-of-block-template
  [block-id conn]
  [:ul
   [:li (:block/hiccup (ds/entity @conn [:block/id block-id]))]
   (let [children (:block/children (ds/entity @conn [:block/id block-id]))]
     (if (not= 0 (count children))
       ;; recurse on each of the children
       (map #(children-of-block-template % conn) children)
       ;; otherwise, evaluate to empty vector
       [:div]))])

(defn linked-references-template
  [references conn]
  (concat []
          (map
           (fn [r] [:li
                    [:a {:href (str "." (utils/page-title->html-file-title
                                         (:block/id (ds/entity @conn (first r)))
                                         :case-sensitive))}
                     (second r)]])
               references)))

(defn context
  [block-ds-id conn]
  (let [parent-ds-id (ds/q '[:find ?parent-ds-id
                             :in $ ?block-ds-id
                             :where
                             [?block-ds-id :block/id ?block-id]
                             [?parent-ds-id :block/children ?block-id]
                             ]
                           @conn block-ds-id)]
    (if (not (empty? parent-ds-id))
      [:a {:href (str "." (utils/page-title->html-file-title (:block/id (ds/entity @conn (first (first parent-ds-id)))) :case-sensitive))}
       (:block/hiccup (ds/entity @conn (first (first parent-ds-id))))]
      [:div])))

(defn block-page-template
  [block-content conn]
  (let [block-content-text (second block-content)
        block-ds-id (first block-content)]
    [:div
     [:div
      [:h3 (context block-ds-id conn)]]
     [:div
      [:h2 (map parser/ele->hiccup (parser/parse-to-ast block-content))] ;; (block-content->hiccup block-ds-id block-content-text conn)
      (children-of-block-template (:block/id (ds/entity @conn block-ds-id)) conn)]
     [:div {:style "background-color:lightblue;"}
      [:h3 "Linked References"]
      (linked-references-template (database/linked-references block-ds-id conn) conn)]]))

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
   (let [page-titles-vals (map first page-titles)
         page-links (map utils/page-link-from-title page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir]
   (let [page-titles-vals (map first page-titles)
         page-links (map #(utils/page-link-from-title %) page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir link-class]
   (let [page-titles-vals (map first page-titles)
         page-links (map #(utils/page-link-from-title dir % link-class) page-titles-vals)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links)))))
