(ns static-roam.templating
  (:require [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]
            [clojure.pprint :as pprint]))

(defn page-hiccup
  [body-hiccup site-title nav-bar-page-dict head-extra]
  [:html
   `[:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title ~site-title]
     ~@head-extra]
   [:body
    [:header.site-header
     [:div.wrapper
      [:a.site-title {:rel "author" :href ".."} site-title]
      [:nav.navbar.navbar-expand-lg.navbar-dark ; .bg-dark.navbar-dark
       (into
       [:ul.navbar-nav.mr-auto]
       (map (fn [[url title]] [:li.nav-item 
                               [:a.nav-link {:href url}
                                title
                                ]])
            nav-bar-page-dict))]]]
    [:div.container.main
     body-hiccup]]])

(defn children-of-block-template
  [block-id block-map]
  (let [properties (database/get-properties-for-block-id block-id block-map)]
    [:ul
     (if (or (nil? (:hiccup properties)) (= (:content properties) block-id))
       ""
       ;;; TODO Turned off block clicks, may want it under a flag or something
       [:li.block #_ {:onclick (str "location.href='" (utils/page-title->html-file-title block-id :case-sensitive) "'")}
        (:hiccup properties)])
     (let [children (:children properties)]
       (if (not= 0 (count children))
         ;; recurse on each of the children
         (map #(children-of-block-template % block-map) children)
         ;; otherwise, evaluate to empty div
         [:div]))]))

;;; TODO does this find page, or intermediate parents?
(defn- find-parent
  [block-id block-map]
                                        ; (first (filter #(not (nil? %)) (map #(is-parent block-id %) block-map)))
  (get-in block-map [block-id :parent]))

(defn- find-page
  [block-id block-map]
  (if-let [parent (find-parent block-id block-map)]
    (find-page parent block-map)
    block-id))

(defn- get-parent
  [block-id block-map]
  (let [parent-id (find-parent block-id block-map)]
    (if (nil? parent-id)
      ""
      [:a {:href (str ""
                      (utils/page-title->html-file-title
                       parent-id
                       :case-sensitive))}
       (:content (get block-map parent-id))])))

(defn page-link [page]
  [:a {:href (utils/page-title->html-file-title page :case-sensitive)}
   (parser/block-content->hiccup page {})]) ;TODO ugly but does the italic thing right

(defn linked-reference-template
  [block-map r]
  (if (nil? (get block-map r))
    ""                                  ;TODO ugly
    (let [parent (find-page r block-map)
          link (utils/page-title->html-file-title (or parent r) :case-sensitive)]
      [:div
       "From " (page-link parent)
       [:div (children-of-block-template r block-map)]])))

(defn linked-references-template
  [references block-map]
  (concat []
          (map (partial linked-reference-template block-map) references)))

(defn block-page-template
  [block-id block-map]
  (let [block-content (get-in block-map [block-id :content])]
    [:div
     [:div
      [:h3 (get-parent block-id block-map)]]
     [:h2.title (vec (map #(parser/ele->hiccup % block-map) (parser/parse-to-ast block-content)))]
     [:div (children-of-block-template block-id block-map)]
     (let [linked-refs (database/get-linked-references block-id block-map)]
       (when-not (empty? linked-refs)
         [:div.incoming
          [:h3 "Incoming links"]
          (linked-references-template linked-refs block-map)]))]))

(defn page-index-hiccup
  [link-list css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body link-list]])

(defn home-page-hiccup
  [link-list title nav-bar-page-dict css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:title title]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body
    [:header.site-header                ;TODO make this work or gegt rid
     [:div.wrapper
      [:a.site-title {:rel "author" :href "."} title]
      [:nav.navbar.navbar-expand-lg ; .bg-dark.navbar-dark
       (into
        [:ul.navbar-nav.mr-auto]
        (map (fn [pair] [:a.nav-link {:href (str "./pages" (subs (first pair) 1))} (second pair)]) nav-bar-page-dict))]]]
    [:main.page-content {:aria-label "Content"}
     [:div.wrapper
       [:h2.post-list-heading "Entry Points"]
       link-list]]]])

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
