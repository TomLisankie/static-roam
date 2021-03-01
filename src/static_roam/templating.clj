(ns static-roam.templating
  (:require [clojure.string :as s]
            [static-roam.config :as config]
            [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]
            [org.parkerici.multitool.core :as u]
            ))

;;; TODO Note: the functions of templating and html-gen seem to overlap; not sure they should be separate.

(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(s/split % #":: ") metadata))))

(defn- site-metadata
  [block-map]
  (let [property-block-ids (:children (get block-map "SR Metadata"))
        property-block-content (map #(:content (get block-map %)) property-block-ids)
        prop-val-dict (metadata-properties property-block-content)]
    prop-val-dict))

(u/defn-memoized nav-bar-page-dict
  [block-map]
  (let [site-metadata-dict (site-metadata block-map)
        nav-bar-page-string (get site-metadata-dict "Nav Bar")
        nav-bar-pages-uncleaned (into
                                 (utils/find-content-entities-in-string nav-bar-page-string)
                                 (utils/find-hashtags-in-string nav-bar-page-string))
        nav-bar-pages (map utils/remove-double-delimiters nav-bar-pages-uncleaned)
        ;; TODO ugly but works to get italics in titles rendered properly. Should do same for backlinks
        nav-bar-pages-r (map #(static-roam.parser/block-content->hiccup % {}) 
                             nav-bar-pages)
        nav-bar-hrefs (map #(utils/page-title->html-file-title % true) nav-bar-pages)
        nav-bar-page-dict (zipmap nav-bar-hrefs nav-bar-pages-r)]
    nav-bar-page-dict))

;;; Old version
#_
(defn page-hiccup
  [body-hiccup page-title block-map]
  [:html
   `[:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title ~page-title]               ;TODO might want to prepend a site title
     [:link {:rel "stylesheet"
             :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
             :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
             :crossorigin "anonymous"}]
     ~@(for [css config/site-css]
         `[:link {:rel "stylesheet" :href ~css}])
     [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap"}]]
   [:body
    [:header.site-header
     [:div.wrapper
      [:nav.navbar.navbar-expand-lg.navbar-light ; .bg-dark.navbar-dark
       (into
        [:ul.navbar-nav.mr-auto]
        (map (fn [[url title]] [:li.nav-item 
                                [:a.nav-link {:href url}
                                 title
                                 ]])
             (nav-bar-page-dict block-map)))]]]
    [:div.container.main
     body-hiccup]]])

(defn roam-url
  [block-id]
  (str (:roam-base-url config/config) block-id))

(defn block-template
  [block-id block-map & [depth]]
  (let [depth (or depth 0)
        properties (get block-map block-id)]
    [:ul {:id block-id :class (if (< depth 2) "nondent" "")} ;don't indent the first 2 levels
     (if (or (nil? (:hiccup properties))
             (= (:content properties) block-id))
       nil
       [:li.block
        (when (:dev-mode config/config)
          [:a {:href (roam-url block-id)
               :target "_roam"
               ;; argh
               :style "background-color: lightgray; margin-left: 5px;"
               }
           "[e]"])
        (:hiccup properties)
        ])


                                        ;TODO prettify
     (map #(block-template % block-map (inc depth))
          (:children properties))
     ]))

(defn- get-parent
  [block-id block-map]
  (let [parent-id (database/block-parent (get block-map block-id) block-map)]
    (if (nil? parent-id)
      ""
      [:a {:href (str (utils/page-title->html-file-title
                       parent-id
                       :case-sensitive))}
       (:content (get block-map parent-id))])))

(defn page-link [page-id]
  [:a {:href (utils/page-title->html-file-title page-id :case-sensitive)}
   (parser/block-content->hiccup page-id {})]) ;TODO ugly but does the italic thing right

;;; Annoying amount of id/block switcheroo
(defn linked-reference-template
  [block-map r]
  (if (nil? (get block-map r))
    ""                                  ;TODO ugly
    (let [page (database/block-page block-map (get block-map r))]
      [:div
       "From " (page-link (:id page))
       [:div (block-template r block-map)]])))

(defn linked-references-template
  [references block-map]
  (concat []
          (map (partial linked-reference-template block-map) references)))

;;; Old version 
#_
(defn block-page-template
  [block-id block-map]
  (let [block (get block-map block-id)
        block-content (get block :content)]
    [:div
     [:h1.title (parser/block-content->hiccup block-content block-map)]    
     (if (or (:exit-point block)
             (empty? (:children block)))
       [:div.missing
        "This page does not yet exist!"] ;TODO ok this is sucky UX, the links themselves should look or act differently.
       [:div.page-content (block-template block-id block-map)])
     (let [linked-refs (database/get-included-linked-references block-id block-map)]
       (when-not (empty? linked-refs)
         [:div.incoming
          [:h3 "Incoming links"]
          (linked-references-template linked-refs block-map)]))]))

;;; Boostrap template version
(defn page-hiccup
  [body-hiccup page-title block-map]
  `[:html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title ~page-title]               ;TODO might want to prepend a site title
     [:link {:rel "stylesheet"
             :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
             :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
             :crossorigin "anonymous"}]
     ~@(for [css (:site-css config/config)]
         `[:link {:rel "stylesheet" :href ~css}])
     [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap"}]]
  [:body
   [:nav.navbar.navbar-expand-lg.navbar-dark.bg-dork.fixed-top
    [:div.container
     [:a.navbar-brand {:href "./Agency-Made-Me-Do-It.html"} "Agency Made Me Do it"] ;TODO config
     [:button.navbar-toggler
      {:type "button",
       :data-toggle "collapse",
       :data-target "#navbarResponsive",
       :aria-controls "navbarResponsive",
       :aria-expanded "false",
       :aria-label "Toggle navigation"}
      [:span.navbar-toggler-icon]]
     [:div.collapse.navbar-collapse
      {:id "navbarResponsive"}
      ;; TODO config and maybe make active page machinery mork
      [:ul.navbar-nav.ml-auto
       [:li.nav-item [:a.nav-link {:href "./index.html"} "Home"]]
       [:li.nav-item [:a.nav-link {:href "./recent-changes.html"} "Recent Changes"]]
       [:li.nav-item [:a.nav-link {:href "./About.html"} "About"]]
       #_ [:li.nav-item [:a.nav-link {:href "#"} "Contact"]]]]]]
   [:div.container
    ~body-hiccup]
   "<!-- Footer -->"
   [:footer.py-5.bg-dork
    [:div.container [:p.m-0.text-center.text-white "Copyright © Hyperphor 2020"]]
    ]]])


;;; Boostrap version
(defn block-page-template
  [block-id block-map]
  (let [block (get block-map block-id)
        title (parser/block-content->hiccup (get block :content) block-map) ;no this makes no sense
        contents (block-template block-id block-map)]
    [:div.main
     [:div.row
      "<!-- Post Content Column -->"
      [:div.col-lg-8
       "<!-- Title -->"
       [:h1.ptitle title]
       [:hr {}]
       contents
       [:hr {}]
       ]
      "<!-- Sidebar Widgets Column -->"
      [:div.col-md-4
       ;; TODO might be nice!
       "<!-- Search Widget -->"
       #_
       [:div.card.my-4
        [:h5.card-header "Search"]
        [:div.card-body
         [:div.input-group
          [:input.form-control {:type "text", :placeholder "Search for..."}]
          [:span.input-group-append [:button.btn.btn-secondary {:type "button"} "Go!"]]]]]
       "<!-- Categories Widget -->"
       #_
       [:div.card.my-4
        [:h5.card-header "Categories"]
        [:div.card-body
         [:div.row
          [:div.col-lg-6
           [:ul.list-unstyled.mb-0
            [:li {} [:a {:href "#"} "Web Design"]]
            [:li {} [:a {:href "#"} "HTML"]]
            [:li {} [:a {:href "#"} "Freebies"]]]]
          [:div.col-lg-6
           [:ul.list-unstyled.mb-0
            [:li {} [:a {:href "#"} "JavaScript"]]
            [:li {} [:a {:href "#"} "CSS"]]
            [:li {} [:a {:href "#"} "Tutorials"]]]]]]]
       ;; incoming
       (let [linked-refs (database/get-included-linked-references block-id block-map)]
         (when-not (empty? linked-refs)
           [:div.card.my-4
            [:h5.card-header "Incoming links"]
            [:div.card-body
             [:div.incoming
              (linked-references-template linked-refs block-map)]]]))
       ]]]
    ))

(defn home-page-hiccup
  [entry-points block-map]
  (page-hiccup 
   [:div.main.page-content {:aria-label "Content"}
     [:div.wrapper
       [:h2.post-list-heading "Entry Points"]
      [:ul.post-list
       ;; TOD sort
       (map (fn [page] [:li [:h3 (page-link (:id page))]]) entry-points)]]]
   (get (site-metadata block-map) "Title")
   block-map))



