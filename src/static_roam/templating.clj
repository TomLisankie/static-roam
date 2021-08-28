(ns static-roam.templating
  (:require [clojure.string :as s]
            [static-roam.rendering :as render]
            [static-roam.config :as config]
            [static-roam.utils :as utils]
            [static-roam.graph :as graph]
            [static-roam.search :as search]
            [static-roam.batadase :as bd]
            [org.parkerici.multitool.core :as u]
            ))

;;; TODO Note: the functions of templating and html-gen seem to overlap; not sure they should be separate.


;;; TODO site-metadata stuff I don't use much, either get rid of it or spruce it up and use it
(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(s/split % #":: ") metadata))))

(defn- site-metadata
  [block-map]
  (let [property-block-ids (:children (get block-map "SR Metadata"))
        property-block-content (map #(:content (get block-map %)) property-block-ids)
        prop-val-dict (metadata-properties property-block-content)]
    prop-val-dict))

;;; Annoying amount of id/block switcheroo
(defn linked-reference-template
  [block-map r]
  (if (nil? (get block-map r))
    ""                                  ;TODO ugly
    (let [page (bd/block-page block-map (get block-map r))]
      [:div
       "from " (render/page-link page)
       ;; TODO this might want to do an expand thing like in recent-changes page? Does't actually seem necessary here
       ;; TODO has been assuming this is in a low-level block, but that's not necessarily the case. For [[Introduction to [[Inventive Minds]]]], it includes the whole page!
       ;; See bd/expand-to, but we want to shrink in this case
       [:div (render/block-full-hiccup r block-map)]])))

(defn linked-references-template
  [references block-map]
  (concat []
          (map (partial linked-reference-template block-map) references)))

(defn analytics-1
  []
  (and (config/config :google-analytics)
       [:script {:async true :src (format "https://www.googletagmanager.com/gtag/js?id=%s" (config/config :google-analytics))}]))

(defn analytics-2
  []
  (and (config/config :google-analytics)
       [:script (format
                 "
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', '%s');
" (config/config :google-analytics))]))

;;; TODO much of this should be configurable
(defn page-hiccup
  [contents title-text title-hiccup block-map & {:keys [head-extra widgets]}]
  `[:html
    [:head
     ~(analytics-1)
     ~(analytics-2)
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title ~(str (config/config :short-title) ": " title-text)]
     [:link {:rel "stylesheet"
             :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
             :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
             :crossorigin "anonymous"}]
     [:link {:rel "stylesheet" :href "../assets/default.css"}]
     ~@(for [css (config/config :site-css)]
         `[:link {:rel "stylesheet" :href ~css}])
     [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
     ;; Using slightly-bold font for links for whatever reason.
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Lara:wght@400;500&display=swap"}]
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Varela+Round:wght@400;600&display=swap"}]
     ;; TODO this should be conditional on latex appearing on the page
     ;; see https://docs.mathjax.org/en/latest/web/typeset.html#load-for-math
     [:script {:src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js" :type "text/javascript"}]
     ~@(search/search-head)             ;search is on every page
     ~@head-extra
     ]
    [:body
     [:nav.navbar.navbar-expand-lg.navbar-dark.bg-dork.fixed-top
      [:div.container
       ~(render/page-link-by-name block-map (config/config :main-page) :class "navbar-brand")
       #_
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
        ;; TODO make active page machinery mork
        [:ul.navbar-nav.ml-auto
         ~@(for [page (config/config :right-navbar)]
             [:li.nav-item (render/page-link-by-name block-map page :class "nav-link")])
         ]]]]
     [:div.container.main
      [:div.row
       "<!-- Sidebar Widgets Column -->"
       [:div.col-md-4
        "<!-- Search Widget -->"
        [:div.card.my-4
         [:h5.card-header [:span {:style ~(utils/css-style {:line-height "2"})} ;makes it line up, god knows why its necessary
                           "Search"]
          [:input.form-control {:id "searcht"
                                :type "text"
                                ;; :placeholder "Search for..."
                                :onkeyup "keypress(event)"
                                :style ~(utils/css-style
                                         {:float "right"
                                          :display "flex"
                                          :width "75%"})
                                }]
          ]
         [:div.card-body
          ;; output goes here
          [:div#searcho {:style ~(utils/css-style
                                  {:margin-left "10px"
                                   :margin-top "5px"
                                   :display "none" ;javascript changes this when there are results
                                   })}] 
          ]]
        ~@widgets
        ]

       "<!-- Post Content Column -->"
       [:div.col-lg-8
        "<!-- Title -->"
        [:div.ptitle
         [:h1 ~title-hiccup]
         ~contents

         ]]
       ]]
     "<!-- Footer -->"
     [:footer.py-5.footer
      [:div.container
       ~(when (config/config :colophon)
          `[:p.m-0.text-center.text-white ~@(config/config :colophon)])
       [:p.m-0.text-center.text-white.small "Exported " ~(utils/render-time @utils/latest-export-time)]]
      ]
     ]])

(defn map-page
  [bm output-dir]
  (page-hiccup
   (graph/render-graph bm output-dir {:name "fullmap" ;warning: this name can't be the same as a page name!
                                      :include-all? (config/config :unexclude?)
                                      })
   "Map"
   "Map"
   bm
   :head-extra (graph/vega-head)))

(defn dataviz-page
  [bm output-dir]
  ;;; TODO write data faile
  (page-hiccup
   (graph/render-dataviz bm output-dir)
   "DataViz"
   "DataViz"
   bm
   :head-extra (graph/vega-lite-head)))

(defn render-date-range
  [[from to]]
  [:div.date (utils/render-time from) " - " (utils/render-time to)])

(defn block-page-hiccup
  [block-id block-map output-dir]
  (let [block (get block-map block-id)
        title-hiccup (render/block-content->hiccup (:content block))
        title-text (render/block-local-text block)
        contents
        [:div
         [:div
          (u/ignore-errors
           (render-date-range (bd/date-range block)))]
         (when-not (:include? block)
           [:span [:b "EXCLUDED"]])       ;TODO make this pop more
         [:hr {}]
         (render/page-hiccup block-id block-map)
         [:hr {}]]

        map-widget
        [:div.card.my-4
         [:h5.card-header "Map"]
         [:div.card-body {:style "padding: 2px;"}
          ;; TODO possible config to do embedded vs external
          (graph/render-graph-embedded
           block-map
           output-dir
           {:name (utils/clean-page-title block-id)
            :width 350
            :height 350
            :controls? false
            :link-distance 65
            :node-charge -60
            :node-radius 25
            :radius-from block-id
            :radius 2}) ;Sadly 1 is too small and 2 is too big. Need 1.1
          ]]

        incoming-links-widget
        (let [linked-refs (bd/get-displayed-linked-references block-id block-map)]
          (when-not (empty? linked-refs)
            [:div.card.my-4
             [:h5.card-header "Incoming links"]
             [:div.card-body
              [:div.incoming
               (linked-references-template linked-refs block-map)]]]))

        ]
    (page-hiccup contents title-text title-hiccup block-map :head-extra (graph/vega-head) :widgets [map-widget incoming-links-widget])
    ))




