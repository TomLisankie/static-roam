(ns goddinpotty.templating
  (:require [clojure.string :as s]
            [goddinpotty.rendering :as render]
            [goddinpotty.config :as config]
            [goddinpotty.utils :as utils]
            [goddinpotty.graph :as graph]
            [goddinpotty.search :as search]
            [goddinpotty.batadase :as bd]
            [org.parkerici.multitool.core :as u]
            ))

;;; TODO Note: the functions of templating and html-gen seem to overlap; not sure they should be separate.


;;; TODO site-metadata stuff I don't use much, either get rid of it or spruce it up and use it
(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(s/split % #":: ") metadata))))

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
             :href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css" 
             :integrity "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU"
             :crossorigin "anonymous"}]
     [:link {:rel "stylesheet" :href "assets/default.css"}]
     ~@(for [css (config/config :site-css)]
         `[:link {:rel "stylesheet" :href ~css}])
     [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
     ;; Using slightly-bold font for links for whatever reason.
     ;; TODO fonts should be specifyiable in config
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=EB+Garamond:wght@400;500&display=swap"}]
     [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Varela+Round:wght@400;600&display=swap"}]
     ;; TODO this should be conditional on latex appearing on the page
     ;; see https://docs.mathjax.org/en/latest/web/typeset.html#load-for-math
     [:script {:src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js" :type "text/javascript"}]
     ~@(search/search-head)             ;search is on every page
     ~@head-extra
     ]
    [:body
     [:nav.navbar.navbar-expand-lg.navbar-dark.bg-dork.fixed-top
      [:div.container.navbar-x
       ~(render/page-link-by-name block-map (config/config :main-page) :class "navbar-brand")
       ;; TODO make active page machinery mork
       [:ul.navbar-nav.ml-auto
        ~@(for [page (config/config :right-navbar)]
            [:li.nav-item
             (if (vector? page) 
               ;[:a {:href (second page) :class "nav-link"} (first page)]
               (render/page-link (second page) :class "nav-link" :bm block-map :alias (first page))
               (render/page-link page :class "nav-link" :bm block-map))]
            )]]]
     [:div.container.main
      [:div.row
       "<!-- Sidebar Widgets Column -->"
       [:div.col-md-3

        ;; TODO Is there a reason this isn't under widget mechanism?
        "<!-- Search Widget -->"
        [:div.card.my-3
         [:h5.card-header [:span {:style ~(utils/css-style {:line-height "2"})} ;makes it line up, god knows why its necessary
                           "Search"]
          [:input.form-control {:id "searcht"
                                :type "text"
                                ;; :placeholder "Search for..."
                                :onkeyup "keypress(event)"
                                :style ~(utils/css-style
                                         {:float "right"
                                          :display "flex"
                                          :width "70%"})
                                }]
          ]
         [:div.card-body {:style ~(utils/css-style {:padding "0.25rem"})}
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
       [:div.col-lg-7
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
       ;; Out of service because it adds a gratuitoous change to all files, not good with git.
       ;; TODO maybe a server-side include or frame or something. Or a config option
       #_
       [:p.m-0.text-center.text-white.small "Exported " ~(utils/render-time (utils/latest-export-time))]]
      ]
     [:script {:src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/js/bootstrap.bundle.min.js"
               :integrity "sha384-/bQdsTh/da6pkI1MST/rWKFNjaCP5gBSY4sEBT38Q/9RBh9AH40zEOg7Hlq2THRZ"
               :crossorigin "anonymous"}]
     ]])

(defn map-page
  [bm]
  (page-hiccup
   (graph/render-graph bm (config/config :output-dir) {:name "fullmap" ;warning: this name can't be the same as a page name!
                                      :include-all? (config/config :unexclude?)
                                      })
   "Map"
   "Map"
   bm
   :head-extra (graph/vega-head)))

(defn render-date-range
  [[from to]]
  [:div.date (utils/render-time from) " - " (utils/render-time to)])


;;; Note: not as elegant as it could be, but works
(defn render-page-hierarchy-1
  [path page-struct bm this]
  (let [top (clojure.string/join "/" path)]
    [:div
     (render/page-link top :bm bm :alias (last path))
   ;; TODO tweak css so long things look rightish
   ;; whitespace: nowrap (but needs to truncate or something)
   [:ul
    (for [child (sort (keys page-struct))] ;Sort imposes some order, better than random I guess
      (if (map? (get page-struct child))
        (render-page-hierarchy-1 (conj path child)
                                 (get page-struct child)
                                 bm this)
        [:li (render/page-link (str top "/" child) :bm bm :alias child :current this)]))]]))


(defn render-page-hierarchy
  [page-name bm]
  (let [[_ top] (or (re-find #"^(.*?)/(.*)$" page-name) [nil page-name])
        page-struct (get (bd/page-hierarchies bm) top)]
    (render-page-hierarchy-1 [top] page-struct bm page-name)))

(defn block-page-hiccup
  [block-id block-map output-dir]
  (let [block (get block-map block-id)
        title-hiccup (render/block-content->hiccup (:title block))
        title-text (:title block) ; was render/block-local-text
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
        [:div.card.my-3
         [:h5.card-header
          [:a {:data-bs-toggle "collapse"
               :data-bs-target "#mapgraph"
;               :type "button"
               :aria-expanded "false"
               :aria-controls "mapgraph"
               :onclick "toggleMap();"  ;hack to persist state, see search.js
               }
           "Map"]
          [:span {:style (utils/css-style
                          {:float "right"
                           :display "flex"
                           })}
           (render/page-link-by-name block-map "Map" :alias "Full" )]]
         [:div#mapgraph.collapse
          [:div.card-body {:style (utils/css-style {:padding "2px"})}
           ;; TODO possible config to do embedded vs external
           (graph/render-graph ;; render-graph-embedded
            block-map
            output-dir
            {:name (utils/clean-page-title block-id)
             :width 290                  ;This depends on the column width css, currently my-3
             :height 350
             :controls? false
             :link-distance 65
             :node-charge -60
             :node-radius 25
             :center block-id
             :radius 2}) ;Sadly 1 is too small and 2 is too big. Need 1.1
           ]]]

        incoming-links-widget           ;TODO suppress if #incoming on page
        (let [linked-refs (bd/get-displayed-linked-references block-id block-map)]
          (when-not (empty? linked-refs)
            [:div.card.my-3
             [:h5.card-header "Incoming links"]
             [:div.card-body
              [:div.incoming
               (linked-references-template linked-refs block-map)]]]))

        page-hierarchy-widget
        (when (bd/page-in-hierarchy? block block-map)
          [:div.card.my-3
           [:h5.card-header "Contents"] ;for lack of a better name
           [:div.card-body
            (render-page-hierarchy (:title block) block-map)]])

        ;; See http://webseitz.fluxent.com/wiki/TwinPages , but this doesn't work for a number of reasons:
        ;; - Needs page names like /AlanKay, not /Alan-Kay.html
        ;; - Needs to be added to a list?
        ;; to test: curl -v -H Referer:http://hyperphor.com/ammdi/Logseq http://www.wikigraph.net/twinpages.js  
        #_
        twin-pages-widget
        #_
        [:div.card.my-3
         [:h5.card-header "Twin Pages"]
         [:div.card-body
          [:div#twin_pages
           {:style {}} ;"border:1px; vertical-align:top; horizontal-align:center"
           [:script {:src "http://www.wikigraph.net/twinpages.js"
                     :referrerpolicy "unsafe-url"
                     }]]]]]

    ;; TODO why isn't search widget done this way?
    (page-hiccup contents title-text title-hiccup block-map
                 :head-extra (graph/vega-lite-head) ;lite to support new dataviz
                 :widgets [map-widget page-hierarchy-widget incoming-links-widget])
    ))


;;; TODO this also should suppress the incoming links in lh column
;;; Oh shit this can't work, rendering happens too early 
;;; Needs to be a post-processing step, fuck me
;;; OR maybe not if I use a delay...what a hack. No that can't really work I think...delays require an explict deref
;;; Not really working yet so disabled
#_
(bd/register-special-tag
 "incoming"
 (fn [bm block]
   (fn [bm]                             ;return a fn to be rendered later with the full bm
     (linked-references-template
      (bd/get-displayed-linked-references (:id (bd/block-page bm block)) bm)
      bm))))
