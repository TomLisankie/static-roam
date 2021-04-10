(ns static-roam.graph
  (:require [oz.core :as oz]
            [static-roam.database :as db]
            [static-roam.utils :as utils]
            [org.parkerici.multitool.core :as u]
            ))

;;; Based on https://vega.github.io/vega/examples/force-directed-layout/

;;; Status: makes a graph
;;; TODO better link highlighting
;;; TODO maybe color for recency?
;;; TODO on side map in private mode, highlight excluded
;;; TODO on maps, empty pages should be visually distinguished
;;; Starting to require a legend though

;;; TODO would be more Roam-ish to have a normal Map page with a special tag that put the vega stuff there. Then it could be linked to in the normal way.  OOOH also partial maps would be easy! Maybe in the sidebar... Mini-maps centered on current page.


;;; → multitool
(defn neighborhood
  [from n neighbors]
  (if (zero? n)
    (set (list from))
    (set (cons from (mapcat #(neighborhood % (- n 1) neighbors)
                            (neighbors from))))))

;;; → db
(defn block-neighbors
  [bm from n]
  (let [neighbors (fn [b] (map bm (db/all-refs b)))]
    (neighborhood from n neighbors)))

;;; → db
(u/defn-memoized degree [block]
  (count (db/all-refs block)))

;;; TODO max-degree is a hack and I don't like it – without it, if you hit a high-degree node you'll get too much in the graph
;;; some kind of smart filter would be better
(defn page-neighbors
  [bm from n max-degree]
  (let [neighbors (fn [b] (take max-degree (map bm (db/page-refs bm b))))]
    (neighborhood from n neighbors)))

(defn graph-data
  [block-map {:keys [radius-from radius max-degree] :or {radius 2 max-degree 8}}]
  (let [pages (->> (db/displayed-regular-pages block-map)
                   (filter (if radius-from
                             (let [neighborhood (set (map :content (page-neighbors block-map (get block-map radius-from) radius max-degree)))]
                               #(contains? neighborhood (:content %)))
                             identity))
                   (map (fn [index block] (assoc block
                                                 :index index
                                                 :page-refs (db/page-refs block-map block)
                                                 :link (utils/html-file-title (:content block))))
                        (range)))
        indexed (u/index-by :id pages)
        ;; Starting nodes, either radius-frame or an entry point if doing the whole graph
        start? (fn [b] (if radius-from  
                         (= (:id b) radius-from)
                         (db/entry-point? block-map b)))
        ]
    [{:name "node-data"
      :values (map (fn [b]
                     ;;  :size (- 20 (or (:depth b) 0)) (not working)
                     {:name (:id b)
                      :link (:link b)
                      :index (:index b)
                      :group (cond (start? b)
                                   7
                                   (:include? b)
                                   1
                                   :else
                                   8)   ;only shows up in private mode
                      ;; This is the AREA of the circle
                      :size (+ 50 (Math/pow (* 3 (- 12 (:depth b 0))) 2))
                      })
                   pages)}
     {:name "link-data"
      :values (remove #(or (nil? (:target %))
                           ;; TODO links are symmetrical so this removes the redundnt half 
                           (< (:target %) (:source %)))
                      (mapcat (fn [b]
                                (map (fn [ref]
                                       {:source (:index b) :target (get-in indexed [ref :index]) })
                                     (:page-refs b)))
                              pages))}]))


(defn spec
  [block-map {:keys [width height controls? link-distance node-charge node-radius] :as options :or {link-distance 60 node-charge -100 node-radius 20}}]
  `{:description
    "A node-link diagram of Roam pages and links."
    :$schema "https://vega.github.io/schema/vega/v5.json"
    :data ~(graph-data block-map options)
    :autosize "none"
    :width ~(or width 1500)
    :height ~(or height 1000)
    :usermeta {:embedOptions {:actions false}} ;this turns off the menu with editor etc.
    :scales
    [{:name "color"
      :type "ordinal"
      :domain {:data "node-data" :field "group"}
      :range {:scheme "pastel1"}
      }]
    :padding 0
    :marks
    [{:name "nodes"
      :type "symbol"
      :zindex 1
      :from {:data "node-data"}
      :on
      [{:trigger "fix" :modify "node" :values "fix === true ? {fx: node.x, fy: node.y} : {fx: fix[0], fy: fix[1]}"}
       {:trigger "!fix" :modify "node" :values "{fx: null, fy: null}"}]
      :encode
      {:enter {:fill {:scale "color"  :field "group"} ;  {:value "lightcoral"}
               ;; TODO would be nice if this could open in different browser tab
               :href {:field "link" }
               :stroke {:value "white"}
               :strokeWidth {:value 0}
               :size {:field "size"}}
       #_ :update #_ {:size {:signal "2 * nodeRadius * nodeRadius"} :cursor {:value "pointer"}}
       }
      :transform
      [{:type "force"
        :iterations 300
        :restart {:signal "restart"}
        :static {:signal "static"}
        :signal "force"
        :forces
        [{:force "center" :x {:signal "cx"} :y {:signal "cy"}}
         {:force "collide" :radius {:signal "nodeRadius"}}
         {:force "nbody" :strength {:signal "nodeCharge"}}
         {:force "link" :links "link-data" :distance {:signal "linkDistance"}}]}]}

     {:name "nodelabels"
      :type "text"
      :from {:data "nodes"}
      :zindex 2
      :encode
      {:enter {:text {:field "datum.name"}
               :x {:signal "datum.x + 2 + sqrt(datum.size)/2"}
               :y {:signal "datum.y + 3"}
               :size {:fontSize {:value 5}}
               :fill {:value "black"}} ; gray
       :update {:x {:signal "datum.x + 2 + sqrt(datum.size)/2"}
                :y {:signal "datum.y + 3"}
                }
       }
      }
     
     {:type "path"
      :from {:data "link-data"}
      :interactive false
      :encode {:update
               {:stroke {:value "gray"}
                :strokeWidth {:signal "datum.source === node || datum.target === node ? 2 : 0.5"}}
               }
      :transform
      [{:type "linkpath"
        :require {:signal "force"}
        :shape "line"
        :sourceX "datum.source.x"
        :sourceY "datum.source.y"
        :targetX "datum.target.x"
        :targetY "datum.target.y"}
       ]
      }
     ]
    :signals
    [{:name "cx" :update "width / 2"}
     {:name "cy" :update "height / 2"}
     {:name "hover"
      :value nil
      :on [
           {:events "@nodes:mouseover" :update "datum"}
           {:events "@nodes:mouseout" :update "null"}
           ]
      }
     ;; TODO Adjusting to different scale should be configurable if not automated
     {:name "nodeRadius" :value ~node-radius :bind ~(and controls? {:input "range" :min 1 :max 50 :step 1})}
     {:name "nodeCharge" :value ~node-charge :bind ~(and controls? {:input "range" :min -100 :max 10 :step 1})}
     {:name "linkDistance" :value ~link-distance :bind ~(and controls? {:input "range" :min 5 :max 100 :step 1})}
     {:name "static" :value false :bind ~(and controls? {:input "checkbox"})}
     {:description "State variable for active node fix status."
      :name "fix"
      :value false
      :on
      [{:events "symbol:mouseout[!event.buttons], window:mouseup" :update "false"}
       {:events "symbol:mouseover" :update "fix || true"}
       {:events "[symbol:mousedown, window:mouseup] > window:mousemove!"
        :update "xy()"
        :force true}]}
     {:description "Graph node most recently interacted with."
      :name "node"
      :value nil
      :on [{:events "symbol:mouseover" :update "fix === true ? item() : node"}]}
     {:description "Flag to restart Force simulation upon data changes."
      :name "restart"
      :value false
      :on [{:events {:signal "fix"} :update "fix && fix.length"}]}]
    })

;;; For displaying in development
(defn display
  [block-map options]
  (oz/view! (spec block-map options) :port 1889 :mode :vega))

;;; Static render



;; obso
#_ 
(defn generate-map
  "Writes out graph json and returns the page hiccup"
  [bm output-dir {:keys [name] :as options}]
  (write-json (str output-dir "/pages/graphs/" name ".json") (spec bm options))
  (template/page-hiccup
    [:div
     [:div#view {:style "width: 100%; height: 1000px;"}] ;TODO parameterize
     [:script
      (format "vegaEmbed('#view_%s', 'graphs/%s.json');" name name)
      ]]
    "Map" bm
    [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.0"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.16.0"}]]))

(defn render-graph
  "Writes out the graph json and returns the hiccup to embed it"
  [bm output-dir {:keys [name width height controls?] :as options :or {height 1000}}]
  (utils/write-json (str output-dir "/pages/graphs/" name ".json") (spec bm options))
  (let [id (str "view_" name)]
    [:div
     [:div {:id id :style (format "width: 100%%; height: %spx;" (+ height (if controls? 300 0)))}] ; width
     [:script
      (format "vegaEmbed('#%s', 'graphs/%s.json');" id name)
      ]]))

(defn vega-head
  []
  [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.0"}]
   [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.16.0"}]
   ;; For search, should be separated out
   [:script {:src "http://elasticlunr.com/elasticlunr.min.js"}] ;TODO temp
   [:script {:src "../assets/search.js"}]
   ])


