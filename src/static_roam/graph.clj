(ns static-roam.graph
  (:require [oz.core :as oz]
            [static-roam.batadase :as bd]
;            [static-roam.rendering :as render]
            [static-roam.utils :as utils]
            [clojure.data.json :as json]
            [org.parkerici.multitool.core :as u]
            ))

;;; Based on https://vega.github.io/vega/examples/force-directed-layout/

;;; Status: makes a graph
;;; TODO better link highlighting
;;; TODO maybe color for recency?
;;; TODO on side map in private mode, highlight excluded
;;; TODO on maps, empty pages should be visually distinguished (or removed, which is DONE)
;;; Starting to require a legend though

;;; TODO would be more Roam-ish to have a normal Map page with a special tag that put the vega stuff there. Then it could be linked to in the normal way.  OOOH also partial maps would be easy! Maybe in the sidebar... Mini-maps centered on current page.

;;; TODO generating a json file per page is a pain. Maybe encode it in the html file? Probably page graphs should be smaller anyway, they are too crowded as it is.

;;; TODO max-degree is a hack and I don't like it – without it, if you hit a high-degree node you'll get too much in the graph
;;; some kind of smart filter would be better
;;; max-degree is really max-distance 

(defn page-neighbors
  [bm from n max-degree]
  (let [neighbors (fn [b] (take max-degree (remove bd/page-empty? (map bm (bd/page-refs bm b)))))]
    (u/neighborhood from n neighbors)))

(defn graph-pages
  [block-map]
  (bd/displayed-regular-pages block-map))

(defn graph-data
  "radius-from: name of central page"
  [block-map {:keys [radius-from radius max-degree] :or {radius 2 max-degree 8}}]
  (let [pages (->> block-map
                  graph-pages
                  (filter (if radius-from
                            (let [neighborhood (set (map :content (page-neighbors block-map (get block-map radius-from) radius max-degree)))]
                              #(contains? neighborhood (:content %)))
                            identity))
                  (map (fn [index block] (assoc block
                                                :index index
                                                :page-refs (bd/page-refs block-map block)
                                                :link (utils/html-file-title (:content block))))
                       (range))
                  )
        indexed (u/index-by :id pages)
        ;; Starting nodes, either radius-frame or an entry point if doing the whole graph
        start? (fn [b] (if radius-from  
                         (= (:id b) radius-from)
                         (bd/entry-point? block-map b)))
        ]
    [{:name "node-data"
      :values (sort-by :index           ; Order matters; links refer to position, not index field
                       (map (fn [b]
                              ;; TODO why not use actual size???
                              ;;  :size (- 20 (or (:depth b) 0)) (not working)
                              {:name (:content b) ; (render/block-local-text b)      ;; TODO strips markup like __foo__ (might want to be config)
                               :link (:link b)
                               :index (:index b)
                               :group (cond (start? b)
                                            0
                                            (:include? b)
                                            1
                                            :else
                                            8)   ;only shows up in private mode
                               ;; This is the AREA of the circle
                               :size (+ 50 (Math/pow (* 3 (- 12 (:depth b 0))) 2))
                               })
                            pages))}
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
  `{:$schema "https://vega.github.io/schema/vega/v5.json"
    :data ~(graph-data block-map options)
    :autosize "none"
    :width ~(or width 1000)
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
     ;; "State variable for active node fix status."
     {:name "fix"
      :value false
      :on
      [{:events "symbol:mouseout[!event.buttons], window:mouseup" :update "false"}
       {:events "symbol:mouseover" :update "fix || true"}
       {:events "[symbol:mousedown, window:mouseup] > window:mousemove!"
        :update "xy()"
        :force true}]}
     ;; :description "Graph node most recently interacted with."
     {:name "node"
      :value nil
      :on [{:events "symbol:mouseover" :update "fix === true ? item() : node"}]}
     ;; :description "Flag to restart Force simulation upon data changes."
     {:name "restart"
      :value false
      :on [{:events {:signal "fix"} :update "fix && fix.length"}]}]
    })

;;; For displaying in development
(defn display
  [block-map options]
  (oz/view! (spec block-map options) :port 1889 :mode :vega))

;;; Static render

(defn render-graph
  "Writes out the graph json and returns the hiccup to embed it"
  [bm output-dir {:keys [name width height controls?] :as options :or {height 1000}}]
  (utils/write-json (str output-dir "/pages/graphs/" name ".json") (spec bm options))
  (let [id (str "view_" name)]
    [:div
     [:div.graph {:id id :style (format "height: %spx;" (+ height (if controls? 300 0)))}]
     [:script
      (format "vegaEmbed('#%s', 'graphs/%s.json');" id name)
      ]]))

(defn render-vega-embedded
  "Render an arbitrary Vega specs"
  [spec name height]
  (let [json (json/write-str spec)
        id (str "view_" name)]
    [:div
     [:div.graph {:id id :style (and height (format "height: %spx;" height))}]
     [:script
      (format "vegaEmbed.embed('#%s', %s);" id json)
      ]]))

(defn render-graph-embedded
  "the hiccup to embed graph, including the json"
  [bm output-dir {:keys [name width height controls?] :as options :or {height 1000}}]
  (render-vega-embedded (spec bm options) name (+ height (if controls? 300 0))))

;;; See https://vega.github.io/vega-lite/usage/embed.html

(defn vega-head
  []
  [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.2"}]
   [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.17.0"}]
   ])

(defn vega-lite-head
  []
  [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.2"}]
   [:script {:src "https://cdn.jsdelivr.net/npm/vega-lite@5.1.0"}]
   [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.17.0"}]
   ])

;;; Experiments

(defn page-data
  "Generate a data table of pages (TODO or blocks) suitable for passing to Vega"
  [block-map]
  (for [page (graph-pages block-map)
        :let [[start end] (bd/date-range page)]]
    {:title (:content page)
     :fan (count (bd/page-refs block-map page))     ;TODO separate in and out 
     :size (bd/size page)
     :depth (:depth page)
     :start (str start)
     :end (str end)
     :link (utils/html-file-title (:content page))
     ; TODO size in blocks would be interesting maybe
     :group (cond (bd/entry-point? block-map page) 2
                  (:include? page) 1
                  :else 0)
     }
    ))

(defn write-page-data
  [bm output-dir]
  (utils/write-json (str output-dir "/pages/graphs/pages.json") (page-data bm)))

(def view1-spec
  {:mark {:type "point"
          :filled true
          :opacity 0.8
;          :tooltip true                 ;for debugging
          }
   :data {:url "graphs/pages.json"}
   :encoding
   {:x {:field "end", :type "temporal" :axis {:title "Last edited"}}
    :y {:field "fan", :type "quantitative" :scale {:type "log"} :axis {:title "Connections"}}
    :color {:field "group" :type "nominal" :legend nil}
    :size {:field "size" :type "quantitative" :scale {:rangeMin 20 :rangeMax 1000}}
    :tooltip {:field "title"}
    :href {:field "link"}
    }
   :height 600,
   :width 700})

(defn render-dataviz
  [bm output-dir]
  ;; Can no longer do this here because rendering gets done before full bm is there.
  #_ (write-page-data bm output-dir)       ;doing this side-effecty thing here feels janky
  (render-vega-embedded
   view1-spec
   "dataviz"
   nil))
