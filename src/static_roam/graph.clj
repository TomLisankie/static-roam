(ns static-roam.graph
  (:require [oz.core :as oz]
            [static-roam.database :as db]
            [static-roam.templating :as template]
            [static-roam.utils :as utils]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [org.parkerici.multitool.core :as u]
            ))

;;; Based on https://vega.github.io/vega/examples/force-directed-layout/

;;; Status: makes a graph
;;; TODO better link highlighting
;;; More contrast between nodes
;;; TODO maybe color for recency?


(defn graph-data
  [block-map]
  (let [pages (->> block-map
                   vals
                   (filter :page?)
                   (filter :include?)
                   (map (fn [index block] (assoc block
                                                 :index index
                                                 :page-refs (db/page-refs block)
                                                 :link (utils/page-title->html-file-title (:content block))))
                        (range)))
        indexed (u/index-by :id pages)] ;make a new block map...
    [{:name "node-data"
      :values (map (fn [b]
                     ;;  :size (- 20 (or (:depth b) 0)) (not working)
                     {:name (:id b)
                      :link (:link b)
                      :index (:index b)
;                      :group (if (:include? b) 1 8)
                      ;; This is the AREA of the circle
                      :size (+ 50 (Math/pow (* 3 (- 12 (:depth b 0))) 2))
                      })
                   pages)}
     {:name "link-data"
      :values (remove #(nil? (:target %))
                      (mapcat (fn [b]
                                (map (fn [ref]
                                       {:source (:index b) :target (get-in indexed [ref :index]) :value 1})
                                     (:page-refs b)))
                              pages))}]))


(defn spec
  [block-map]
  `{:description
    "A node-link diagram of AMMDI pages and links."
    :$schema "https://vega.github.io/schema/vega/v5.json"
    :data ~(graph-data block-map)
    :autosize "none"
    :width 1500
    :height 1000
    :scales
    [{:name "color"
      :type "ordinal"
      :domain {:data "node-data" :field "group"}
      :range {:scheme "set1"}
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
      {:enter {:fill {:scale "color" :field "group"}
               ;; TODO apply to label also
               ;; TODO customization point
               ;; TODO would be nice if this could open in different browser tab
               :href {:field "link" }
               :stroke {:value "white"}
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
      :zindex 1
      :encode
      {:enter {:text {:field "datum.name"}
               :x {:signal "datum.x + 10"} ;TODO these offsets should depend on size of node
               :y {:signal "datum.y + 3"}
               :size {:fontSize {:value 5}}
               :fill {:value "gray"}}
       :update {:x {:signal "datum.x + 10"}
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
     {:name "title"
      :update "hover ? hover.name : 'AMMDI'"
      }
     ;; These numbers make a decent looking graph for current export (~200 nodes).
     ;; TODO Adjusting to different scale should be configurable if not automated
     {:name "nodeRadius" :value 20 :bind {:input "range" :min 1 :max 50 :step 1}}
     {:name "nodeCharge" :value -100 :bind {:input "range" :min -100 :max 10 :step 1}}
     {:name "linkDistance" :value 60 :bind {:input "range" :min 5 :max 100 :step 1}}
     {:name "static" :value true :bind {:input "checkbox"}}
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
  [block-map]
  (oz/view! (spec block-map) :port 1889 :mode :vega))

;;; Static render

(defn write-json [f data]
  (with-open [s (io/writer f)]
    (json/write data s)))

(defn generate-map
  "Writes out graph json and returns the page hiccup"
  [bm output-dir]
  (write-json (str output-dir "/pages/graph.json") (spec bm))
   (template/page-hiccup
    [:div
     [:div#view {:style "width: 100%; height: 1000px;"}]
     [:script
      "vegaEmbed('#view', 'graph.json');"
      ]]
    "Map" bm
    [[:script {:src "https://cdn.jsdelivr.net/npm/vega@5.20.0"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@6.16.0"}]]))


