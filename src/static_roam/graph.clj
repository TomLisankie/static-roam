(ns static-roam.graph
  (:require [oz.core :as oz]
            [static-roam.database :as db]
            [org.parkerici.multitool.core :as u]
            ))

;;; Based on https://vega.github.io/vega/examples/force-directed-layout/

;;; Status: makes a first-order graph! To make it useful:
;;; - tooltips for block names, and click action to open
;;; - sizing (by size) and coloring (by certain tags)


(defn foo
  [block-map]
  (let [pages (->> block-map
                   vals
                   (filter :page?)
                   (map (fn [index block] (assoc block
                                                 :index index
                                                 :page-refs (db/page-refs block-map block)))
                        (range)))
        indexed (u/index-by :id pages)] ;make a new block map...
    indexed))


(defn graph-data
  [block-map]
  (let [pages (->> block-map
                   vals
                   (filter :page?)
                   (map (fn [index block] (assoc block
                                                 :index index
                                                 :page-refs (db/page-refs block-map block)))
                        (range)))
        indexed (u/index-by :id pages)] ;make a new block map...
    [{:name "node-data"
      :values (map (fn [b]
                     ;;  :size (- 20 (or (:depth b) 0)) (not working)
                     {:name (:id b) :group (if (:include? b) 1 8) :index (:index b)})
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
    "A node-link diagram with force-directed layout, depicting character co-occurrence in the novel Les Misérables.",
    :data
    ~(graph-data block-map)
    :autosize "none",
    :width 700,
    :scales
    [{:name "color",
      :type "ordinal",
      :domain {:data "node-data", :field "group"},
      :range {:scheme "set1"}
      }],
    :padding 0,
    :marks
    [{:name "nodes",
      :type "symbol",
      :zindex 1,
      :from {:data "node-data"},
      :on
      [{:trigger "fix",
        :modify "node",
        :values "fix === true ? {fx: node.x, fy: node.y} : {fx: fix[0], fy: fix[1]}"}
       {:trigger "!fix", :modify "node", :values "{fx: null, fy: null}"}],
      :encode
      {:enter {:fill {:scale "color", :field "group"}, :stroke {:value "white"}, :size {:field "size"}},
       :update {:size {:signal "2 * nodeRadius * nodeRadius"}, :cursor {:value "pointer"}}
       },
      :transform
      [{:type "force",
        :iterations 300,
        :restart {:signal "restart"},
        :static {:signal "static"},
        :signal "force",
        :forces
        [{:force "center", :x {:signal "cx"}, :y {:signal "cy"}}
         {:force "collide", :radius {:signal "nodeRadius"}}
         {:force "nbody", :strength {:signal "nodeCharge"}}
         {:force "link", :links "link-data", :distance {:signal "linkDistance"}}]}]}
     {:type "path",
      :from {:data "link-data"},
      :interactive false,
      :encode {:update {:stroke {:value "#ccc"}, :strokeWidth {:value 0.5}}},
      :transform
      [{:type "linkpath",
        :require {:signal "force"},
        :shape "line",
        :sourceX "datum.source.x",
        :sourceY "datum.source.y",
        :targetX "datum.target.x",
        :targetY "datum.target.y"}]}],
    :$schema "https://vega.github.io/schema/vega/v5.json",
    :signals
    [{:name "cx", :update "width / 2"}
     {:name "cy", :update "height / 2"}
     {:name "nodeRadius", :value 8, :bind {:input "range", :min 1, :max 50, :step 1}}
     {:name "nodeCharge", :value -30, :bind {:input "range", :min -100, :max 10, :step 1}}
     {:name "linkDistance", :value 30, :bind {:input "range", :min 5, :max 100, :step 1}}
     {:name "static", :value true, :bind {:input "checkbox"}}
     {:description "State variable for active node fix status.",
      :name "fix",
      :value false,
      :on
      [{:events "symbol:mouseout[!event.buttons], window:mouseup", :update "false"}
       {:events "symbol:mouseover", :update "fix || true"}
       {:events "[symbol:mousedown, window:mouseup] > window:mousemove!",
        :update "xy()",
        :force true}]}
     {:description "Graph node most recently interacted with.",
      :name "node",
      :value nil,
      :on [{:events "symbol:mouseover", :update "fix === true ? item() : node"}]}
     {:description "Flag to restart Force simulation upon data changes.",
      :name "restart",
      :value false,
      :on [{:events {:signal "fix"}, :update "fix && fix.length"}]}],
    :height 500,
    })

(defn display
  [block-map]
  (oz/view! (spec block-map) :port 1884))

