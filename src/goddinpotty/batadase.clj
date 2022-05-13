(ns goddinpotty.batadase
  (:require [goddinpotty.utils :as utils]
            [goddinpotty.config :as config]
            [org.parkerici.multitool.core :as u]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.truss :as truss :refer (have have! have?)]
            ))

;;; Database accessors. The name is a PROTEST against the feature of Clojure I hate most, rigid limits on namespace reference

(defn block? [x]
  (and (map? x)
       (:id x)))

(defn assert-block
  [x]
  (or (block? x)
      (throw (ex-info "Not a block" {:thing x}))))

;;; included? means reachable from an entry point
;;; displayed? means actually generated. Usually these are the same, except when the :unexcluded? config is set, meaning we want to see everything, included or not.

(defn included?
  ([block]
   (assert-block block)
   (:include? block))
  ([block-map block-id]
   (included? (get block-map block-id))))

(defn displayed?
  ([block]
   (assert-block block)
   (if (config/config :unexclude?)
     true
     (:include? block)))
  ([block-map block-id]
   (displayed? (get block-map block-id))))

(defn- get-linked-references
  [block-id block-map]
  (filter #(get-in block-map [% :id])      ;trying to filter out bogus entries, not working
          (get-in block-map [block-id :linked-by])))

(defn get-displayed-linked-references
  [block-id block-map]
  (filter (partial displayed? block-map)
          (get-linked-references block-id block-map)))

;;; Some new accessors

(def descendents
  (u/transitive-closure :dchildren))

;;; TODO DWIMish. Maybe turn into a macro and use everywhere. Or put the map in a global, I don't care if that's bad Clojure
(defn coerce-block
  [b block-map]
  (if (block? b)
    b
    (or (get block-map b)
        (throw (ex-info "Not a block" {:thing b})))))

(defn block-parent
  [block-map block]
  (let [block (coerce-block block block-map)]
    (and (:parent block)
         (get block-map (:parent block)))))

(defn ancestors0
  [block-map block]
  (cons block (if-let [parent (block-parent block-map block)]
                (ancestors0 block-map parent)
                nil)))

(defn block-ancestors
  [block-map block]
  (let [block (coerce-block block block-map)]
    (rest (ancestors0 block-map block))))

(defn block-children
  [block-map block]
  (map block-map (:children block)))

(defn block-descendents
  [block]
  ((u/transitive-closure :dchildren) block))

(defn block-contains?
  [b1 b2]
  (contains? (set (map :id (descendents b1))) (:id b2)))

(defn forward-page-refs
  "Forward page refs. Returns set of ids"
  [page]
  (apply clojure.set/union
         (map :refs (filter displayed? (block-descendents page)))))

(defn block-page
  [block-map block]
  (let [block (coerce-block block block-map)]
    (if-let [parent (block-parent block-map block)]
      (block-page block-map parent)
      block)))

(defn backward-page-refs
  [bm page]
  (map :id
       (filter displayed?
               (map (comp (partial block-page bm) bm)
                    (:linked-by page)))))

(defn page-refs
  [bm page]
  (set/union (forward-page-refs page)
             (backward-page-refs bm page)))

(defn pages
  [block-map]
  (filter :page? (vals block-map)))

(defn included-blocks
  [block-map]
  (filter included? (vals block-map)))

(defn included-pages
  [block-map]
  (filter included? (pages block-map)))

(defn displayed-pages
  [block-map]
  (filter displayed? (pages block-map)))

(defn displayed-blocks
  [block-map]
  (filter displayed? (vals block-map)))



(defn displayed-regular-pages
  [block-map]
  (remove :special? (displayed-pages block-map)))

(declare alias-map)

(defn tagged?
  [block-map block tag]
  (let [tag-id (get (alias-map block-map) tag)]
  (or (contains? (:refs block) tag-id)
      ;; This implements the somewhat weird convention that tags are done in contained elts, eg
      ;; - Some private stuff
      ;;   - #Private
      ;; partly for historical reasons and partly so pages can be tagged
      (some #(contains? (:refs %) tag-id)
            (block-children block-map block)))))

(defn tagged-or-contained?
  [block-map block tag]
  (and block
       (or (tagged? block-map block tag)
           (tagged-or-contained? block-map (block-parent block-map block) tag))))

;;; These are the top-level entrypoints, other than those defined by tags. Crude at the moment
(defn advertised-page-names
  []
  (conj
   (set (map (fn [e]
              (if (vector? e) (second e) e))
             (config/config :right-navbar)))
   (config/config :main-page)))

(defn entry-point?
  "Determines whether or not a given page is tagged with #EntryPoint in its first child block"
  [block-map block]
  ;; TODO this should be done in logseq import, like convert this to a pseudo-tag
  (or (:public? block)                  ;the logseq block property
      (some #(tagged? block-map block %)
            (config/config :entry-tags))
      (get (advertised-page-names) (:title block))))

(defn entry-points
  [block-map]
  (filter (partial entry-point? block-map) (pages block-map)))

(def daily-notes-regex #"(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d+.., \d+")

(defn daily-notes?
  [block-map block]
  (let [page (block-page block-map block)
        title (or (:title page) (:content page))]
    (when title (re-matches daily-notes-regex title))))

(defn exit-point?
  [block-map block]
  (or (some #(tagged-or-contained? block-map block %)
            (config/config :exit-tags))
      (and (not (config/config :daily-notes?))
           (daily-notes? block-map block))))

;;; Temp
(def min* (partial u/min-by identity))
(def max* (partial u/max-by identity))

;;; Prob needs to deal with missing data
;;; I suppose the median time might be more informative – or an Ed Tufte minigraph

;;; Some bogus dates creep in somehow, this removes them (TODO figure this out better)
(def earliest-date  #inst "2020-01-01T00:00")

(defn date-range [page]
  (let [blocks (block-descendents page)
        visible-blocks (filter displayed? blocks)
        visible-dates (filter (partial u/<* earliest-date)
                              ;; TODO should use both
                              (map (some-fn :edit-time :create-time) visible-blocks))]
    [(min* visible-dates) (max* visible-dates)]))

(defn stats [bm]
  {:blocks {:total (count bm)
            :published (count (filter :include? (vals bm)))}
   :pages {:total (count (pages bm))
           :published (count (filter :include? (pages bm)))}})


;;; These could be part of the map but it's easier this way

(u/defn-memoized edit-time
  [page]
  (second (date-range page)))

(u/defn-memoized size
  [page]
  (reduce +
          (count (:content page ""))
          (map size
               (filter displayed? (:dchildren page)))))

(defn page-empty?
  [page]
  (and (not (:special? page))
       (< (size page)                   ;TODO might need tweaking, used to take title size into account?
          10)))

(defn expand-to [block-map block minsize]
  (cond (>= (size block) minsize)
        block
        (nil? (:parent block))
        block
        :else
        (expand-to block-map (get block-map (:parent block)) minsize)))

(defn leaf?
  [block]
  (empty? (:children block)))

(defn all-refs [block]
  (set/union
   (set (:children block))
   (set (:refs block))
   (set (:linked-by block))
   (set (and (:parent block) (list (:parent block))))))

;;; Special tag handling. Here for no very good reason

(def special-tags (atom {}))

(defn register-special-tag
  [tag handler]
  (swap! special-tags assoc tag handler))

(defn special-hashtag-handler
  [bm ht block]
  #_ (when (contains? @special-tags ht) (prn :ht ht (:id block)))
  (when-let [handler (get @special-tags ht)]
    (handler bm block)))

;;; Redone now that we create parents
;;; TODO This would be a good place to add flags that cause the incoming links to render as main body
(defn add-empty-pages
  [bm]
  (u/map-values
   (fn [block]
     (if (and (:include? block)
              (not (:parent block))
              (not (:page? block)))
              ;; If there arent multiple incoming links, really no point in having a page
       (if (> (count (:linked-by block)) 1)
         (do
           (prn :add-empty-page (:id block))
           (assoc block :page? true :title (:id block)))
         (assoc block :include? false))
       block))
   bm))

;;;  in multiool 0.19
(defn merge-recursive
  "Merge two arbitrariy nested map structures. Terminal seqs are concatentated, terminal sets are merged."
  [m1 m2]
  (cond (and (map? m1) (map? m2))
        (merge-with merge-recursive m1 m2)
        (and (set? m1) (set? m2))
        (set/union m1 m2)
        (and (sequential? m1) (sequential? m2))
        (concat m1 m2)
        (nil? m2) m1
        :else m2))

;;; → multitool
(defn collecting-merge
  "Exec is a fn of one argument, which is called and passed another fn it can use to collect values which are merged with merge-recursive; the result is returned. See tests for example TODO" 
  [exec]
  (let [acc (atom {})
        collect #(swap! acc merge-recursive %)]
    (exec collect)
    @acc))

;;; All legit names of block, main and aliases
(defn block-names
  [block]
  (if (:title block)
    (conj (:alias block) (:title block))
    (:alias block)))

(u/defn-memoized alias-map
  "Return map of aliases to real page ids"
  [bm]
  (collecting-merge
   (fn [collect]
     (doseq [block (vals bm)]
         (doseq [alias (block-names block)]
           (collect {alias (:id (block-page bm block))})
           )))))

(defn get-with-aliases
  ([bm page-name]
   (get-with-aliases bm  (alias-map bm) page-name))
  ([bm aliases page-name]
   (or (get bm page-name)
       (get bm (get aliases page-name)))))

;;; → Multitool? 
(defn vec->maps
  [v]
  (if (empty? v)
    nil                                 ;Note: other values won't work!
    {(first v) (vec->maps (rest v))}))

;;; Handles multilevel 
(defn- page-hierarchies-1
  [page-names]
  (collecting-merge
   (fn [collect]
     (doseq [title page-names]
       (when (re-find #"/" title)
         (prn  (vec->maps (str/split title #"/")))
         (collect (vec->maps (str/split title #"/"))))))))

(u/defn-memoized page-hierarchies ;only need to compute this once
  [bm]
  (page-hierarchies-1 (map :title (pages bm))))


(defn page-in-hierarchy?
  [page bm]
  (or (and (:title page)
           (re-find #"^(.*)/(.*?)$" (:title page)))
      (get (page-hierarchies bm) (:title page))))  ;top page

;;; Table o' contents

;;; This must exist? core.match, but not quite
;;; https://github.com/dcolthorp/matchure

;;; Note: this is minimal, and in some cases just wrong, but good enoug
;;; → multitool
(defn extend-seq
  [seq]
  (concat seq (repeat nil)))

;;; → multitool
(defn str-match
  [pat thing]
  (cond (and (list? pat) (= '? (first pat)))
        (if thing
          {(keyword (second pat)) thing}
          nil)
        (and (sequential? pat) (sequential? thing))
        (reduce (fn [a b] (and a b (merge a b)))
                {}
                (map str-match pat (extend-seq thing)))
        (= pat thing) {}
        :else nil))

;;; Page table of contents generation

(defn- toc-1
  [block]
  (let [head (when-let [{:keys [depth content]}
                        (and (displayed? block)
                             (str-match '[:block [:heading (? depth) (? content)]]
                                        (:parsed block)))]
               (when (zero? (count depth)) (prn :foo block))
               [(count depth) (:id block)]) ; might be nice to add rendered text but namespace fucks us, it's render/block-locak-text
        rest
        (filter identity (map toc-1 (:dchildren block)))]
    (if head
      (cons head rest)
      (if (empty? rest)
        nil
        rest))))

(defn toc
  [block]
  (partition 2 (flatten (toc-1 block))))


          
          
          
        
