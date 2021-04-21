(ns static-roam.rendering
  (:require [static-roam.config :as config]
            [static-roam.parser :as parser]
            [static-roam.batadase :as bd]
            [static-roam.utils :as utils]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [org.parkerici.multitool.core :as u]
            [taoensso.truss :as truss :refer (have have! have?)]
            )
  )

;;; Turn parsed content into hiccup. 
(declare block-content->hiccup)         ;allow recursion on this

;;; TODO "alias" seems like a misnomer, these are mostly external links.
(defn- format-alias
  [alias-content]
  (let [[_ alias-text alias-dest] (re-find #"(?s)\[(.+?)\]\((.+?)\)" alias-content)
        internal? (or (= \( (first alias-dest)) (= \[ (first alias-dest)))
        alias-link (if internal?
                     (utils/html-file-title alias-dest)
                     alias-dest)
        content (block-content->hiccup alias-text)]
    (if internal?
      [:a {:href alias-link} content]
      [:a.external {:href alias-link} content])))

(defn- format-image
  [image-ref-content]
  (let [alt-text (utils/remove-n-surrounding-delimiters 1 (re-find #"\[.*?\]" image-ref-content))
        image-source (utils/remove-n-surrounding-delimiters 1 (re-find #"\(.*?\)" image-ref-content))]
    ;; Link to
    [:a.imga {:href image-source :target "_image"} ;cheap way to get expansion. TODO link highlighhting looking slightly crufty
     [:img {:src image-source :alt alt-text}]]))

(defn youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [youtube-id]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube.com/embed/" youtube-id)
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
            :allowfullscreen ""}])

(defn get-youtube-id
  [string]
  (or (second (re-find #"https\:\/\/youtu\.be/([\w_-]*)" string))
      (second (re-find #"https\:\/\/www.youtube.com\/watch\?v=([\w_-]*)" string))))

;;; Not used much
(defn- make-link-from-url
  [string]
  [:a.external {:href string} string])

;;; Tried to build this into instaparse parser, but couldn't make it take precedence over :bare-url
(defn- twitter-url?
  [url]
  (re-matches #"https:\/\/twitter.com\/\S*" url))

(defn- embed-twitter
  [url]
  (try
    (let [oembed (json/read-str (slurp (str "https://publish.twitter.com/oembed?url=" url)) :key-fn keyword)]
      (:html oembed))
    (catch Throwable e
      (prn :twitter-embed-failed e)
      (make-link-from-url url))))

(defn- make-content-from-url
  [url]
  (cond (twitter-url? url)                ;TODO if there are more
        (embed-twitter url)
        (get-youtube-id url)
        (youtube-vid-embed (get-youtube-id url))
        :else
        (make-link-from-url url)))

(defn unspan
  "Remove :span elts that are basically no-ops. Would be cleaner to not generate"
  [hiccup]
  (if (and (vector? hiccup)
           (= :span (first hiccup))
           (= 2 (count hiccup)))
    (second hiccup)
    hiccup))

(defn format-codeblock
  [spec]
  (let [[_ _lang content] (re-matches #"(?sm)```(\w*)\n(.*)```\s*" spec)]
    ;; TODO there are packages that will do language-specific highlighting
    [:code.codeblock content]))

;;; Following 2 fns dup from database until I can untangle the ns mess
;;; And they have diverged...argh TODO
(def size
  (memoize
   (fn [page]
     (reduce +
             (count (:content page 0))
             (map size
                  (filter :include? (:dchildren page)))))))

(defn page-empty?
  [page]
  (when-not (:special? page)
    (< (- (size page)
          (count (:id page)))
       10)))

;;; God damn it, had to dupe this from database
(defn displayed?
  [block]
  (or (:include? block)
      (config/config :unexclude?)))

(defn page-link
  [page & {:keys [alias class]}]
  (let [page-id (:id page)]
    (if (displayed? page)
      [:a (u/clean-map
           {:href (utils/html-file-title page-id)
            ;; TODO behavior with empties should be configurable, I keep
            ;; changing my own mind about it.
            :class (str/join " "
                             (filter identity
                                     (list (when (page-empty? page) "empty")
                                           class)))})
       (block-content->hiccup (or alias page-id))]
      (do
        (prn "ref to excluded page " page-id)
        (block-content->hiccup (or alias page-id))))))

(defn page-link-by-name
  [bm page-name & rest]
  (cond (contains? bm page-name)
        (apply page-link (get bm page-name) rest)
        (empty? bm)                     ;this can happen when a page title contains a link.
        [:span page-name]
        :else
        [:span.missing "Missing link: " page-name] ;; Turns out Roam can have links to nonexistant pages, eg from Import
        ))

(declare block-hiccup)

;;; map from containing id to seq of sidenote ids
;;; gets reset at start of run 
(def *sidenotes* (atom {}))  

(defn record-sidenote
  [block-map sidenote-block containing-block]
  (let [anchor-block containing-block]
    (swap! *sidenotes* update-in [(:id anchor-block)] conj (:id sidenote-block))))

(defn- ele->hiccup
  [ast-ele block-map & [block]]
  (utils/debuggable
   :ele->hiccup [ast-ele]
   (let [recurse (fn [s]                 ;TODO probably needs a few more uses
                   (ele->hiccup (parser/parse-to-ast s) block-map block))] ; used to be, do we really need that ? 
     (if (string? ast-ele)
       ast-ele
       (let [ele-content (second ast-ele)]
         (unspan
          (case (first ast-ele)
            nil nil
            :metadata-tag [:b [:a {:href (utils/html-file-title ele-content)}
                               (subs ele-content 0 (dec (count ele-content)))]]
            :page-link (page-link-by-name block-map (utils/remove-double-delimiters ele-content))
            :page-alias (let [[_ page alias] (re-matches #"\{\{alias\:\[\[(.+)\]\](.*)\}\}"
                                                         ele-content)]
                          (page-link-by-name block-map page :alias alias))
            :block-ref (let [ref-block (get block-map (utils/remove-double-delimiters ele-content))]
                         ;; ARGh can't work because of fucking namespace rules. POS!
                         (if (and block (= (bd/block-page block-map ref-block)
                                           (bd/block-page block-map block)))
                           (do (record-sidenote block-map ref-block block)
                               [:span.superscript]) 
                           [:div.block-ref
                            (:hiccup ref-block)]))
            :hashtag [:a {:href (utils/html-file-title ele-content)}
                      (utils/format-hashtag ele-content)]
            :strikethrough [:s (recurse (utils/remove-double-delimiters ele-content))]
            :highlight [:mark (recurse (utils/remove-double-delimiters ele-content))]
            :italic [:i (recurse (utils/remove-double-delimiters ele-content))]
            :bold [:b (recurse (utils/remove-double-delimiters ele-content))]
            :alias (format-alias ele-content)
            :image (format-image ele-content)
            :todo [:input {:type "checkbox" :disabled "disabled"}]
            :done [:input {:type "checkbox" :disabled "disabled" :checked "checked"}]
            :code-line [:code (utils/remove-n-surrounding-delimiters 1 ele-content)]
            :code-block (format-codeblock ele-content)
            :youtube (youtube-vid-embed
                      (or (get-youtube-id ele-content)
                          (throw (ex-info "Couldn't find youtube id" {:string ele-content}))))
            
            :bare-url (make-content-from-url ele-content)
            :blockquote [:blockquote (ele->hiccup ele-content block-map block)]
                                        ;ast-ele
            :block `[:span ~@(map #(ele->hiccup % block-map block) (rest ast-ele))]
            :block-embed `[:pre "Unsupported: " (str ast-ele)] ;TODO temp duh
            :hr [:hr]
            )))))))

;;; Used for converting things like italics in blocknames
(defn block-content->hiccup
  [block-content]
  (ele->hiccup (parser/parse-to-ast block-content) {}))

(defn block-hiccup
  "Convert Roam markup to Hiccup"
  [block block-map]
  (if (:parsed block)
    (let [basic (ele->hiccup (:parsed block) block-map block)]
      (if (> (:heading block) 0)
        [(keyword (str "h" (:heading block))) basic]
        basic))
    (do
      (prn "Missing content: " (:id block))
      nil)))

;;; This renders individual blocks into hiccup. Blocks and their children is handled by block-full-hiccup and friends below,
(defn render
  [bm]
  (reset! *sidenotes* {})
  ;; Sometimes render incorporates other blocks; the memoize tries to ensure that the render is just done once. 
  (let [render-block (memoize #(block-hiccup % bm))]
    (u/map-values #(if (displayed? %)
                        (assoc % :hiccup (render-block %))
                        %)
                  bm)))

(defn roam-url
  [block-id]
  (str (config/config :roam-base-url) block-id))

(declare render-sidenotes)
(declare block-full-hiccup)

 ;Has to do full hiccup to include children
(defn block-full-hiccup-sidenotes
  [block-id block-map & [depth]]
  {:pre [(have? string? block-id)]}
  (let [depth (or depth 0)]
    (let [block (get block-map block-id)
          base
          [:ul {:id block-id :class (if (< depth 2) "nondent" "")} ;don't indent the first 2 levels
           (if (or (nil? (:hiccup block))                          ;TODO ech
                   (= (:content block) block-id))
             nil
             [:li.block
              (when (config/config :dev-mode)
                [:a.edit {:href (roam-url block-id)
                          :target "_roam"}
                 "[e]"])                      ;TODO nicer icons
              (when-not (:include? block)
                [:span.edit 
                 "[X]"])
              (:hiccup block)
              ])
           (map #(block-full-hiccup % block-map (inc depth))
                (:children block))
           ]]
      (if-let [sidenotes (get @*sidenotes* block-id)]
        [:div
         (render-sidenotes block-map sidenotes)
         base]
        base
        ))))

(defn render-sidenotes
  [block-map sidenotes]
  (for [s sidenotes]
    [:div.sidenote
     [:span.superscript.side]
     (block-full-hiccup-sidenotes s block-map 1)]))

(defn sidenote?
  [id]
  ;; TODO want a once macro
  (contains? ((memoize (fn [] (set (flatten (vals @*sidenotes*)))))) id))

;;; The real top-level call
(defn block-full-hiccup
  [block-id block-map & [depth]]
  (when-not (sidenote? block-id)
    (block-full-hiccup-sidenotes block-id block-map depth)))
