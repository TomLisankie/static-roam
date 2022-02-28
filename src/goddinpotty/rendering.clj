(ns goddinpotty.rendering
  (:require [goddinpotty.config :as config]
            [goddinpotty.parser :as parser]
            [goddinpotty.batadase :as bd]
            [goddinpotty.graph :as graph]
            [goddinpotty.utils :as utils]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [org.parkerici.multitool.core :as u]
            [taoensso.truss :as truss :refer (have have! have?)]
            )
  )

;;; Turn parsed content into hiccup. 

(declare block-content->hiccup)         ;allow recursion on this
(declare block-full-hiccup)
(declare block-full-hiccup-sidenotes)

;;; Boostrap icons, see css and https://icons.getbootstrap.com/
(defn- icon
  [name]
  [:i {:class (str "bi-" name)}])

;;; "alias" seems like a misnomer, these are mostly external links.
(defn parse-alias
  [alias-content]
  (re-find #"(?s)\[(.+?)\]\((.+?)\)" alias-content))

(defn- format-alias
  [alias-content]
  (let [[_ alias-text alias-dest] (parse-alias alias-content)
        internal? (or (= \( (first alias-dest)) (= \[ (first alias-dest)))
        alias-link (if internal?
                     (utils/html-file-title alias-dest)
                     alias-dest)
        content (block-content->hiccup alias-text)]
    (if internal?
      [:a {:href alias-link} content]
      [:a.external {:href alias-link} content])))

;;; Track local image files so they can be copied to output
(def published-images (atom #{}))

(defn local-path?
  [path]
  ;; TODO there must be better url regexs out there
  (not (re-matches #"[\w-]+\:(.+)" path)))

(defn maybe-publish-image
  [src]
  (when (local-path? src)
    (swap! published-images conj src)))

;;; Logseq stores links to images as "../assets/..." , which breaks
;;; when we have page hierarchy. This is an ugly fix for that problem
(defn root-image-url
  [u]
  (let [[match? base] (re-find #"^\.\.(.*)$" u)]
    (if match?
      base
      u)))

(defn- format-image
  [image-ref-content]
  (let [alt-text (utils/remove-n-surrounding-delimiters 1 (re-find #"\[.*?\]" image-ref-content))
        image-source (utils/remove-n-surrounding-delimiters 1 (re-find #"\(.*?\)" image-ref-content))
        image-source (root-image-url image-source)
        ]
    (maybe-publish-image image-source)
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
;;; Note: this only is relevant to Roam; Logseq uses {{tweet ...}} syntax
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

;;; Extremely smelly. 
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
    ;; There are packages that will do language-specific highlighting, don't care at the moment.
    [:code.codeblock content]))

;;;  page can be page or page-id (requires bm)
(defn page-link
  [opage & {:keys [alias class bm current]}]     
  (let [page (if (string? opage) (get (bd/alias-map bm) opage) opage)
        alias (or alias (and (string? opage) opage)) ;argh
        page-id (:id page)]
    (prn :page-link page-id current alias )
    (if (and page (bd/displayed? page))
      [:a (u/clean-map
           {:href (str "/" (utils/html-file-title page-id))
            ;; TODO behavior with empties should be configurable, I keep
            ;; changing my own mind about it.
            :class (str/join " "
                             (filter identity
                                     (list (when (bd/page-empty? page) "empty")
                                           (when (= current page-id) "self")
                                           class)))})
       (block-content->hiccup (or alias page-id))]
      (do
        ;; This is normal but a sign that target might want to be exposed.
        (prn (str "ref to excluded page: " (or page-id opage)))
        [:span.empty
         (block-content->hiccup (or alias page-id))]))))

(defn page-link-by-name
  [bm page-name & rest]
  (let [bm (bd/alias-map bm)]
    (cond (contains? bm page-name)
        (apply page-link page-name :bm bm rest)
        (empty? bm)                     ;this can happen when a page title contains a link. (???)
        [:span page-name]
        :else
        [:span.missing page-name]       ;In Logseq world, this happens if you have a single link to a non-existant (empty) page
        )))

(declare block-hiccup)

;;; Blocks used as sidenotes get recorded here so they skip their normal render
;;; Yes this is global state and bad practice. Shoot me.
(def sidenotes (atom #{}))

(defn sidenote
  [block-map sidenote-block]
  (swap! sidenotes conj (:id sidenote-block))
  [:span
   [:span.superscript]
   [:div.sidenote                     ;TODO option to render on left/right, but
    [:span.superscript.side]
    (block-full-hiccup-sidenotes (:id sidenote-block) block-map)]])

;;; Not strictly necessary, but makes tests come out better
(defn- maybe-conc-string
  [seq]
  (if (every? string? seq)
    [(str/join seq)]
    seq))

;;; Smelly
(defn new-head
  [thing head]
  (if (vector? thing)
    (assoc thing 0 head)
    [head thing]))
  
;;; Does most of the real work of rendering.
(defn ele->hiccup
  [ast-ele block-map & [block]]
  (utils/debuggable                     ;TODO for dev, but for production it should just render an error box rather than crapping out. 
   :ele->hiccup [ast-ele]
   ;; TODO this approach is broken, it hides page-refs within italics. 
   (letfn [(recurse [s]                 ;TODO probably needs a few more uses
             (ele->hiccup (parser/parse-to-ast s) block-map block))
           (nrecurse [seq]
             (mapv #(if (string? seq)
                      seq
                      (ele->hiccup % block-map block))
                    seq))] 
     (if (string? ast-ele)
       ast-ele
       (let [ele-content (second ast-ele)]
         (unspan
          (case (first ast-ele)
            nil nil

            ;; Might want this old behavior under a flag, but definitely not wanted in Logseq context.
;;            :metadata-tag [:b [:a {:href (utils/html-file-title ele-content)}
;;                               (subs ele-content 0 (dec (count ele-content)))]]
            :block-property nil         ;These aren't included in output
            :prop-block nil
            :heading (let [base (ele->hiccup (nth ast-ele 2) block-map)]
                       (case (count (second ast-ele))
                         1 [:h1 base]
                         2 [:h2 base]
                         3 [:h3 base]))
            :page-link (page-link-by-name block-map (utils/remove-double-delimiters ele-content))
            ;; Roam only feature, TODO should convert these to Logseq alliases and remove from parser
            :page-alias (let [[_ page alias] (re-matches #"\{\{alias\:\[\[(.+)\]\](.*)\}\}"
                                                         ele-content)]
                          (page-link-by-name block-map page :alias alias))
            :block-ref (let [ref-block (get block-map (utils/remove-double-delimiters ele-content))]
                         ;; ARGh can't work because of fucking namespace rules. POS!
                         (try 
                           (if (and block (= (bd/block-page block-map ref-block)
                                             (bd/block-page block-map block)))
                             (sidenote block-map ref-block)
                             [:div.block-ref
                              (block-hiccup ref-block block-map)])
                           ;; Specifically, bad block refs will cause this.
                           (catch Throwable e
                             [:div.error "Couldn't render: " (str ast-ele)])))
            :hashtag (let [ht (utils/parse-hashtag ele-content)]
                       (or (bd/special-hashtag-handler block-map ht block)
                           (page-link-by-name block-map ht)))
            :strikethrough [:s (recurse (utils/remove-double-delimiters ele-content))]
            :highlight [:mark (recurse (utils/remove-double-delimiters ele-content))]
            :italic `[:i ~@(maybe-conc-string (nrecurse (rest ast-ele)))]
            :bold `[:b ~@(maybe-conc-string (nrecurse (rest ast-ele)))]
            :alias (format-alias ele-content)
            :image (format-image ele-content)
            :todo [:input {:type "checkbox" :disabled "disabled"}]
            :done [:input {:type "checkbox" :disabled "disabled" :checked "checked"}]
            :code-line [:code (utils/remove-n-surrounding-delimiters 1 ele-content)]
            :code-block (format-codeblock ele-content)
            :youtube (if-let [youtube-id (get-youtube-id ele-content)]
                       (youtube-vid-embed youtube-id)
                       [:span "Non-youtube video" ele-content]) ;TODO temp, do something better
            :bare-url (make-content-from-url ele-content)
            :blockquote (new-head (ele->hiccup ele-content block-map block) :blockquote)
                                        ;ast-ele
            :block (let [contents (filter identity (map #(ele->hiccup % block-map block) (rest ast-ele)))
                         ;; bring this property up to a useful level
                         ;; TODO should perhaps be done elsewhere and/or in a more general way
                         class (some :class (:dchildren block))]
                     (cond (empty? contents)
                           nil
                           class
                           `[:span {:class ~class} ~@contents]
                           (> (count contents) 1) 
                           `[:span ~@contents]
                           :else
                           (first contents)))
            :block-embed `[:pre "Unsupported: " (str ast-ele)] ;TODO temp duh
            :hr [:hr]
            ;; See https://www.mathjax.org/ This produces an inline LaTex rendering.
            :latex [:span.math.display (str "\\(" (utils/remove-double-delimiters ele-content) "\\)")]
            :tweet (embed-twitter (second (second ast-ele)))
            )))))))

;;; Used for converting things like italics in blocknames
(defn block-content->hiccup
  [block-content]
  (ele->hiccup (parser/parse-to-ast block-content) {}))

;;; Total hack because I failed to figure this out in instaparse, and its a one-shot thing...
(defn remove-logseq-title
  [[_ h1 title h2 :as parsed]]
  (if (and (= h1 [:hr "---"])
           (= h2 [:hr "---"])
           (re-matches #"(?m)^\ntitle\: (.*)\n$" title))
    [:block]
    parsed))

;;; In lieu of putting this in the blockmap
(u/defn-memoized block-hiccup
  "Convert Roam markup to Hiccup"
  [block block-map]
  (when (:parsed block)
    (let [parsed (remove-logseq-title (:parsed block))
          basic (ele->hiccup parsed block-map block)]
      (cond (and (:heading block) (> (:heading block) 0))
            [(keyword (str "h" (:heading block))) basic]
            (fn? basic)                 ;#incoming uses this hack, maybe others
            (basic block-map)
            :else
            basic))
    ))

;;; TODO this doesn't work, should be using block-id rather than page titles
;;; goddinpotty fucks this up...sigh, should have rolled my own from the start
(defn roam-url
  [block-id]
  (str (config/config :roam-base-url) block-id))

 ;Has to do full hiccup to include children
(defn block-full-hiccup-sidenotes
  [block-id block-map & [depth]]
  {:pre [(have? string? block-id)]}
  (let [depth (or depth 0)
        block (get block-map block-id)]
    (when (bd/displayed? block)
      [:ul {:id block-id :class (if (< depth 2) "nondent" "")} ;don't indent the first 2 levels
       "\n"
       [:li.block
        (when (config/config :dev-mode)
          [:a.edit {:href (roam-url block-id)
                    :target "_roam"}
           (icon "pencil")
           ])           
        (when-not (bd/included? block)
          [:span.edit 
           (icon "file-lock2")])
        (when-not (:page? block)        ;Page content is title and rendered elsewhere
          (block-hiccup block block-map))]
       (map #(block-full-hiccup % block-map (inc depth))
            (:children block))])))

(defn sidenote?
  [id]
  (contains? @sidenotes id))

;;; The real top-level call
(defn block-full-hiccup
  [block-id block-map & [depth]]
  (when-not (sidenote? block-id)
    (block-full-hiccup-sidenotes block-id block-map depth)))

(defn page-hiccup
  [block-id block-map]
  (block-full-hiccup block-id block-map))

(defn url?
  [s]
  (re-matches #"(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})" s))

(defn block-local-text
  "just the text of a block, formatting removed"
  [block]
  (letfn [(text [thing]
            (cond (string? thing)
                  (if (url? thing)
                    ()
                    (list thing))
                  (map? thing)
                  ()
                  (vector? thing)
                  (mapcat text (rest thing))
                  :else
                  ()))]
    (str/join "" (text (block-hiccup block {})))))

;;; Used for search index
(defn block-full-text
  [block-map block]
  (if (bd/displayed? block)
    (str/join " " (cons (block-local-text block)
                        (map #(block-full-text block-map (get block-map %))
                             (:children block))))
    ""))


