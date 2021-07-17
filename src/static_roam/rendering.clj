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
    ;; There are packages that will do language-specific highlighting, don't care at the moment.
    [:code.codeblock content]))

(defn page-link
  [page & {:keys [alias class]}]
  {:pre [(have? bd/block? page)]}
  (let [page-id (:id page)]
    (if (bd/displayed? page)
      [:a (u/clean-map
           {:href (utils/html-file-title page-id)
            ;; TODO behavior with empties should be configurable, I keep
            ;; changing my own mind about it.
            :class (str/join " "
                             (filter identity
                                     (list (when (bd/page-empty? page) "empty")
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

(defn ele->hiccup
  [ast-ele block-map & [block]]
  (utils/debuggable
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
                           (sidenote block-map ref-block)
                           [:div.block-ref
                            (block-hiccup ref-block block-map)]))
            :hashtag (page-link-by-name block-map 
                                        (utils/format-hashtag ele-content))
            :strikethrough [:s (recurse (utils/remove-double-delimiters ele-content))]
            :highlight [:mark (recurse (utils/remove-double-delimiters ele-content))]
            :italic `[:i ~@(maybe-conc-string (nrecurse (subvec ast-ele 2 (- (count ast-ele) 1))))]
            :bold `[:b ~@(maybe-conc-string (nrecurse (subvec ast-ele 2 (- (count ast-ele) 1))))]
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
            ;; See https://www.mathjax.org/ This produces an inline LaTex rendering.
            :latex [:span.math.display (str "\\(" (utils/remove-double-delimiters ele-content) "\\)")]
            )))))))

;;; Used for converting things like italics in blocknames
(defn block-content->hiccup
  [block-content]
  (ele->hiccup (parser/parse-to-ast block-content) {}))

;;; In lieu of putting this in the blockmap
(u/defn-memoized block-hiccup
  "Convert Roam markup to Hiccup"
  [block block-map]
  (if (:parsed block)
    (let [basic (ele->hiccup (:parsed block) block-map block)]
      (if (> (:heading block -1) 0)
        [(keyword (str "h" (:heading block))) basic]
        basic))
    (do
      (prn "Missing content: " (:id block))
      nil)))

;;; TODO this doesn't work, should be using block-id rather than page titles
;;; Static-roam fucks this up...sigh, should have rolled my own from the start
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

(defn block-full-text
  [block-map block]
  (str/join " " (cons (block-local-text block)
                      (map #(block-full-text block-map (get block-map %))
                           (:children block)))))


