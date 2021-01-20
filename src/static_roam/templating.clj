(ns static-roam.templating
  (:require [clojure.string :as s]
            [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]))

;;; TODO Note: the functions of templating and html-gen seem to overlap; not sure they should be separate.

(def site-css "../../resources/public/hyper-roam.css") ;TODO make this user settable somehow

(defn- metadata-properties
  [metadata]
  (into (hash-map) (filter #(= 2 (count %)) (map #(s/split % #":: ") metadata))))

(defn- site-metadata
  [block-map]
  (let [property-block-ids (:children (get block-map "SR Metadata"))
        property-block-content (map #(:content (get block-map %)) property-block-ids)
        prop-val-dict (metadata-properties property-block-content)]
    prop-val-dict))

(defn- nav-bar-page-dict
  [block-map]
  (let [site-metadata-dict (site-metadata block-map)
        nav-bar-page-string (get site-metadata-dict "Nav Bar")
        nav-bar-pages-uncleaned (into
                                 (utils/find-content-entities-in-string nav-bar-page-string)
                                 (utils/find-hashtags-in-string nav-bar-page-string))
        nav-bar-pages (map utils/remove-double-delimiters nav-bar-pages-uncleaned)
        ;; TODO ugly but works to get italics in titles rendered properly. Should do same for backlinks
        nav-bar-pages-r (map #(static-roam.parser/block-content->hiccup % {}) 
                             nav-bar-pages)
        nav-bar-hrefs (map #(utils/page-title->html-file-title % true) nav-bar-pages)
        nav-bar-page-dict (zipmap nav-bar-hrefs nav-bar-pages-r)]
    nav-bar-page-dict))

(defn page-hiccup
  [body-hiccup page-title block-map]
  [:html
   `[:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title ~page-title]               ;TODO might want to prepend a site title
     [:link {:rel "stylesheet"
              :href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
              :integrity "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
              :crossorigin "anonymous"}]
      [:link {:rel "stylesheet" :href ~site-css}] 
      [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
      [:link {:rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap"}]]
   [:body
    [:header.site-header
     [:div.wrapper
      [:nav.navbar.navbar-expand-lg.navbar-light ; .bg-dark.navbar-dark
       (into
       [:ul.navbar-nav.mr-auto]
       (map (fn [[url title]] [:li.nav-item 
                               [:a.nav-link {:href url}
                                title
                                ]])
            (nav-bar-page-dict block-map)))]]]
    [:div.container.main
     body-hiccup]]])

(defn children-of-block-template
  [block-id block-map]
  (let [properties (get block-map block-id)]
    [:ul
     (if (or (nil? (:hiccup properties))
             (= (:content properties) block-id))
       ""
       ;;; TODO Turned off block clicks, may want it under a flag or something
       [:li.block #_ {:onclick (str "location.href='" (utils/page-title->html-file-title block-id :case-sensitive) "'")}
        (:hiccup properties)])
     (let [children (:children properties)]
       (if (not= 0 (count children))
         ;; recurse on each of the children
         (map #(children-of-block-template % block-map) children)
         ;; otherwise, evaluate to empty div
         [:div]))]))

(defn- find-parent
  [block-id block-map]
                                        ; (first (filter #(not (nil? %)) (map #(is-parent block-id %) block-map)))
  (get-in block-map [block-id :parent]))

;;; TODO this should prob be built into block map, or cached.
(defn find-page
  [block-id block-map]
  (if-let [parent (find-parent block-id block-map)]
    (find-page parent block-map)
    block-id))

(defn- get-parent
  [block-id block-map]
  (let [parent-id (find-parent block-id block-map)]
    (if (nil? parent-id)
      ""
      [:a {:href (str (utils/page-title->html-file-title
                       parent-id
                       :case-sensitive))}
       (:content (get block-map parent-id))])))

(defn page-link [page]
  [:a {:href (utils/page-title->html-file-title page :case-sensitive)}
   (parser/block-content->hiccup page {})]) ;TODO ugly but does the italic thing right

(defn linked-reference-template
  [block-map r]
  (if (nil? (get block-map r))
    ""                                  ;TODO ugly
    (let [page (find-page r block-map)]
      [:div
       "From " (page-link page)
       [:div (children-of-block-template r block-map)]])))

(defn linked-references-template
  [references block-map]
  (concat []
          (map (partial linked-reference-template block-map) references)))

(defn block-page-template
  [block-id block-map]
  (let [block (get block-map block-id)
        block-content (get block :content)]
    [:div
     [:h1.title (parser/block-content->hiccup block-content block-map)]    ; (vec (map #(parser/ele->hiccup % block-map) (parser/parse-to-ast block-content)))]
     (if (or (:exit-point block)
             (empty? (:children block)))
       [:div.missing
        "This page does not yet exist!"] ;TODO ok this is sucky UX, the links themselves should look or act differently.
       [:div (children-of-block-template block-id block-map)])
     (let [linked-refs (database/get-linked-references block-id block-map)]
       (when-not (empty? linked-refs)
         [:div.incoming
          [:h3 "Incoming links"]
          (linked-references-template linked-refs block-map)]))]))

(defn list-of-page-links
  "Generate a Hiccup unordered list of links to pages"
  ([page-titles]
   (let [page-links (map utils/page-link-from-title page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir]                    ;TODO dir not used
   (let [page-links (map #(utils/page-link-from-title %) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir link-class]
   (let [page-links (map #(utils/page-link-from-title dir % link-class) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links)))))

(defn home-page-hiccup
  [entry-points block-map]
  (page-hiccup 
   [:main.page-content {:aria-label "Content"}
     [:div.wrapper
       [:h2.post-list-heading "Entry Points"]
      (list-of-page-links
       (sort (keys entry-points)) "pages" "entry-point-link")
      ]]
      (get (site-metadata block-map) "Title")
      block-map))


