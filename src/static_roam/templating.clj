(ns static-roam.templating
  (:require [static-roam.utils :as utils]
            [static-roam.parser :as parser]
            [static-roam.database :as database]
            [datascript.core :as ds]
            [clojure.pprint :as pprint]))

(defn page-hiccup
  [body-hiccup site-title nav-bar-page-dict css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:title site-title]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body
    [:header.site-header
     [:div.wrapper
      [:a.site-title {:rel "author" :href ".."} site-title]
      (into
       [:div.nav-links]
       (map (fn [pair] [:a.nav-link {:href (first pair)} (second pair)]) nav-bar-page-dict))]]
    body-hiccup]])

(defn children-of-block-template
  [block-id block-map]
  (let [properties (database/get-properties-for-block-id block-id block-map)]
    [:ul
     (if (or (nil? (:hiccup properties)) (= (:content properties) block-id))
       ""
       [:li {:onclick (str "location.href='" (utils/page-title->html-file-title block-id :case-sensitive) "'")} (:hiccup properties)])
     (let [children (:children properties)]
       (if (not= 0 (count children))
         ;; recurse on each of the children
         (map #(children-of-block-template % block-map) children)
         ;; otherwise, evaluate to empty div
         [:div]))]))

(defn linked-references-template
  [references block-map]
  (concat []
          (map
           (fn
             [r]
             (if (nil? (get block-map r))
               ""
               [:li
                {:onclick
                 (str "location.href='"
                      (str ""
                           (utils/page-title->html-file-title
                            r
                            :case-sensitive)) "'" )}
                (:hiccup (get block-map r))]))
           references)))

(defn- is-parent
  [block-id block-kv]
  (if (some #(= block-id %) (:children (second block-kv)))
    (first block-kv)
    nil))

(defn- find-parent
  [block-id block-map]
  (filter #(not (nil? %)) (map #(is-parent block-id %) block-map)))

(defn- get-parent
  [block-id block-map]
  (let [parent-id (first (find-parent block-id block-map))]
    (if (nil? parent-id)
      ""
      [:a {:href (str ""
                      (utils/page-title->html-file-title
                       parent-id
                       :case-sensitive))}
       (:content (get block-map parent-id))])))

(defn block-page-template
  [block-id block-content block-map]
  [:div
   [:div
    [:h3 (get-parent block-id block-map)]]
   [:h2 (vec (map #(parser/ele->hiccup % block-map) (parser/parse-to-ast block-content)))]
   [:div (children-of-block-template block-id block-map)]
   [:div {:style "background-color:lightblue;"}
    [:h3 "Linked References"]
    (linked-references-template (database/get-linked-references block-id block-map) block-map)]])

(defn page-index-hiccup
  [link-list css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body link-list]])

(defn home-page-hiccup
  [link-list title nav-bar-page-dict css-path js-path]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:title title]
    [:link {:rel "stylesheet" :href css-path}]
    [:script {:src js-path}]]
   [:body
    [:header.site-header
     [:div.wrapper
      [:a.site-title {:rel "author" :href "."} title]
      (into
       [:div.nav-links]
       (map (fn [pair] [:a.nav-link {:href (str "./pages" (subs (first pair) 1))} (second pair)]) nav-bar-page-dict))]]
    [:main.page-content {:aria-label "Content"}
     [:div.wrapper
       [:h2.post-list-heading "Entry Points"]
       link-list]]]])

(defn list-of-page-links
  "Generate a Hiccup unordered list of links to pages"
  ([page-titles]
   (let [page-links (map utils/page-link-from-title page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir]
   (let [page-links (map #(utils/page-link-from-title %) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links))))
  ([page-titles dir link-class]
   (let [page-links (map #(utils/page-link-from-title dir % link-class) page-titles)]
     (conj [:ul.post-list ] (map (fn [a] [:li [:h3 a]]) page-links)))))

;; ---------------------------------------------------------------------------------------------------------

(def general-site-info
  {
   "Site Title" "Thomas Lisankie"
   "Nav Bar Pages" ["About", "Now", "Posts", "My Mind", "Contact"]
   "Twitter Profile" "TomLisankie"
   "GitHub Profile" "TomLisankie"
   })

(defn- homepage-head
  [page-title]
  [:head
   [:title page-title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
   [:link {:rel "shortcut icon" :href "./assets/favicon.ico" :type "image/x-icon"}]
   [:meta {:charset "utf-8"}]
   [:link {:rel "stylesheet" :type "text/css" :href "./assets/style.css"}]
   [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
   [:link {:href "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap" :rel "stylesheet"}]])

(defn- head
  [page-title]
  [:head
   [:title page-title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
   [:link {:rel "shortcut icon" :href "../assets/favicon.ico" :type "image/x-icon"}]
   [:meta {:charset "utf-8"}]
   [:link {:rel "stylesheet" :type "text/css" :href "../assets/style.css"}]
   [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
   [:link {:href "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap" :rel "stylesheet"}]])

(defn- header
  []
  [:header
   [:h1 {:id "site-title-link"}
    [:a {:href "../"} (get general-site-info "Site Title")]]
   [:nav {:id "nav-bar"}
    [:input {:type "checkbox" :name "nav-trigger" :id "nav-trigger"}]
    [:label {:for "nav-trigger"}
     [:svg {:viewBox "0 0 100 80" :width "40" :height "40"}
      [:rect {:width "100" :height "20"}]
      [:rect {:y "30" :width "100" :height "20"}]
      [:rect {:y "60" :width "100" :height "20"}]]]
    [:div {:id "nav-links"}
     [:a {:class "nav-bar-link" :href "../about"} "About"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "../now"} "Now"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "../posts"} "Posts"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "../mind"} "My Mind"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "../contact"} "Contact"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "social-icon" :href (str "https://twitter.com/" (get general-site-info "Twitter Profile"))}
      [:img {:src "../assets/twitter.svg" :width "20" :height "20"}]]
     " "
     [:a {:class "social-icon" :href (str "https://github.com/" (get general-site-info "GitHub Profile"))}
      [:img {:src "../assets/github.svg" :width "20" :height "20"}]]]]])

(defn- homepage-header
  []
  [:header
   [:h1 {:id "site-title-link"}
    [:a {:href "./"} (get general-site-info "Site Title")]]
   [:nav {:id "nav-bar"}
    [:input {:type "checkbox" :name "nav-trigger" :id "nav-trigger"}]
    [:label {:for "nav-trigger"}
     [:svg {:viewBox "0 0 100 80" :width "40" :height "40"}
      [:rect {:width "100" :height "20"}]
      [:rect {:y "30" :width "100" :height "20"}]
      [:rect {:y "60" :width "100" :height "20"}]]]
    [:div {:id "nav-links"}
     [:a {:class "nav-bar-link" :href "./about"} "About"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "./now"} "Now"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "./posts"} "Posts"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "./mind"} "My Mind"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "nav-bar-link" :href "./contact"} "Contact"]
     [:span {:class "link-separator"} " | "]
     [:a {:class "social-icon" :href (str "https://twitter.com/" (get general-site-info "Twitter Profile"))}
      [:img {:src "./assets/twitter.svg" :width "20" :height "20"}]]
     " "
     [:a {:class "social-icon" :href (str "https://github.com/" (get general-site-info "GitHub Profile"))}
      [:img {:src "./assets/github.svg" :width "20" :height "20"}]]]]])

(defn- footer
  []
  [:footer
   [:center
    [:div {:class "rc-webring"}
     [:a {:href "https://webring.recurse.com"}
      [:img {:src "https://webring.recurse.com/icon.png" :width "20" :height "20"}
       "RC Webring"]]]
    [:div
     "Want to become a better programmer? "
     [:a {:href "https://www.recurse.com/scout/click?t=1a34fc98998afcb8a3ea183e9aa2b564"} "Join the Recurse Center!"]]]])

(defn- children-of-block-hiccup
  [roam-db block-eid]
  [:ul {:style "list-style-type:none; padding-left:0;"}
   [:li (:static-roam/hiccup (ds/entity (ds/db roam-db) block-eid))]
   (let [children-eids (:block/children (ds/entity (ds/db roam-db) block-eid))]
     (if (not= 0 (count children-eids))
       ;; recurse on each of the children
       (map #(children-of-block-hiccup roam-db %) children-eids)
       ;; otherwise, evaluate to empty div
       [:div]))])

(defn- get-eids-for-tagged-blocks
  [roam-db tag]
  (let [sr-info-eid (first (first (ds/q '[:find ?eid
                                          :where
                                          [?eid :node/title "Static-Roam Info"]]
                                        @roam-db)))
        eid-of-tag (first (first
                           (ds/q '[:find ?eid
                                   :in $ ?tag
                                   :where
                                   [?eid :node/title ?tag]]
                                 @roam-db tag)))
        query-result (ds/q '[:find ?parent-eid
                             :in $ ?tag-eid ?sr-info-eid
                             :where
                             [?eid :block/refs ?sr-info-eid]
                             [?eid :block/refs ?tag-eid]
                             [?eid :block/parents ?parent-eid]]
                           @roam-db eid-of-tag sr-info-eid)
        eids-of-tagged-blocks (filter (complement nil?)
                                      (into []
                                            (map first query-result)))]
    eids-of-tagged-blocks))

(defn- get-about-content-hiccup
  [roam-db]
  (let [about-page-eid (first (first (ds/q '[:find ?eid
                                               :where
                                               [?eid :node/title "About Page"]]
                                           @roam-db)))
        ;; abt-pg (get-eids-for-tagged-blocks roam-db "About")
        ;; about-eid-print (println abt-pg)
        children-eids (:block/children (ds/entity (ds/db roam-db) about-page-eid))]
    [:section {:id "about-content"}
     [:div
      [:h1 "About Me"]]
     [:ul {:style "list-style-type:none; padding-left:0;"}
      (map #(children-of-block-hiccup roam-db %) children-eids)]]))

(defn about-template
  [roam-db]
  (let [about-content (get-about-content-hiccup roam-db)]
    {"/about/index.html"
     [:html {:lang "en-US"}
      (head "About | Thomas Lisankie")
      [:body
       (header)
       [:main {:id "content"}
        about-content]
       (footer)]]}))

(defn- get-contact-content-hiccup
  [roam-db]
  (let [contact-page-eid (first (first (ds/q '[:find ?eid
                                               :where
                                               [?eid :node/title "Contact Page"]]
                                              @roam-db)))
        children-eids (:block/children (ds/entity (ds/db roam-db) contact-page-eid))]
    [:section {:id "contact-content"}
     [:div
      [:h1 "Contact Me"]]
     [:ul {:style "list-style-type:none; padding-left:0;"}
      (map #(children-of-block-hiccup roam-db %) children-eids)]]))

(defn contact-template
  [roam-db]
  (let [contact-content (get-contact-content-hiccup roam-db)]
    {"/contact/index.html"
     [:html {:lang "en-US"}
      (head "Contact | Thomas Lisankie")
      [:body
       (header)
       [:main {:id "content"}
        contact-content]
       (footer)]]}))

(defn- get-now-content-hiccup
  [roam-db]
  (let [now-page-eid (first (first (ds/q '[:find ?eid
                                               :where
                                               [?eid :node/title "Now Page"]]
                                              @roam-db)))
        children-eids (:block/children (ds/entity (ds/db roam-db) now-page-eid))]
    [:section {:id "contact-content"}
     [:div
      [:h1 "What I'm Up To"]]
     [:ul {:style "list-style-type:none; padding-left:0;"}
      (map #(children-of-block-hiccup roam-db %) children-eids)]]))

(defn now-template
  [roam-db]
  (let [now-content (get-now-content-hiccup roam-db)]
    {"/now/index.html"
     [:html {:lang "en-US"}
      (head "Now | Thomas Lisankie")
      [:body
       (header)
       [:main {:id "content"}
        now-content]
       (footer)]]}))

(defn- path-for-post
  [roam-db post-eid]
  (let [uid (:block/uid (ds/entity (ds/db roam-db) post-eid))
        path (str "/nodes/" uid ".html")]
    path))

(defn- get-post-content-hiccup
  [roam-db post-eid]
  (let [children-eids (:block/children (ds/entity (ds/db roam-db) post-eid))
        post-title (:node/title (ds/entity (ds/db roam-db) post-eid))]
    [:section {:id "post-content"}
     [:div
      [:h1 post-title]]
     [:ul {:style "list-style-type:none; padding-left:0;"}
      (map #(children-of-block-hiccup roam-db %) children-eids)]]))

(defn- hiccup-for-post
  [roam-db post-eid]
  (let [post-content (get-post-content-hiccup roam-db post-eid)
        post-title (:node/title (ds/entity (ds/db roam-db) post-eid))]
    [:html {:lang "en-US"}
     (head post-title)
     [:body
      (header)
      [:main {:id "content"}
       post-content]
      (footer)]]))

(defn- post-path-hiccup-pairs
  [roam-db]
  (let [post-eids (get-eids-for-tagged-blocks roam-db "Post")]
    (map (fn [post] [(path-for-post roam-db post) (hiccup-for-post roam-db post)]) post-eids)))

(defn post-template
  [roam-db]
  (into {} (post-path-hiccup-pairs roam-db)))

(defn- link-li-ele
  [roam-db eid]
  (let [page-title (:node/title (ds/entity (ds/db roam-db) eid))
        uid (:block/uid (ds/entity (ds/db roam-db) eid))
        path (str "../nodes/" uid ".html")]
    [:li
     [:a {:href path} page-title]]))

(defn- get-links-for-eids
  [roam-db eids]
  (map #(link-li-ele roam-db %) eids))

(defn- get-page-title-links-for-tagged
  [roam-db tag]
  (let [eids-of-tagged-blocks (get-eids-for-tagged-blocks roam-db tag)
        link-li-eles (get-links-for-eids roam-db eids-of-tagged-blocks)]
    link-li-eles))

(defn mind-template
  [roam-db]
  (let [entry-point-list (into [:ul {:id "entry-points"}] (get-page-title-links-for-tagged roam-db "EntryPoint"))]
    {"/mind/index.html"
     [:html {:lang "en-US"}
      (head "My Mind | Thomas Lisankie")
      [:body
       (header)
       [:main {:id "content"}
        [:section {:id "entry-point-list"}
         [:div
          [:h1 "Entry Points into my mind"]]
         entry-point-list]]
       (footer)]]}))

(defn posts-index-template
  [roam-db]
  (let [post-list (into [:ul {:id "posts"}] (get-page-title-links-for-tagged roam-db "Post"))]
    {"/posts/index.html"
     [:html {:lang "en-US"}
      (head "My Mind | Thomas Lisankie")
      [:body
       (header)
       [:main {:id "content"}
        [:section {:id "post-list"}
         [:div
          [:h1 "Posts / Essays / Prose / etc."]]
         post-list]]
       (footer)]]}))

(defn- children-of-node-hiccup
  [roam-db block-eid]
  [:ul
   [:li {:onclick (str "location.href='" ;;path
                       "'")}
    (:static-roam/hiccup (ds/entity (ds/db roam-db) block-eid))]
   (let [children-eids (:block/children (ds/entity (ds/db roam-db) block-eid))]
     (if (not= 0 (count children-eids))
       ;; recurse on each of the children
       (map #(children-of-node-hiccup roam-db %) children-eids)
       ;; otherwise, evaluate to empty div
       [:div]))])

(defn- get-node-content-hiccup
  [roam-db node-eid]
  (let [children-eids (:block/children (ds/entity (ds/db roam-db) node-eid))
        node-title (if (:node/title (ds/entity (ds/db roam-db) node-eid))
                     (:node/title (ds/entity (ds/db roam-db) node-eid))
                     (:block/content (ds/entity (ds/db roam-db) node-eid)))
        uid (:block/uid (ds/entity (ds/db roam-db) node-eid))
        path (str "./nodes/" uid ".html")]
    [:section {:id "post-content"}
     [:div
      [:h1 node-title]]
     [:ul
      (map #(children-of-node-hiccup roam-db %) children-eids)]]))

(defn- hiccup-for-node
  [roam-db node-eid]
  (let [node-content (get-node-content-hiccup roam-db node-eid)
        node-title (if (:node/title (ds/entity (ds/db roam-db) node-eid))
                     (:node/title (ds/entity (ds/db roam-db) node-eid))
                     (:block/content (ds/entity (ds/db roam-db) node-eid)))]
    [:html {:lang "en-US"}
     (head node-title)
     [:body
      (header)
      [:main {:id "content"}
       node-content]
      (footer)]]))

(defn- get-eids-for-included-blocks
  [roam-db]
  (map first (ds/q '[:find ?eids
                     :where
                     [?eids :static-roam/included true]] @roam-db)))

(defn- node-path-hiccup-pairs
  [roam-db]
  (let [node-eids (get-eids-for-included-blocks roam-db)]
    (map (fn [node] [(path-for-post roam-db node) (hiccup-for-node roam-db node)]) node-eids)))

(defn graph-page-template
  [roam-db]
  (into {} (node-path-hiccup-pairs roam-db)))

(defn homepage-template
  [roam-db]
  (let [post-list (into [:ul {:id "posts"}] (get-page-title-links-for-tagged roam-db "Post"))
        entry-point-list (into [:ul {:id "entry-points"}] (get-page-title-links-for-tagged roam-db "EntryPoint"))]
    {"/index.html"
     [:html {:lang "en-US"}
      (homepage-head "Thomas Lisankie")
      [:body
       (homepage-header)
       [:main {:id "homepage"}
        [:section {:id "about-me"}
         [:div
          [:h2 "Me In 10 Seconds"]
          [:p "I'm a software developer. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog."]]
         [:div
          [:h2 "Me In 10 Minutes"]
          [:p
           "Check out the "
           [:a {:href "./about"}
            "about"]
           " page"]]]
        [:section {:id "posts-home"}
         [:h2 "Posts"]
         post-list]
        [:section {:id "entry-points-home"}
         [:h2 "Entry Points into my Mind"]
         entry-point-list]]
       (footer)]]}))

(defn- create-templates
  [roam-db template-info]
  (for [template-fn-pair {"about"
                          about-template
                          "contact"
                          contact-template
                          "graph-page-example"
                          graph-page-template
                          "index"
                          homepage-template
                          "mind"
                          mind-template
                          "now"
                          now-template
                          "post-example"
                          post-template
                          "posts"
                          posts-index-template
                          }
        :let [template-name (first template-fn-pair)
              template-fn (second template-fn-pair)]]
    (template-fn roam-db)))

(defn- aggregate-templates
  [template-maps]
  (reduce into {} template-maps))

(defn generate-templates
  [roam-db template-info]
  (let [template-maps (create-templates roam-db template-info)
        final-template-map (aggregate-templates template-maps)]
    final-template-map))
