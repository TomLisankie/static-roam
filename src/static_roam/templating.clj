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

(defn- head
  [page-title]
  [:head
   [:title page-title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
   [:link {:rel "shortcut icon" :href "./assets/favicon.ico" :type "image/x-icon"}]
   [:meta {:charset "utf-8"}]
   [:link {:rel "stylesheet" :type "text/css" :href "./assets/style.css"}]
   [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
   [:link {:href "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap" :rel "stylesheet"}]])

(defn- header
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

(defn about-template
  [roam-db]
  [:html {:lang "en-US"}
   (head "About | Thomas Lisankie")
   [:body
    (header)
    [:main {:id "content"}
     [:section {:id "about-content"}
      [:div
       [:h1 "About Me"]]
      [:ul {:style "list-style-type:none; padding-left:0;" :sr-content {:page "About"}}
       [:li {:style " padding-bottom:1em;"}
        "fsfsadffasdfsd"]
       [:li {:style " padding-bottom:1em;"}
        "fsfsadffasdfsd"]]
      [:p "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog."]
      [:p "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. "]]]
    (footer)]])

(defn contact-template
  [roam-db])

(defn graph-page-template
  [roam-db])

(defn homepage-template
  [roam-db])

(defn mind-template
  [roam-db])

(defn now-template
  [roam-db])

(defn post-template
  [roam-db])

(defn posts-index-template
  [roam-db])

(def template-fns
  {"about"
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
   })

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
    (template-fn roam-db (get template-info template-name))))

(defn- aggregate-templates
  [template-maps]
  (reduce into {} template-maps))

(defn generate-templates
  [roam-db template-info]
  (let [template-maps (create-templates roam-db template-info)
        final-template-map (aggregate-templates template-maps)]
    final-template-map))
