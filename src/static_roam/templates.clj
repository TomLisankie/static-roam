(ns static-roam.templates)

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
