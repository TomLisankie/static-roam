(ns templates)

(def general-site-info
  {
   "Site Title" "Thomas Lisankie"
   "Nav Bar Pages" ["About", "Now", "Posts", "My Mind", "Contact"]
   "Twitter Profile" "TomLisankie"
   "GitHub Profile" "TomLisankie"
   })

(defn head
  [site-title]
  [:head
   [:title site-title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1"}]
   [:link {:rel "shortcut icon" :href "./assets/favicon.ico" :type "image/x-icon"}]
   [:meta {:charset "utf-8"}]
   [:link {:rel "stylesheet" :type "text/css" :href "./assets/style.css"}]
   [:link {:rel "preconnect" :href "https://fonts.gstatic.com"}]
   [:link {:href "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap" :rel "stylesheet"}]])

(defn header
  [site-title nav-bar-pages twitter-username github-username]
  [:header
   [:h1 {:id "site-title-link"}
    [:a {:href "./"} site-title]]
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
     [:a {:class "social-icon" :href (str "https://twitter.com/" twitter-username)}
      [:img {:src "./assets/twitter.svg" :width "20" :height "20"}]]
     " "
     [:a {:class "social-icon" :href (str "https://github.com/" github-username)}
      [:img {:src "./assets/github.svg" :width "20" :height "20"}]]]]])

(defn footer
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

(defn index
  [head header footer site-title me-in-10-secs me-in-10-mins posts entry-points]
  [:html {:lang "en-US"}
   (head site-title)
   [:body
    (header site-title)
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
      [:ul
       [:li [:a {:href "./nodes/post-example.html"}
             "What I'm Doing in Austin"]]
       [:li "A New Start"]
       [:li "What is an IP Address Really?"]]]
     [:section {:id "entry-points-home"}
      [:h2 "Entry Points into my Mind"]
      [:ul
       [:li [:a {:href "./nodes/graph-page-example.html"}
             "What's been on my mind"]]
       [:li "The Network is the metaphor of this age"]]]]
    (footer)]])

(defn about
  [head header footer about-content]
  [:html {:lang "en-US"}
   (head)
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

(defn contact
  [head header footer contact-content]
  [:html {:lang "en-US"}
   (head)
   [:body
    (header)
    [:main {:id "content"}
     [:section {:id "contact-content"}
      [:div
       [:h1 "Contact Me"]]
      [:p "Here are a couple of ways to contact me:"]
      [:ul
       [:li
        "Email: hello (you know the symbol) tomlisankie.com"]
       [:li
        [:a {:href "https://twitter.com/TomLisankie"}
         "Twitter"]]]]]
    (footer)]])

(defn graph-page
  [head header footer page-title page-children linked-references]
  [:html {:lang "en-US"}
   (head)
 [:body
  (header)
  [:main {:id "graph-page-content"}
   [:section {:id "nav-bar-page"}
    [:div
     [:h1 page-title]]
    [:ul {:class "graph-page-ul"}
     [:li {:class "graph-page-li"}
      "Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text. Some example text.ffffffffff "]
     [:li {:class "graph-page-li"}
      "More example text!"]
     [:ul {:class "graph-page-ul"}
      [:li {:class "graph-page-li"}
       "This is a child of the previous block"]
      [:ul {:class "graph-page-ul"}
       [:li {:class "graph-page-li"}
        "This is a child of that child"]]
      [:li {:class "graph-page-li"}
       "This is another child"]]
     [:li {:class "graph-page-li"}
      "Another top-level block"]
     [:li {:class "graph-page-li"}
      "The final block here"]]]
   [:div {:class "linked-references"}
    [:h3 {:style "padding-left: 1em;"} "Linked References"]
    [:ul
     [:li {:class "graph-page-li"}
      "Hello"]
     [:li {:class "graph-page-li"}
      "Yes"]
     [:li {:class "graph-page-li"}
      "this is"]
     [:li {:class "graph-page-li"}
      "dog"]]]]
  (footer)]])

(defn now
  [head header footer now-content]
  [:html {:lang "en-US"}
   (head)
   [:body
    (header)
    [:main {:id "content"}
     [:section {:id "now-content"}
      [:div
       [:h1 "What I'm Up to These Days"]]
      [:p "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. "]]]
    (footer)]])

(defn mind
  [head header footer entry-points]
  [:html {:lang "en-US"}
   (head)
   [:body
    (header)
    [:main {:id "content"}
     [:section {:id "entry-point-list"}
      [:div
       [:h1 "Entry Points into my mind"]]
      [:ul {:id "entry-points"}
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "What's been on my mind"]]
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "The Network is the metaphor of this age"]]
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "The Pseudonymity Sacrament"]]
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "What's been on my mind"]]
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "The Network is the metaphor of this age"]]
       [:li
        [:a {:href "../nodes/graph-page-example.html"}
         "The Pseudonymity Sacrament"]]]]]
    (footer)]])

(defn post
  [head header footer post-title post-content]
  [:html {:lang "en-US"}
   (head)
   [:body
    (header)
    [:main {:id "content"}
     [:article {:id "post-content"}
      [:div
       [:h1 post-title]]
      [:p "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. "]
      [:p "The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. The quick brown fox jumps over the lazy dog. "]]]
    (footer)]])

(defn posts
  [head header footer posts-list]
  [:html {:lang "en-US"}
   (head)
   [:body
    (header)
    [:main {:id "content"}
     [:section {:id "post-list"}
      [:div
       [:h1 "Posts / Essays / Prose / etc."]]
      [:ul {:id "posts"}
       [:li
        [:a {:href "../nodes/post-example.html"}
         "What I'm Doing in Austin"]]
       [:li
        [:a {:href "../nodes/post-example.html"}
         "A New Start"]]
       [:li
        [:a {:href "../nodes/post-example.html"}
         "What is an IP Address Really?"]]
       [:li
        [:a {:href "../nodes/post-example.html"}
         "What I'm Doing in Austin"]]
       [:li
        [:a {:href "../nodes/post-example.html"}
         "A New Start"]]
       [:li
        [:a {:href "../nodes/post-example.html"}
         "What is an IP Address Really?"]]]]]
    (footer)]])
