(ns static-roam.rendering-test
  (:require [static-roam.rendering :refer :all]
            [static-roam.parser :as parser]
            [static-roam.utils :as utils]
            [mock-clj.core :as mc]
            [clojure.test :refer :all]))

(def fake-block-map
  {"foo" {:id "foo" :page? true :include? true :content "foo"}
   "bar" {:id "bar" :page? true :include? true :content "short"}
   "baz" {:id "baz" :page? true :include? true :content (str (range 1000))}})

(defn fake-block
  [content]
  {:content content
   :parsed (parser/parse-to-ast content)})

(deftest alias-html-test
  (is (= [:span "what " [:a.external {:href "fuck"} "the"] " is this"]
         (block-content->hiccup "what [the](fuck) is this")))
  (is (= [:span "what " [:a.external {:href "fuck"} "the fucking"] " is this"]
         (block-content->hiccup "what [the fucking](fuck) is this")))

  (is (= [:span "foo " [:a {:href "bar.html" :class "empty"} "bar"] " baz " [:a.external {:href "yuck"} "ugh"]]
         (block-hiccup (fake-block "foo [[bar]] baz [ugh](yuck)") fake-block-map)))
  (is (= [:span "foo " [:a.external {:href "yuck"} "ugh"] " baz " [:a {:href "bar.html" :class "empty"} "bar"]]
         (block-hiccup (fake-block "foo [ugh](yuck) baz [[bar]]") fake-block-map)
         ))
  (is (= [:span "foo " [:a.external {:href "yuck"} "ugh"] " baz " [:a.external {:href "zippy"} "yow"]]
         (block-hiccup (fake-block "foo [ugh](yuck) baz [yow](zippy)") {})))
  (is (= [:span "foo " [:a {:href "bar.html" :class "empty"} "bar"] " and " [:a {:href "baz.html"} "baz"]]
         (block-hiccup (fake-block "foo [[bar]] and [[baz]]") fake-block-map)
         ))
  )

(deftest blockquote-gen-test
  (testing "simple blockquote"
    (is (= [:blockquote "Call me Ishmael."]
           (block-hiccup (fake-block "> Call me Ishmael.") {}))))
  (testing "multiline blockquote"
    (is (= [:blockquote "I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow."]
           (block-hiccup (fake-block "> I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow.") {}))))

  (testing "blockquote with embedded markup"
    (is
     (= [:blockquote
         [:span
          "A: Well, " [:b "meditation is dealing with purpose itself"] ". It is not that meditation is for something, but it is dealing with the aim."]]
        (block-hiccup (fake-block "> A: Well, **meditation is dealing with purpose itself**. It is not that meditation is for something, but it is dealing with the aim.") {})))))

(deftest code-block-test
  (testing "codeblock htmlgen"
    (is (= [:code.codeblock "This is code\n and so is this."]
           (block-hiccup (fake-block  "```javascript\nThis is code
 and so is this.```") {})))))

(deftest markup-in-page-names-test
  (is (= [:a {:href "__foo__.html" :class "empty"} [:i "foo"]] 
         (block-hiccup (fake-block "[[__foo__]]")
                       (assoc fake-block-map "__foo__"
                              {:id "__foo__" :include? true :page? true :content "eh"})))))

(deftest italic-link-bug
  (testing "link inside italics"
    (is (= [:span
            "  – Wiliam S. Burroughs,  "
            [:i [:a.external {:href "http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479"} "The Western Lands"]]
            "."]
           (block-hiccup (fake-block 
            "  – Wiliam S. Burroughs,  __[The Western Lands](http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479)__.") {}))))
  (testing "italic inside link"
    (is (= [:span
            "  – Wiliam S. Burroughs,  "
            [:a.external {:href "http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479"} [:i "The Western Lands"]]
            "."]
           (block-hiccup (fake-block
            "  – Wiliam S. Burroughs,  [__The Western Lands__](http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479).") {})))))

(deftest youtube-id-test
  (is (= "rrstrOrJxOc"
         (get-youtube-id "pretty good video https://www.youtube.com/watch?v=rrstrOrJxOc")))
  (is (= "dO6v_tZtyu0"
         (get-youtube-id "– The Who, [__The Seeker__](https://youtu.be/dO6v_tZtyu0)")))
  (is (= "PwuckTkE7T4"
         (get-youtube-id "https://youtu.be/PwuckTkE7T4")))
  (is (= "-Jq0lohh_5U"
         (get-youtube-id "LA Open School {{[[video]]: https://youtu.be/-Jq0lohh_5U}}")))
  (is (= "0nU4EnB6wiE"
         (get-youtube-id "Dennett et al debate free will https://www.youtube.com/watch?v=0nU4EnB6wiE&feature=youtu.be")))
  (is (nil? (get-youtube-id "Not a youtube https://www.foo.com"))))

(deftest multiline-alias-test
  (is (= [:a.external
          {:href "https://faculty.washington.edu/lynnhank/GouldLewontin.pdf"}
          "The Spandrels of San Marco and the Panglossian Paradigm:\nA Critique of the Adaptationist Programme"]
         (ele->hiccup
          [:alias
           "[The Spandrels of San Marco and the Panglossian Paradigm:\nA Critique of the Adaptationist Programme](https://faculty.washington.edu/lynnhank/GouldLewontin.pdf)"] {}))))

(deftest internal-external-test
  (is (= [:span
          "Blah blah "
          [:a {:href "What-Motivated-Rescuers-During-the-Holocaust.html"} "finished coherent essay"]
          " or "
          [:a.external {:href "http://link"} "normal"]])
      (block-content->hiccup "Blah blah [finished coherent essay]([[What Motivated Rescuers During the Holocaust?]])")))

(deftest page-alias-test
  (mc/with-mock [utils/html-file-title :link-url]
    (is (= [:span "A show about " [:a {:href :link-url :class "empty"} "The Big Nada"] ]
           (block-hiccup (fake-block "A show about {{alias:[[nihilism]]The Big Nada}}")
                         {"nihilism" {:id "nihilism" :page? true :include? true :content "foo"}})))
    (is (= [:span "A show about " [:a {:href :link-url} "The Big Nada"] ]
           (block-hiccup (fake-block "A show about {{alias:[[nihilism]]The Big Nada}}")
                         {"nihilism" {:id "nihilism" :page? true :include? true :content (str (range 1000))}})))))
