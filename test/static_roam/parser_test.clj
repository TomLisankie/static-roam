(ns static-roam.parser-test
  (:require [static-roam.parser :refer :all]
            [clojure.test :refer :all]))

;;; Stray stuff
(comment


(parse-to-ast "(((hello)))")

(parse-to-ast "[hello]([[Hello]])")

(parse-to-ast "I hope to combine my expertise in behavioral science and gamification to help users improve their lives through products. Your users hire you to help them achieve some goal, but loving your product is a result of your teamwork with the user.[++]([[The role of the user and the role of the app]]) Both players need to play.")
)

;;; TODO I thik parsing should stript the punctuation, but that is not the current convention

(deftest parsing-test
  (let [parsed (set (rest (parse-to-ast "Metadata is here:: According to [[BJ Fogg]], we have [[motivation waves]].  Tie that in with the [[Fogg Behavior Model]] and you find that when people have high motivation, you should ask them to do something big and impactful, because if people are motivated to do more than the task that we ask them to do, it would be a waste for us not to prompt them to do so.  On the flip side, if people aren't particularly motivated, we shouldn't ask them to do something hard. ((j598fj6)) This is similar to the premise of #[[difficulty matching]] #yessereebob `this is a line of code` {{query: {and: [[note]] [[January]] }}} {{youtube: https://youtu.be/5iI_0wnwIpU}} [fasfa]([[Hello page]]) **IMPORTANT** __emphasis__ ^^pop out^^ ~~old news~~")))
        ]
    (is (contains? parsed [:page-link "[[BJ Fogg]]"]))
    (is (contains? parsed [:block-ref "((j598fj6))"]))
    (is (contains? parsed [:hashtag "#[[difficulty matching]]"])) ;TODO failing
    (is (contains? parsed [:hashtag "#yessereebob"]))
    (is (contains? parsed [:alias "[fasfa]([[Hello page]])"]))
    (is (contains? parsed [:youtube "{{youtube: https://youtu.be/5iI_0wnwIpU}}"]))
    (is (contains? parsed [:bold "**IMPORTANT**"]))
    (is (contains? parsed [:italic "__emphasis__"]))
    (is (contains? parsed [:highlight "^^pop out^^"]))
    (is (contains? parsed [:strikethrough "~~old news~~"]))))


(deftest hashtag-parse-test
  (is (= [:block [:page-link "[[Physics]]"] " " [:hashtag "#Static-Roam"]]
         (parse-to-ast "[[Physics]] #Static-Roam"))))

(deftest alias-parse-test
  ;; Was formerly misparsing, changed the alias regex to use \w
  ;; TODO make parsing in general more systematics and accurate
  ;; TODO seems like these should strip the punctuation
  (is (= [:block "foo " [:page-link "[[bar]]"] " baz " [:alias "[ugh](yuck)"]]
         (parse-to-ast "foo [[bar]] baz [ugh](yuck)")))
  (is (= [:block "foo " [:alias "[ugh](yuck)"] " baz " [:page-link "[[bar]]"]]
         (parse-to-ast "foo [ugh](yuck) baz [[bar]]")
         ))
  (is (= [:block "foo " [:alias "[ugh](yuck)"] " baz " [:alias "[yow](zippy)"]]
         (parse-to-ast "foo [ugh](yuck) baz [yow](zippy)")))
  (is (= [:block "foo " [:page-link "[[bar]]"] " and " [:page-link "[[baz]]"]]
         (parse-to-ast "foo [[bar]] and [[baz]]")
         )))

(deftest alias-html-test
  (is (= [:span "what " [:a {:href "fuck"} "the"] " is this"]
         (block-content->hiccup "what [the](fuck) is this" {})))
  (is (= [:span "what " [:a {:href "fuck"} "the fucking"] " is this"]
         (block-content->hiccup "what [the fucking](fuck) is this" {})))
  (is (= [:span "foo " [:a {:href "./bar.html"} "bar"] " baz " [:a {:href "yuck"} "ugh"]]
         (block-content->hiccup "foo [[bar]] baz [ugh](yuck)" {})))
  (is (= [:span "foo " [:a {:href "yuck"} "ugh"] " baz " [:a {:href "./bar.html"} "bar"]]
         (block-content->hiccup "foo [ugh](yuck) baz [[bar]]" {})
         ))
  (is (= [:span "foo " [:a {:href "yuck"} "ugh"] " baz " [:a {:href "zippy"} "yow"]]
         (block-content->hiccup "foo [ugh](yuck) baz [yow](zippy)" {})))
  (is (= [:span "foo " [:a {:href "./bar.html"} "bar"] " and " [:a {:href "./baz.html"} "baz"]]
         (block-content->hiccup "foo [[bar]] and [[baz]]" {})
         )))
  

(deftest blockquote-parse-test
  (testing "simple blockquote"
    (is (= [:block [:blockquote [:block "Call me Ishmael."]]]
           (parse-to-ast "> Call me Ishmael."))))
  (testing "multiline blockquote"
    (is (= [:block
            [:blockquote
             [:block "I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow."]]]
           (parse-to-ast "> I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow.")))))


(deftest blockquote-gen-test
  (testing "simple blockquote"
    (is (= [:blockquote "Call me Ishmael."]
           (block-content->hiccup "> Call me Ishmael." {}))))
  (testing "multiline blockquote"
    (is (= [:blockquote "I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow."]
           (block-content->hiccup "> I see the Four-fold Man, The Humanity in deadly sleep
And its fallen Emanation, the Spectre and its cruel Shadow." {}))))

  ;; Not working
  (testing "blockquote with embedded markup"
    (= [:blockquote
         "A: Well, " [:b "meditation is dealing with purpose itself"] ". It is not that meditation is for something, but it is dealing with the aim."]
       (block-content->hiccup "> A: Well, **meditation is dealing with purpose itself**. It is not that meditation is for something, but it is dealing with the aim." {})))
  )


(deftest code-block-test
  (testing "codeblock parsing"
    (is (= [:block [:code-block "```This is code
 and so is this.```"]]
           (parse-to-ast "```This is code
 and so is this.```"))))
  (testing "codeblock htmlgen"
    (is (= [:code.codeblock "This is code\n and so is this."]
           (block-content->hiccup "```This is code
 and so is this.```" {})))))

(deftest markup-in-page-names-test
  (is (= [:a {:href "./__foo__.html"} [:i "foo"]] 
         (block-content->hiccup "[[__foo__]]" {}))))

(deftest blockquote-parse-bug
  (is (= [:block                        ;Ugly. The point is to not gen a blockquote
          [:text " "]
          [:text "why"]
          [:text " "]
          [:text "me"]
          [:text ">"]
          [:text " "]]
         (block-parser " why me> "))))

(deftest hashtag-parse-bug
  (is (= '[:block "I was under the " [:hashtag "#influence"] ", officer"]
         (parse-to-ast "I was under the #influence, officer"))))

(deftest italic-link-bug
  (testing "link inside italics"
    (is (= [:span
            "  – Wiliam S. Burroughs,  "
            [:i [:a {:href "http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479"} "The Western Lands"]]
            "."]
           (block-content->hiccup
            "  – Wiliam S. Burroughs,  __[The Western Lands](http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479)__." {}))))
  (testing "italic inside link"
    (is (= [:span
            "  – Wiliam S. Burroughs,  "
            [:a {:href "http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479"} [:i "The Western Lands"]]
            "."]
           (block-content->hiccup
            "  – Wiliam S. Burroughs,  [__The Western Lands__](http://books.google.com/books?id=Vg-ns2orYBMC&pg=PA479)." {})))))


(deftest page-alias-test
  (is (= [:span "A show about " [:a {:href "./nihilism.html"} "The Big Nada"] ]
         (block-content->hiccup "A show about {{alias:[[nihilism]]The Big Nada}}" {}))))


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
         (get-youtube-id "Dennett et al debate free will https://www.youtube.com/watch?v=0nU4EnB6wiE&feature=youtu.be"))))
  
