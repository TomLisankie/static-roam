(ns goddinpotty.parser-test
  (:require [goddinpotty.parser :refer :all]
            [goddinpotty.utils :as utils]
            [mock-clj.core :as mc]
            [clojure.test :refer :all]))


(deftest parsing-test
  (let [parsed (set (rest (parse-to-ast "According to [[BJ Fogg]], we have [[motivation waves]].  Tie that in with the [[Fogg Behavior Model]] and you find that when people have high motivation, you should ask them to do something big and impactful, because if people are motivated to do more than the task that we ask them to do, it would be a waste for us not to prompt them to do so.  On the flip side, if people aren't particularly motivated, we shouldn't ask them to do something hard. ((j598fj6)) This is similar to the premise of #[[difficulty matching]] #yessereebob `this is a line of code` {{query: {and: [[note]] [[January]] }}} {{youtube https://youtu.be/5iI_0wnwIpU}} [fasfa]([[Hello page]]) **IMPORTANT** __emphasis__ ^^pop out^^ ~~old news~~")))
        ]
    (is (contains? parsed [:page-link "[[BJ Fogg]]"]))
    (is (contains? parsed [:block-ref "((j598fj6))"]))
    (is (contains? parsed [:hashtag "#[[difficulty matching]]"]))
    (is (contains? parsed [:hashtag "#yessereebob"]))
    (is (contains? parsed [:alias "[fasfa]([[Hello page]])"]))
    (is (contains? parsed [:youtube "{{youtube https://youtu.be/5iI_0wnwIpU}}"]))
    (is (contains? parsed [:bold "IMPORTANT"]))
    (is (contains? parsed [:italic "emphasis"]))
    ;; Turned off for performance
    #_ (is (contains? parsed [:highlight "^^" "pop out" "^^"]))
    #_ (is (contains? parsed [:strikethrough "~~" "old news" "~~"]))
    ))


(deftest hashtag-parse-test
  (is (= [:block [:page-link "[[Physics]]"] " " [:hashtag "#Goddinpotty"]]
         (parse-to-ast "[[Physics]] #Goddinpotty"))))

(deftest alias-parse-test
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



(deftest code-block-test
  (testing "codeblock parsing"
    (is (= [:block [:code-block "```javascript\nThis is code
 and so is this.```"]]
           (parse-to-ast "```javascript\nThis is code
 and so is this.```")))))


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


(deftest image-test
  (is (= [:block
          [:image
           "![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fhyperphor%2FyQ6S-ONK3c.png?alt=media&token=e3c079a8-2245-4fac-9772-483443e74b65)"]]
         (parse-to-ast "![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2Fhyperphor%2FyQ6S-ONK3c.png?alt=media&token=e3c079a8-2245-4fac-9772-483443e74b65)"))))

;;; The comma was causing problems.
(deftest weird-italics-bug
  (is (= [:block "oh " [:italic "fuck"] " this, it is " [:italic "nonsense"] " please"]
         (parse-to-ast "oh __fuck__ this, it is __nonsense__ please")))
  (is (= [:block "oh " [:italic "fuck"] " this. it is " [:italic "nonsense"] " please"]
         (parse-to-ast "oh __fuck__ this. it is __nonsense__ please")))
  (is (= [:block "foo, "
          [:italic "bar"]
          " blag "
          [:italic "The Confidence Man"]
          " and"]
         (parse-to-ast "foo, __bar__ blag __The Confidence Man__ and")))
  (is (= [:block "foo, "
          [:italic "bar"]
          " blag "
          [:italic "The Confidence Man"]
          " and "]
         (parse-to-ast "foo, __bar__ blag __The Confidence Man__ and ")))
  ;; Apparently this one is very tough to parse, maybe due to the heavy literary pretentiousness
  (let [dis-content "Anyway, __White Noise__ is thematically timely and one of the best novels of the late 20th century according to Harold Bloom, who placed it in the tradition of \"American comic apocalypses\" that includes Melville's __The Confidence Man__ and [[Thomas Pynchon]]'s __Crying of Lot 49__. (I guess I'd put __[[Infinite Jest]]__ and the work of [[Philip K Dick]] in that bucket as well).  And it's due to be [turned into a movie](https://www.latimes.com/entertainment-arts/story/2021-01-14/adam-driver-greta-gerwig-and-noah-baumbach-reunite-for-adaptation-of-don-delillos-white-noise) this year."]
    (= [:block
        "Anyway, "
        [:italic "White" " " "Noise"]
        " is thematically timely and one of the best novels of the late 20th century according to Harold Bloom, who placed it in the tradition of \"American comic apocalypses\" that includes Melville's "
        [:italic "The" " " "Confidence" " " "Man"]
        " and "
        [:page-link "[[Thomas Pynchon]]"]
        "'s "
        [:italic "Crying" " " "of" " " "Lot" " " "49"]
        ". (I guess I'd put "
        [:italic [:page-link "[[Infinite Jest]]"]]
        " and the work of "
        [:page-link "[[Philip K Dick]]"]
        " in that bucket as well).  And it's due to be "
        [:alias
         "[turned into a movie](https://www.latimes.com/entertainment-arts/story/2021-01-14/adam-driver-greta-gerwig-and-noah-baumbach-reunite-for-adaptation-of-don-delillos-white-noise)"]
        " this year."]
       (parse-to-ast dis-content)
       )))


(deftest underscore-bug
  (is (= [:block "this is pretty basic and should work"]
         (parse-to-ast "this is pretty basic and should work")))
  (is (= [:block "this is pretty_basic and should work"]
         (parse-to-ast "this is pretty_basic and should work"))))

(deftest three-italians-bug
  (is (= [:block
          "blah "
          [:italic "Epistemology"]
          ", "
          [:italic "Agency"]
          ", and "
          [:italic "Curiosity"]
          ", "]
         (parse-to-ast "blah __Epistemology__, __Agency__, and __Curiosity__, "))))

(deftest italic-colon-bug
  (is (= [:block "what " [:italic "the: fuck"] " charlie?"]
         (parse-to-ast "what __the: fuck__ charlie?")))  )

(deftest parse-many-bolds
  (is (= [:block "a " [:bold "b"] " c " [:bold "d"] " e"]
         (parse-to-ast "a **b** c **d** e")))
  (is (= [:block "a " [:bold "b"] " c " [:bold "d"] " e " [:bold "f"] " g"]
         (parse-to-ast "a **b** c **d** e **f** g"))))
