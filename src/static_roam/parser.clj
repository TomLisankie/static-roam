(ns static-roam.parser
  (:require [static-roam.parser :as parser]))

;; Raw content comes in, parsed tree should come out

(defn- content->ast
  [content])

(defn- ast->hiccup
  [block-ds-id content conn])

(defn block-content->hiccup
  [block-ds-id content conn]
  (->> content
       parse-to-ast
       (#(ast->hiccup block-ds-id % conn))
       vec))

(content->ast "According to [[BJ Fogg]], we have [[motivation waves]].  Tie that in with the [[Fogg Behavior Model]] and you find that when people have high motivation, you should ask them to do something big and impactful, because if people are motivated to do more than the task that we ask them to do, it would be a waste for us not to prompt them to do so.  On the flip side, if people aren't particularly motivated, we shouldn't ask them to do something hard. This is similar to the premise of #[[difficulty matching]]")
