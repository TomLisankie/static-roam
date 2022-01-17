(ns goddinpotty.import.logseq-from-md
  (:require [goddinpotty.utils :as utils]
            [goddinpotty.database :as db]
            [goddinpotty.rendering :as rendering]
            [goddinpotty.batadase :as bd]
            [goddinpotty.config :as config]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.string :as str]
            )
  (:import [org.commonmark.parser Parser])
  )

;;; This is unfinished, it seems like the wrong thing. I'd have to either bypass the normal parser
;;; (but that is still needed for page refs and tags and probably other things)
;;; OR re-convert the parsed stufff back to markdown, which seems stupid.
;;; Best solution would be to use Logseq's parser, but I can't even find it!

;;; From Markdown
(def parser (.build (Parser/builder)))
(def doc (.parse parser (slurp "/opt/mt/repos/ammdi/pages/Computer Power and Human Reason.md")))

(declare clj-node)

(defn node-children
  [node]
  (loop [child (.getFirstChild node)
         children []]
    (if child
      (recur (.getNext child)
             (conj children  (clj-node child)))
      children)))

(defn clj-node
  [node]
  (u/clean-map
   {:node node
    :type (keyword (.getSimpleName (class node)))
    :children (node-children node)
    :literal (u/ignore-errors (.getLiteral node))}))
                                        ;TODO temp!

(defmulti ->blockmap :type)

(defmethod ->blockmap :default [node]
  (cond (:literal node)
        (list (:type node) (:literal node))
        :else
        (cons (:type node) (map ->blockmap (:children node)))))

(defmethod ->blockmap :ListItem [node]
  (cons (:type node)
        (map ->blockmap (:children node))))

(defmethod ->blockmap :Text [node]
  (:literal node))

(defmethod ->blockmap :SoftLineBreak [node]
  "\n")

;;; empirically this seems safe
(defmethod ->blockmap :Paragraph [node]
  (apply str (map ->blockmap (:children node))))

(defn bracket
  [s punc]
  (str punc s punc))
  

;;; Argh, but oh well
(defmethod ->blockmap :Emphasis [node]
  (bracket (->blockmap (first (:children node))) "__"))

(defmethod ->blockmap :StrongEmphasis [node]
  (bracket (->blockmap (first (:children node))) "**"))

(defmethod ->blockmap :BlockQuote [node]
  (str "> " (->blockmap (first (:children node)))))

(defmethod ->blockmap :ListItem [node]
  (let [children (map ->blockmap (:children node))]
    {:id (gensym "b")
     :children children
   }))

(defmethod ->blockmap :BulletList [node]
  {:id (gensym "b")
   :children 
   (map ->blockmap (:children node))
   })

(defn decode-file
  [f]
  (->> f
       slurp
       (.parse parser)
       clj-node
       ->blockmap))

(defn parse-file
  [f]
  (->> f
       slurp
       (.parse parser)
       clj-node
       ))

;;; Crude parser


;      ju/file-lines
 
