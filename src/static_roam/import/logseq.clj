(ns static-roam.import.logseq
  (:require [static-roam.utils :as utils]
            [static-roam.database :as db]
            [static-roam.rendering :as rendering]
            [static-roam.batadase :as bd]
            [static-roam.config :as config]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.string :as str]
            )
  (:import [org.commonmark.parser Parser]))

;;; Logseq EDN export + repo ⇒ blockmap

;;; Mainly uses EDN export – but should compute that myself.

;;; Note: there's not a lot to be gained by using that (and it requires a manual export
;;; Might be better to just build from .md files like Logseq does. Might even steal their code!


(defn logseq-edn->blockmap
  [f]
  (prn :reading f)
  (let [pages (-> f
                  utils/read-edn        ;TODO check version 1
                  :blocks)
        bm (atom {})
        ]
    (prn :pages (:count pages))
    (letfn [(convert [block]
              (let [b
                    {
                     :title (or (get-in block [:block/properties :title])
                                (:block/page-name block)) ; This is often wrong, eg lowercased, so ony use when we have tof
                     :id (if (:block/page-name block)
                           (or (get-in block [:block/properties :title])
                               (:block/page-name block))
                           (str (:block/id block)))
                     :uid (str (:block/id block))
                     :content (:block/content block) ;TODO strip out properties
                     :edit-time (utils/coerce-time (get-in block [:block/properties :updated-at]))
                     :create-time (utils/coerce-time (get-in block [:block/properties :created-at]))
                     :children (doall (map (comp :id convert) (:block/children block)))
                     :page? (boolean (:block/page-name block))
                     ;; Support Logseq publish tag
                     :public? (get-in block [:block/properties :public])
                     }]
                (swap! bm assoc (:id b) b)
                b))]
      (doseq [p pages]
        (convert p))
      (vals @bm))))

;;; Determined empirically. This still fails on a few files
(defn clean-page-name
  [n]
  (-> n
      #_ (str/replace #"/" ".")
      (str/replace #"/" "-")            ;sometimes this becomes a . Argh
      (str/replace #"[:\?\"]" "_")  ; \(\) but these seem just as often passed through...argh
      ))

(defn source-file
  [page-name]
  (and page-name
       (str (get-in (config/config) [:source :repo])
            "/pages/"
            (clean-page-name page-name)
            ".md")))                          ;TODO could be .org

(defn safe-mod-time
  [f]
  (if (fs/exists? f)
    (java.util.Date. (fs/mod-time f))
    (prn :file-not-found f)))           ;temp

(u/defn-memoized get-edit-time-from-repo
  [page]
  (-> page
      :title
      source-file
      safe-mod-time
))

;;; Set last edit time of all blocks to file write date. Best we can do
;;; TODO without mining git logs, which seems...excessive
(defn get-edit-times
  [bm]
  (u/map-values (fn [b]
                  (if (and (:include? b))
                    (assoc b
                           :edit-time
                           (get-edit-time-from-repo (bd/block-page bm b)))
                    b))
                bm))

(defn produce-bm
  []
  (let [{:keys [directory file-pattern]} (:source (config/config))]
  (-> (utils/latest directory file-pattern)
      logseq-edn->blockmap
      db/index-blocks    
      db/roam-db-1
      get-edit-times
      bd/add-empty-pages
      db/generate-inverse-refs ;have to redo this after add-empty-pages
      )))

(defn publish-images
  [logseq-dir]
  (doseq [file @rendering/published-images]
    (u/ignore-report
    ;; this tree-hopping is ugly
     (fs/copy+ (fs/expand-home (str logseq-dir "/assets/" file))
               (fs/expand-home (str (:output-dir (config/config)) "/pages/" file))))))

;;; TODO Not Logseq specific
(defn publish-assets
  []
  (doseq [file (fs/list-dir "resources/public")]
    (fs/copy+ file
              (str (fs/expand-home (:output-dir (config/config))) "/assets/" (fs/base-name file)))))

(defn post-generation
  []
  (publish-images (get-in (config/config) [:source :repo]) )
  (publish-assets))

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
 
