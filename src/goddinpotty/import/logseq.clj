(ns goddinpotty.import.logseq
  (:require [goddinpotty.utils :as utils]
            [goddinpotty.database :as db]
            [goddinpotty.rendering :as rendering]
            [goddinpotty.batadase :as bd]
            [goddinpotty.config :as config]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [goddinpotty.endure :as e]
            )
  )

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
                     ;; TODO make this more general
                     :public? (get-in block [:block/properties :public])
                     :alias (get-in block [:block/properties :alias])
                     :class (get-in block [:block/properties :class])
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
      (str/replace #"/" ".")
      ;;
      #_ (str/replace #"/" "-")            ;sometimes this becomes a . Argh
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

;;; Alternative to this shell nonsense https://www.eclipse.org/jgit/
(def git-date-formatter
  (java.text.SimpleDateFormat. "yyyy-MM-dd hh:mm:ss ZZZZZ"))

(defn parse-git-date
  [s]
  (.parse git-date-formatter s))

;;; TODO These are way too slow for practical use; need to cache the data across runs somehow, which is a pain.
;;; Hm, I want def-memoized but persistant...

;;; This saves a full 5 minutes in hyperphor build (as of 2/15/2022).
(e/defn-memoized git-first-mod-time
  [f]
  (-> (sh/sh "git" "log" "--reverse" "--date=iso"  "--format=\"%ad\"" "--" f "|" "head" "-1"
             :dir (get-in (config/config) [:source :repo]))
      :out
      (utils/strip-chars #{\" \newline})
      parse-git-date
      )
  )

;;; Unlike git-first-mod-time, the value of this will change over time.
;;; So this uses the file write time as a second key for the persistence lookup
;;; So changes should be detected.
;;; This will accumulate old cruft in the persistence store, but it can
;;; be deleted at will, so...
;;; TODO smarter store where it only keeps the last value of certain keys
(e/defn-memoized git-last-mod-time-1
  [f time]
  (-> (sh/sh "git" "log" "-1" "--date=iso"  "--format=\"%ad\"" "--" f
             :dir (get-in (config/config) [:source :repo]))
      :out
      (utils/strip-chars #{\" \newline})
      ((u/saferly parse-git-date))))

(defn git-last-mod-time
  [f]
  (git-last-mod-time-1 f (fs/mod-time f)))

(defn safe-times
  [f]
  (u/ignore-report
   (if (fs/exists? f)                    ;TODO this is not adequate check due to retarded case folding
     {:edit-time (git-last-mod-time f)
      :create-time (git-first-mod-time f)}
     (prn :file-not-found f)
     )))

;;; Slow...maybe too slow to use
(u/defn-memoized get-edit-times-from-repo
  [page]
  (-> page
      :title
      source-file
      safe-times
))

;;; Set last edit time of all blocks to file write date. Best we can do
(defn get-edit-times
  [bm]
  (u/map-values (fn [b]
                  (if (and (:include? b))
                    (merge b
                           (get-edit-times-from-repo (bd/block-page bm b))
                           )
                    b))
                bm))

(defn produce-bm
  [config]
  (let [{:keys [directory file-pattern]} (:source config)]
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

