(ns static-roam.curation
    (:require [static-roam.config :as config]
              [static-roam.utils :as utils]
              [static-roam.database :as database]
              [static-roam.batadase :as bd]
              [static-roam.core :as core]
              [clojure.set :as set]
              [org.parkerici.multitool.core :as u]
              [org.parkerici.multitool.cljcore :as ju]
              [me.raynes.fs :as fs]
              [clojure.string :as str]
              [clj-http.client :as client]
              ))

;;; Curation functions. For use via REPL; not wired into main program.

(defn bm
  []
  @core/last-bm)

;;; Finds where the same ref appears >1 time in a block, usually the result
;;; of overagressive Roam auto-linking.
#_
(def weirdos
  (filter #(and (:content %)
                (not (= (count (database/content-refs (:content %)))
                        (count (database/content-refs-0 (:content %))))))
          (vals (bm))))

;;; Trying to find substitutions within urls; happens quite frequently
(def bunkos
  (filter #(and (:content %)
                (re-find #"[\.]\[\[" (:content %)))
          (vals (bm))))


;;; This Roam syntax (block embeds?) not supported, and I think it's functionallyt the same as the plain block include?

;;; {{[[embed]]: ((rRHEjBfYg))}}

;;; Unmatched refs, should be empty except for certain parse issues
(defn wankos
  []
  (set/difference
   (reduce clojure.set/union (map :refs (vals (bm))))
   (set (keys (bm)))))

(defn wankos2
  []
  (let [included (u/clean-map (comp not :include?) (bm))]
    (clojure.set/difference
     (reduce clojure.set/union (map :refs (vals included)))
     (set (keys included)))))

; Ah that's actually useful, shows dead links in output, due to #Private pages I assume.

#{"meditation" "Stances, a Catalog" "Buddhism" "William Blake" "Infinite Jest" "YMCYL Kindle Notes"
  "Free Play"}


;;; Find bad links

(defn block-links
  [block]
  (u/walk-collect #(and (string? %)
                        (str/starts-with? % "http")
                        %)
                  (:parsed block)))

(defn all-links
  [block-map]
  (distinct (mapcat block-links (bd/displayed-blocks block-map))))

(defn check-link
  [url]
  (client/head url
               {:cookie-policy :standard
                :trace-redirects true
                :redirect-strategy :graceful}))

(defn check-links
  [bm]
  (let [bads (atom nil)]
    (doseq [l (all-links bm)]
      (future-call
       #(try
          (check-link l)
          (catch Throwable e (swap! bads conj [l e])))))
    bads))

;;; OK...next step is to generate archive.org links where possible, andd substitute them...how to do that? Write json for import? Can that do modifications? Need the Roam API...


(defn prettify
  [bads]
  (for [b bads]
    [(first b) (:status (ex-data (second b))) (:reason-phrase (ex-data (second b)))]))


;;; Check output and ensure all local html links are valid.
;;; This finds a lot of missing things due to Zoetero import, plus
; :missing "Topics.html" :in "play.html"
; :missing "Topics.html" :in "agency.html"
; :missing "Mastery-of-Non-Mastery.html" :in "mimesis.html"
(defn check-output-links
  []
  (doseq [f (fs/list-dir "output/pages")]
    (doseq [link (map second (re-seq #"href=\"(.*?)\"" (slurp f)))]
      (if (str/starts-with? link "http")
        nil                             ;ignore external links
        (when-not (fs/exists? (str "output/pages/" link))
          (prn :missing link :in (.getName f)))))))


(defn wayback
  [url]
  (let [resp (client/get "http://archive.org/wayback/available"
                         {:query-params {:url url}})]
    (clojure.data.json/read-str (:body resp) :keyword-fn keyword)))

;;; TODO convert bare URLs into [label](url)


;;; Highly connected nodes
(defn fan
  [bm]
  (let [bm (u/map-values #(assoc % :fan (count (bd/page-refs bm %))) bm)]
    (map (juxt :content :fan) (take 50 (reverse (sort-by :fan (filter :fan (vals bm))))))))

;;; Find blocks containing a given string.
;;; To be usefufl, probably wants to return pages
; (static-roam.batadase/block-page @last-bm (get @last-bm dblock))
(defn grep
  [bm string]
  (filter #(re-find (re-pattern string) (or (:content %) "")) (vals bm)))

;;; → multitool
(defn download
  [url to]
  (clojure.java.shell/sh "curl" url "-o" to))

(defn roam-image?
  "Returns the extension if this is in fact a roam image, nil otherwise"
  [url]
  (second (re-matches #"https\:\/\/firebasestorage\.googleapis\.com/.*\.(\w+)\?.*" url)))

(defn download-images
  [bm directory]
  (doseq [image-block (filter #(= :image (first (second (:parsed %)))) (vals bm))]
    (let [markdown (second (second (:parsed image-block)))
          ;; See rendering/format-image
          image-source (utils/remove-n-surrounding-delimiters 1 (re-find #"\(.*?\)" markdown))]
      (when-let [ext (roam-image? image-source)]
        (let [local-file (str directory (:title (bd/block-page bm image-block)) "-" (:id image-block) "." ext)]
          (prn :download local-file image-source)
          (download image-source local-file))))))

(defn collecting
  "Exec is a fn of one argument, which is called and passed another fn it can use to collect values; the collection is returned."
  [exec]
  (let [acc (atom [])
        collect #(swap! acc conj %)]
    (exec collect)
    @acc))

(defn page-images
  [bm]
  (collecting
   (fn [collect]
     (u/map-values
      (fn [block]
        (when-let [match (and (string? (:content block))
                              (re-matches #"^!\[\]\((.*)\)" (:content block)))]
          (collect [(:id block) (:id (bd/block-page bm block)) (second match)])))
      bm))))

(defn image-copy-script
  [bm dir]
  (doseq [[id page url] (page-images bm)]
    (let [file (format "%s/%s-%s.png" dir page id)]
      (println (format "curl \"%s\" > \"%s\"" url file)))))

;;; TODO Pici images

;;; Reexport from logseq

#_
(def logseq (utils/read-json "/Users/mtravers/Downloads/System_Volumes_Data_misc_working_org-roam_roam_1633487329.json"))


;;; Split



;; Idea:
;;- daily notes, plus anything heavily linked to (like #bikeride or other habits) gos in one bucket
;; - published pages
;; - everything else

;;Maybe too fancy, how about just separate out daily notes and everything else?

(defn subst-images
  [substs prefix s]
  (let [[m u] (re-find #"\((https://firebasestorage.*)\)" s)]
    (if m
      (if (contains? substs u)
        (str/replace s u (str prefix (get substs u)))
        (do
          ;; TODO this is bad, it ends up in output file because Clojure can be dumb
          (prn :not-found u s)          ;shouldn't happen
          s))
      s)))

(defn file-subst
  [f substs prefix]
  (->> f
       ju/file-lines
       doall
       (map (partial subst-images substs prefix))
       (ju/file-lines-out f)))

#_
(file-subst "/misc/repos/ammdi-augmented/pages/Whole Earth Catalog.md" substs "../assets/")

(doseq [f (fs/list-dir "/misc/repos/ammdi-augmented/pages/")]
  (prn f)
  (file-subst f substs "../assets/"))
