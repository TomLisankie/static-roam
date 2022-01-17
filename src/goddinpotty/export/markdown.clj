(ns goddinpotty.markdown
  "Rendering to Markdown"
  (:require [goddinpotty.config :as config]
            [goddinpotty.batadase :as bd]
            [goddinpotty.rendering :as render]
            [goddinpotty.utils :as utils]
            [clojure.string :as str]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [taoensso.truss :as truss :refer (have have! have?)]
            )
  )

;;; This code was originally made to generate a github site, but has been repurposed to do an export
;;; to Logseq. Needs to be rationalized

(def +for-export+ true)                ;TODO stopgap

(defn md-file-name
  [page-name]
  (str (utils/clean-page-title page-name) ".md"))

(defn html-file-name
  [page-name]
  (str (utils/clean-page-title page-name) ".html"))

(defn page-link
  [page-name & [link-text]]
  (format "[%s](%s)" (or link-text page-name) (md-file-name page-name)))

(defn youtube-link
  [md]
  (let [youtube-id (render/get-youtube-id md)
        img-link (format "https://img.youtube.com/vi/%s/0.jpg" youtube-id)
        video-link (format "https://youtu.be/%s" youtube-id)]
    (if youtube-id
      (format "[![](%s)](%s)" img-link video-link)
      md)))

(defn parsed->markdown
  [parse]
  (if (vector? parse)
    (case (first parse)
      :block (str/join "" (map parsed->markdown (rest parse)))
      :blockquote (str "> " (parsed->markdown (second parse)))
      :page-link (page-link (utils/remove-double-delimiters (second parse)))
      :hashtag (page-link (utils/parse-hashtag (second parse)))
      :alias (let [[_ text target] (render/parse-alias (second parse))]
               (if (str/starts-with? target "[[")
                 (page-link (utils/remove-double-delimiters target) text)
                 (second parse)))
      ;; TODO [:page-alias "{{alias:[[Meditations on Meditations on Moloch]]Meditations on Moloch}}"]
      :page-alias (second parse)

      (:italic :bold) (second parse)    ;TODO no longer correct
      (:image :code-block :code-line :hr) (second parse)
      :metadata-tag (second parse)
      :block-property nil               ;could include I suppose
      :bare-url (second parse)          ;github is ok with this, not sure about other markdown
      :todo "◘"                         ;I guess
      :done "⌧"                         ;I guess
      :block-ref (second parse)         ;TODO
      :youtube (youtube-link (second parse))
      (prn "Don't know how to translate to markdown" parse)) ;TODO log/warn
    parse))
      
;;;; Note: in most cases, we could just use :content, but links and other things
;;; need transform so md is reconstructed from the parse
(defn markdown-content
  [block]
  (parsed->markdown (:parsed block)))

;;; Returns list of lines
(defn block->md
  [depth block]
  (when (or +for-export+
            (bd/displayed? block))
    (cons (str (if +for-export+
                 (n-chars depth \tab)
                 (n-chars (* depth 4) \space))
               "- "
               ;; Might not want to do this in +for-export+ mode, but doesn't matter
               (when (and (:heading block) (> (:heading block) 0))
                 (str (n-chars (:heading block) \#) " "))
               (if +for-export+
                 (:content block)
                 (markdown-content block)))
          (filter identity (mapcat (partial block->md (+ 1 depth)) (:dchildren block))))))

(defn render-date-range
  [[from to]]
  (when (and from to)
    (str (utils/render-time from) " - " (utils/render-time to))))

(defn real-url
  [page]
  (str (config/config :real-base-url) (html-file-name (:content page))))

(defn real-page-pointer
  [page]
  (format "> This is a markdown backup. The [real page is part of %s](%s).\n\n"
          (config/config :main-page)
          (real-url page)))

(defn page->md
  [block]
  (let [title (or (:title block) (:content block))
        footer-lines []                 ;TODO colophon is in hiccup 
        header-lines
        (list (real-page-pointer block)
              title
              (n-chars (count title) \=)
              (render-date-range (bd/date-range block))
              )]
    (concat header-lines
            (mapcat (partial block->md 0) (:dchildren block))
            footer-lines)))

(defn write-page
  [block file]
  (ju/file-lines-out file (page->md block)))

(defn write-displayed-pages
  [bm directory]
  (doseq [page (bd/displayed-pages bm)]
    (write-page page (str directory (md-file-name (:content page))))))
