(ns static-roam.markdown
  "Rendering to Markdown"
  (:require [static-roam.config :as config]
            [static-roam.batadase :as bd]
            [static-roam.utils :as utils]
            [clojure.string :as str]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [taoensso.truss :as truss :refer (have have! have?)]
            )
  )

;;; TODO [[links]] don't render into anything useful. Nor # unsuprisingly.
;;; Need to be rendered as [Agency](Agency.md) (but spaces in names don't work)

;;; TODO → multitool
(u/defn-memoized n-chars
  [n char]
  (str/join (repeat n char)))

(defn md-file-name
  [page-name]
  (str (utils/clean-page-title page-name) ".md"))

(defn page-link
  [page-name & [link-text]]
  (format "[%s](%s)" (or link-text page-name) (md-file-name page-name)))

(defn parsed->markdown
  [parse]
  (if (string? parse)
    parse
    (case (first parse)
      :block (str/join "" (map parsed->markdown (rest parse)))
      :blockquote (str "> " (parsed->markdown (second parse)))
      :page-link (page-link (utils/remove-double-delimiters (second parse)))
      :hashtag (page-link (utils/format-hashtag (second parse)))
      :alias (let [[_ text target] (render/parse-alias (second parse))]
               (if (str/starts-with? target "[[")
                 (page-link (utils/remove-double-delimiters target) text)
                 (second parse)))
      ;; TODO [:page-alias "{{alias:[[Meditations on Meditations on Moloch]]Meditations on Moloch}}"]
      :page-alias (second parse)

      (:italic :bold) (second parse)
      :metadata-tag (second parse)
      :bare-url (second parse)          ;github is ok with this, not sure about other markdown
      :todo "◘"                         ;I guess
      :block-ref (second parse)         ;TODO
      )))
      
(defn markdown-content
  [block]
  (if (empty? (:refs block))
    (:content block)
    (parsed->markdown (:parsed block))))

;;; Returns list of lines
(defn block->md
  [depth block]
  (when (bd/displayed? block)
    (cons (str (n-chars (* depth 4) \space)
               "- "
               (when (> (:heading block -1) 0)
                 (str (n-chars (:heading block) \#) " "))
               (markdown-content block))
          (filter identity (mapcat (partial block->md (+ 1 depth)) (:dchildren block))))))

(defn page->md
  [block]
  (let [title (:content block)
        footer-lines []                 ;TODO?
        header-lines
        (list title
              (n-chars (count title) \=)
              ;; TODO
              ;; (render-date-range (bd/date-range block))
              )]
    (concat header-lines
            (mapcat (partial block->md 0) (:dchildren block))
            footer-lines)))

(defn write-page
  [block file]
  (ju/file-lines-out file (page->md block)))

(defn write-all-pages
  [bm directory]
  (doseq [page (bd/displayed-pages bm)]
    (write-page page (str directory (md-file-name (:content page))))))
