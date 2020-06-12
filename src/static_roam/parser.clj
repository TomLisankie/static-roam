(ns static-roam.parser)

;; Raw content comes in, parsed tree should come out

(defn- string-replace
  [data f]
  (if (string? data)
    (f data)
    data))

(defn- todo-replace
  [string]
  ;; needs to replace the {{[[TODO]]}} in a string
  ;; will need to split the string if it finds it and replace with a vec I guess?
  )

(defn- content->ast
  [content]
  ;; This is analogous to the role of `roam-web-elements` in `core.clj`
  ;; At each step it maps on all of the strings it can find in the sequence and transforms them according to the rule at that step
  (let [og-seq [:block content]
        todos (map #(string-replace % todo-replace) og-seq)])
  )

(defn- get-youtube-vid-embed
  "Returns an iframe for a YouTube embedding"
  [string]
  [:iframe {:width "560"
            :height "315"
            :src (str "https://www.youtube-nocookie.com/embed/"
                      (cond
                        (re-find #"youtube\.com" string) (subs string 32)
                        (re-find #"youtu\.be" string) (subs string 17)
                        :else "NO VALID ID FOUND"))
            :frameborder "0"
            :allow "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"}])

(defn- query
  [query-string]
  ;; TODO write this function
  query-string)

(defn- word-count
  [block-ds-id conn]
  ;; TODO Implement this function
  "0")

(defn- hiccup-of-ele
  [block-ds-id ast-ele conn]
  (let [ele-content (second ast-ele)]
    (case (first ast-ele)
      :page-link (if (included? (remove-double-delimiters ele-content) conn)
                   [:a {:href (page-title->html-file-title (remove-double-delimiters ele-content) :case-sensitive)}
                    (remove-double-delimiters ele-content)]
                   (remove-double-delimiters ele-content))
      :block-ref (if (included? ele-content conn)
                   [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                    (content-find ele-content conn)]
                   "REDACTED")
      :metadata-tag (if (included? ele-content conn)
                      [:a {:href (page-title->html-file-title ele-content :case-sensitive)}
                       (str ele-content ":")]
                      (str ele-content ":"))
      :code-line [:code ele-content]
      :query (query ele-content)
      :youtube-embed (get-youtube-vid-embed ele-content)
      :word-count [:p (word-count block-ds-id conn)]
      :hashtag []
      :url-link []
      :bold [:b ele-content]
      :italic [:i ele-content]
      :highlight [:mark ele-content]
      :strikethrough [:s ele-content])))

(defn- ast-ele->hiccup
  [block-ds-id ast-ele conn]
  (cond
    (string? ast-ele) ast-ele
    (= ast-ele :block) :div
    (vector? ast-ele) (hiccup-of-ele block-ds-id ast-ele conn)
    :else ast-ele))

(defn- ast->hiccup
  [block-ds-id content conn]
  (map #(ast-ele->hiccup block-ds-id % conn) ast))

(defn block-content->hiccup
  [block-ds-id content conn]
  (->> content
       content->ast
       (#(ast->hiccup block-ds-id % conn))
       vec))

(content->ast "According to [[BJ Fogg]], we have [[motivation waves]].  Tie that in with the [[Fogg Behavior Model]] and you find that when people have high motivation, you should ask them to do something big and impactful, because if people are motivated to do more than the task that we ask them to do, it would be a waste for us not to prompt them to do so.  On the flip side, if people aren't particularly motivated, we shouldn't ask them to do something hard. This is similar to the premise of #[[difficulty matching]]")
