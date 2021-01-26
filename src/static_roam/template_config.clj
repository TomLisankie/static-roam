(ns static-roam.template-config)

{
 "about"
 {
  :folder "about"
  :query '[:find ?eid
           :where
           [?eid :node/title "About"]]
  }
 "contact"
 {
  :folder "contact"
  :query '[:find ?eid
           :where
           [?eid :node/title "Contact"]]
  }
 "graph-page-example"
 {
  :folder "nodes"
  :query :query-for-graph-pages
  }
 "index"
 {
  :folder "."
  :query '[:find ?eid
           :where
           [?eid :node/title "Homepage"]]
  }
 "mind"
 {
  :folder "mind"
  :query :query-for-entry-points-page
  }
 "now"
 {
  :folder "now"
  :query '[:find ?eid
           :where
           [?eid :node/title "Now Page"]]
  }
 "post-example"
 {
  :folder "nodes"
  :query :query-for-post-pages
  }
 "posts"
 {
  :folder "posts"
  :query :query-for-posts-page
  }
 }
