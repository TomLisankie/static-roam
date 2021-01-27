(ns static-roam.template-config)

;; Keep in mind, all of these are only operating on blocks that we've already marked as included
{
 :exclude ["Exclude", "high"]
 :include ["Include"]
 :templates {"about"
             {
              :folder "/about"
              :index true
              :query "TaggedAs: About"
              }
             "contact"
             {
              :folder "/contact"
              :index true
              :query "TaggedAs: Contact"
              }
             "graph-page-example"
             {
              :folder "/nodes"
              :query "Exclude:Post,About,Homepage,Contact,Now"
              }
             "index"
             {
              :folder "/"
              :index true
              :query "TaggedAs: Homepage"
              }
             "mind"
             {
              :folder "/mind"
              :index true
              :query "TaggedAs: EntryPoint"
              }
             "now"
             {
              :folder "/now"
              :index true
              :query "TaggedAs: Now"
              }
             "post-example"
             {
              :folder "/nodes"
              :query "TaggedAs: Post"
              }
             "posts"
             {
              :folder "/posts"
              :index true
              :query "TaggedAs: Post"
              }
             }
 }
