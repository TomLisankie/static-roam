(ns static-roam.template-config)

{
 :site-title "Thomas Lisankie"
 :exclude ["Exclude" "high"]
 :include ["Include"]
 ;; Keep in mind, all of these are only operating on blocks that we've already marked as included:
 :templates {"about"
             {
              :folder "/about"
              :index true
              :tagged-as "About"
              }
             "contact"
             {
              :folder "/contact"
              :index true
              :tagged-as "Contact"
              }
             "graph-page-example"
             {
              :folder "/nodes"
              :exclude-tagged-as ["Post" "About" "Homepage" "Contact" "Now"]
              }
             "index"
             {
              :folder "/"
              :index true
              :tagged-as "Homepage"
              }
             "mind"
             {
              :folder "/mind"
              :index true
              :tagged-as "EntryPoint"
              }
             "now"
             {
              :folder "/now"
              :index true
              :tagged-as "Now"
              }
             "post-example"
             {
              :folder "/nodes"
              :tagged-as "Post"
              }
             "posts"
             {
              :folder "/posts"
              :index true
              :tagged-as "Post"
              }
             }
 }
