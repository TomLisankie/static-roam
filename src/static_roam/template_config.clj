(ns static-roam.template-config)

{
 "about"
 {
  :folder "/about"
  :index true
  :query "PageTitle: About"
  :query '(tagged-as "About")
  }
 "contact"
 {
  :folder "/contact"
  :index true
  :query "PageTitle: Contact"
  }
 "graph-page-example"
 {
  :folder "/nodes"
  :query "Not:Post,About,Homepage,Contact,Now"
  }
 "index"
 {
  :folder "/"
  :index true
  :query "PageTitle: Homepage"
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
  :query "PageTitle: Now"
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
