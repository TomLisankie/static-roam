(ns static-roam.config
  )

(def site-css ["../assets/hyper-roam.css"
               "../assets/proofreading.css"
               ])

(def entry-tags ["EntryPoint"])
(def exit-tags ["ExitPoint" "Private" "charliebrowning"])

(def exclude-daily-logs true)


#_ (def exit-tags []) ;; to bypass privacy mechanism entirely

(def dev-mode true)                    ;true turns on links into Roam itself
(def roam-base-url "https://roamresearch.com/#/app/hyperphor/page/")
