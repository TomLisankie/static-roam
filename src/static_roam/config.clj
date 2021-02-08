(ns static-roam.config
  )

(def site-css ["../assets/hyper-roam.css"
               "../assets/proofreading.css"
               ])

(def exit-tag "#Private")

#_ (def exit-tag nil) ;; to bypass privacy mechanism entirely

(def dev-mode false)                    ;true turns on links into Roam itself
