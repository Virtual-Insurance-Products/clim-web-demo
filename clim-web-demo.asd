
(asdf:defsystem :clim-web-demo
  :description "Demonstration of 'CLIM' web application"
  :author "VIP"
  :serial t
  :depends-on ("clim-web")
  :components ((:file "package")
               (:file "web-server")
               (:file "application")
               (:file "todo")))
