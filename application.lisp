(in-package :clim-web-demo)

(define-command (/app/welcome :command-table *base-command-table*
                              :name "Welcome"
                              :return-type 'web-monad)
    ()
  (show-application
   (with-web-monad
     (mhtml
       (:h1 "Hello world!")))))
