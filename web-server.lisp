
(in-package :clim-web-demo)

(publish-directory :prefix "/pub/" :destination "./pub/")

(publish-prefix :prefix "/app/"
                :function (clim-request-handler (make-instance 'clim-web-manager)))


(start :port "9880")

