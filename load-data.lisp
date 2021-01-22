(in-package :clim-web-demo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the TODO items

(setf *todos*
      (accept-from-string '((list todo) :separator-regex "^\\n")
                          (vip-utils:slurp-file "todo.org")
                          :view (make-instance 'org-view))
      nil)


