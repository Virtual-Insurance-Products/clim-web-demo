(in-package :clim-web-demo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODEL

(defclass todo ()
  ((done-p :initform nil :accessor done-p :type boolean)
   (description :type string :accessor description :initarg :description)))

(defun todo-eql (a b)
  (equal (description a) (description b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VIEWS

(defclass todo-editable-view (textual-view) ())
(defclass org-view (textual-view) ())

;; this is used to provide a textual representation of the TODO item which we can #'accept
(define-presentation-method present ((object todo) (type todo) (stream stream)
                                     (view textual-view) &key acceptably)
  (present (description object) 'string :stream stream :view view :acceptably acceptably))

;; (present-to-string (make-instance 'todo :description "Do something") 'todo :acceptably t)
;; (present-to-string (make-instance 'todo :description "Do something") 'todo)

(define-presentation-method present ((object todo) (type todo) (stream stream)
                                     (view org-view) &key acceptably)
  acceptably
  (format stream "- [~A] ~A"
          (if (done-p object) "X" " ")
          (description object)))

;; (present-to-string (make-instance 'todo :description "Hi there") 'todo :view (make-instance 'org-view))

;; WEB PRESENTATION
;; Here we use :html as the 'stream' to designate output to HTML
;; This could probably be abstracted out as a proper stream and just make extra commands on the stream to output
;; elements, but this is how it works so far

(define-presentation-method present ((object todo) (type todo) (stream (eql :html))
                                     (view todo-editable-view) &key acceptably)
  (html
    (:span :style (css :width "14px"
                       :height "14px"
                       :border-radius "12px"
                       :display "inline-block"
                       :margin "4px"
                       :border "2px solid #555")
           (when (done-p object)
             (html (:noescape " &#x2713"))))
    (:print (description object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ACCEPTING

;; We need a way to accept a todo item based on a string. Because we
;; have a list of them, and they should logically be distinct, we can
;; just accept the description...

;; This diverges somewhat from 'normal' CLIM. I'm using the
;; simple-parser framework to, potentially, provide backtracking. Here
;; it doesn't make any difference though
;; !!! I wonder if I can do this in a more 'normal' way? 
(define-parser-acceptor todo
  (awhen (accept 'string :stream :simple-parser)
    (or (find it *todos* :key #'description :test #'equal)
        (error "No such TODO item: ~A" it))))

;; this is for reading in from a stream. Really it would be better if this worked on a normal stream
(define-presentation-method accept ((type todo) (stream (eql :simple-parser)) (view org-view)
                                    &key error-if-not-eof)
  error-if-not-eof
  (awhen (simple-parser:sp-scan "^- \\[[X ]\\] ")
    (let ((item (make-instance 'todo :description (or (simple-parser:sp-scan "^.*")
                                                      (simple-parser:sp-error "Expected todo item")))))
      (when (equal "- [X] " it)
        (setf (done-p item) t))
      item)))
;; (accept-from-string 'todo "- [ ] Hi there" :view (make-instance 'org-view))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the TODO items

(defparameter *todos*
  (accept-from-string '((list todo) :separator-regex "^\\n")
                      (vip-utils:slurp-file "todo.org")
                      :view (make-instance 'org-view))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; COMMANDS

;; !!! I should make a new command table

(define-command (com-new-todo :name "New TODO"
                              :command-table *base-command-table*)
    ((item string))
  (let ((new (make-instance 'todo :description item)))
    ;; delete it if it already exists in the list
    (com-delete-todo-item new)
    ;; then add it to the end
    (setf *todos* (append *todos* (list new)))
    ;; since the return value is unimportant
    (values)))


(define-command (com-mark-as-done :name "Mark As Done"
                                  :command-table *base-command-table*)
    ((item todo))
  (setf (done-p item) t)
  (values))


(define-command (com-delete-todo-item :name "Delete TODO Item"
                                      :command-table *base-command-table*)
    ((item todo))
  (setf *todos* (remove item *todos* :test #'todo-eql))
  (values))

(define-command (com-move-item-above-item :name "Move Item Above Item"
                                          :command-table *base-command-table*
                                          :options '(:gesture :drag-and-drop
                                                     :drag-above t
                                                     :no-context-menu t))
    ((a todo)
     ;; Null will mean the end of the list
     (b (or (eql :end-of-list) todo)))
  (setf *todos*
        (append (loop for item in *todos*
                   when (and (typep b 'todo) (todo-eql b item)) collect a
                   when (not (todo-eql a item))
                   collect item)
                ;; stick it at the end if there was no item to move it above
                (when (eql :end-of-list b) (list a))))
  
  (values))

(define-command (com-save-todo-list :name "Save TODO List"
                                    :command-table *base-command-table*)
    ()
  (with-open-file (stream "todo.org"
                          :if-exists :supersede
                          :direction :output
                          :if-does-not-exist :create)
    (dolist (x *todos*)
      (present x 'todo :stream stream :view (make-instance 'org-view))
      (terpri stream)))
  
  "saved")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; APPLICATION

(defparameter *edit-view* (make-instance 'todo-editable-view))

(define-command (/app/todo-list :command-table *base-command-table*
                                :name "TODO List"
                                :return-type 'web-application)
    ()
  (show-application
   (with-web-monad
     (mhtml
       (:style "li {list-style-type: none;} * {font-family: Helvetica}")

       (:h1 "TODO")
       
       (:ul
        (dolist (x *todos*)
          (html-present x 'todo
                        :view *edit-view*
                        :element-type :li))

        ;; I'm going to use this partly so that I can drag items to the bottom
        (with-output-as-presentation (:html :end-of-list '(eql :end-of-list) :element-type :li)
          (html (:i :style (css :margin "4px") "--- end of list ---"))))
       
       (html-present (make-command 'com-new-todo)
                     'command
                     :view (make-instance 'url-view))

       (html-present (make-command 'com-save-todo-list)
                     'command
                     :view (make-instance 'url-view))))))





