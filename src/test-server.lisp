(in-package :cl-bloggy)

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :document-root "./"
                                      :port 4203 :name 'main))

(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

;; (defmacro new-entry ((title category) ))
(hunchentoot:define-easy-handler (root :uri "/dd")
    ()
  "boof")

(add-route
 (make-route :get "/blog"
             (lambda ()
               (let ((blog (blog *server*)))
                 (to-html blog))))
 *server*)

(defmacro easy-blog-entry ((category title acceptor &optional (let-bindings-to-override-global-vars nil)) &body body)
  (alexandria:with-gensyms (new-entry)
    `(let ((,new-entry
             (new-blog-entry (blog ,acceptor) ,title ,category
               (lambda () (spinneret:with-html ,@body)))))
       (add-route
        (make-route :GET
                    (normalize-category-and-title ,category ,title)
                    (lambda ()
                      (if ',let-bindings-to-override-global-vars
                          (let ,let-bindings-to-override-global-vars
                            (to-html ,new-entry))
                          (to-html ,new-entry))))
        ,acceptor))))

(easy-blog-entry ("general" "entry1" *server*)
  (:div :class "elp"
        (:h1 "A story to tell")
        (:p "once upon a time in a land far away")))

(easy-blog-entry ("general" "entry2" *server*)
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))

(easy-blog-entry ("general" "entry3" *server* ((*global-css* nil)))
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))



