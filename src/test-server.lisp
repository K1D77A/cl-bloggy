(in-package :cl-bloggy)

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :blog (make-instance 'my-blog :title "Main")
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

(easy-blog-entry (blog-entry "general" "entry1" *server*)
  (:div :class "elp"
        (:h1 "A story to tell")
        (:p "once upon a time in a land far away")))

(easy-blog-entry (blog-entry "general" "entry2" *server*)
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))

(defclass new-blog-entry (blog-entry)
  ())

(defclass my-blog (blog)
  ())

(defmethod html-footer ((blog my-blog))
  (spinneret:with-html
    (:p "i'm a different footer for the main page!!")))

(defmethod html-footer ((entry new-blog-entry))
  (spinneret:with-html
    (:p "i'm a different footer!!")))

(easy-blog-entry (new-blog-entry "general" "entry4" *server*)
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))



