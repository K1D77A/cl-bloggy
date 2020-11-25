(in-package :cl-bloggy)

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :blog (make-instance 'blog
                                                           :title "Main")
                                      :document-root "./"
                                      :port 4203 :name 'main))

(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

(hunchentoot:define-easy-handler (root :uri "/dd")
    ()
  "boof")

(add-blog)
(add-index 'blog-index)

(easy-blog-entry (blog-entry "general" "entry1" *server*)
  (:div :class "elp"
        (:h1 "A story to tell")
        (:p "once upon a time in a land far away")))

(easy-blog-entry (blog-entry "general" "entry2" *server*)
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))

(defclass new-blog-entry (blog-entry)
  ((css-rules
    :initform '(()))));;overriding the default css rules found in
;;*blog-entry-css-rules*

(defclass my-blog (blog)
  ())

(defclass my-index (blog-index)
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
        (dolist (item (lorem-ipsum:paragraphs 5))
          (:p item))
        (:style :type "text/css"
                (lass:compile-and-write
                 '(h1 :font-size 3vw)))))



