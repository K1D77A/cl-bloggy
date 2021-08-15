(in-package :cl-bloggy)

(defclass my-index (index)
  ())

(defclass my-entry (entry)
  ())

(defclass my-blog (blog)
  ((title :initform "K1D77A's Test Blog")))

(defparameter *server* (make-instance 'bloggy-acceptor                                      
                                      :document-root "./"
                                      :port 4203 :name 'main))

(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

(hunchentoot:define-easy-handler (root :uri "/dd")
    ()
  "boof")

(add-blog *server* 'my-blog)
(add-index *server* 'my-index)

(easy-blog-entry (my-entry 1 "general" "A story to tell" "23rd of June" *server*)
  (:div :class "elp"
        (:p "once upon a time in a land far away")))

(easy-blog-entry (my-entry 2 "general" "Fun on FFXIV" "24th of June" *server* )
  (:div :class "elp"
        (:p "A second time in a land far away")))

(easy-blog-entry (my-entry 3 "general" "I just don't understand." "26th of June" *server*)
  (:div :class "elp"
        (dolist (item (lorem-ipsum:paragraphs 5))
          (:p item))))



