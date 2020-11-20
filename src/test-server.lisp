(in-package :cl-bloggy)

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :document-root "./"
                                      :port 4203 :name 'main))
(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

;; (defmacro new-entry ((title category) ))

(add-route
 (make-route :get "/"
             (lambda () 
               (to-html
                (make-blog-entry "I am a title" "I am the category" '(:p "doof")))))
 *server*)

(add-route
 (make-route :get "/blog"
             (lambda ()
               (let ((blog-entries (blog-entries *server*)))
                 (to-html blog-entries))))
 *server*)

(defmacro new-blog-entry ((title category acceptor) &body body)
  (alexandria:with-gensyms (new-entry)
    `(with-accessors ((entries entries))
         (blog-entries ,acceptor)
       (let ((,new-entry
               (make-blog-entry ,title ,category
                                (lambda () (spinneret:with-html ,@body)))))
         (push ,new-entry entries)
         (add-route
          (make-route :GET
                      (normalize-category-and-title ,category ,title)
                      (lambda ()
                        (to-html ,new-entry)))
          ,acceptor)))))

(new-blog-entry ("entry1" "general" *server*)
  (:div :class "elp"
        (:h1 "A story to tell")
        (:p "once upon a time in a land far away")))

(new-blog-entry ("entry2" "general" *server*)
  (:div :class "elp"
        (:h1 "A second story to tell")
        (:p "A second time in a land far away")))



