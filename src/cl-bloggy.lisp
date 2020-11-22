;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defmethod normalize-category-and-title ((entry blog-entry))
  (with-accessors ((cat category)
                   (title title))
      entry
    (concatenate 'string "/blog/"
                 (str:replace-all " " "-" cat)
                 "/"
                 (str:replace-all " " "-" title))))

(defmacro easy-blog-entry ((blog-class category title acceptor &optional (let-bindings-to-override-global-vars nil)) &body body)
  (alexandria:with-gensyms (new-entry)
    `(let ((,new-entry
             (new-blog-entry (blog ,acceptor) ',blog-class ,title ,category
               (lambda () (spinneret:with-html ,@body)))))
       (add-route
        (make-route :GET
                    (normalize-category-and-title ,new-entry)
                    (lambda ()
                      (if ',let-bindings-to-override-global-vars
                          (let ,let-bindings-to-override-global-vars
                            (to-html ,new-entry))
                          (to-html ,new-entry))))
        ,acceptor))))

