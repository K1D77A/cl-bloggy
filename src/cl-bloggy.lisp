;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defgeneric normalize-category-and-title (e)
  (:documentation "This method produces a URL used to refer to a blog entry"))

(defmethod normalize-category-and-title ((entry blog-entry))
  (with-accessors ((cat category)
                   (title title))
      entry
    (concatenate 'string *blog-root-directory*
                 (str:replace-all " " "-" cat)
                 "/"
                 (str:replace-all " " "-" title))))

(defmacro easy-blog-entry ((blog-class category title acceptor &optional (let-bindings-to-override-global-vars nil)) &body body)
  "Takes BLOG-CLASS (a subclass of 'blog-entry' or an instance of blog-entry) 
and creates a new page at the url '*blog-root-directory*/CATEGORY/TITLE'.
You can add this to any arbitrary running instance of hunchentoot as long as it was
initiated using a subclass of the acceptor bloggy-acceptor. You can use the 
variable 'let-bindings-to-override-global-vars' to modify any global variable 
used in the render pipeline however I recommend you simply create subclass and
specialize methods on that subclass instead. BODY must be valid spinneret code, it
is an implicit (spinneret:with-html ,@body ) so you can enter any arbitrary HTML
using spinneret, use wisely."
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

(defun add-blog (&optional (path *blog-root-directory*))
  "Initializes the main blog page at PATH"
  (add-route
   (make-route :get path
               (lambda ()
                 (let ((blog (blog *server*)))
                   (to-html blog))))
   *server*))

(defun add-index (&optional (path *blog-index-directory*))
  "Initializes the main blog index at PATH"
  (add-route
   (make-route :get path
               (lambda ()
                 (let ((index (make-instance 'blog-index :blog (blog *server*))))
                   (to-html index))))
   *server*))

