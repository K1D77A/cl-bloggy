;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defgeneric normalize-category-and-title (e)
  (:documentation "This method produces a URL used to refer to a blog entry"))

(defmethod normalize-category-and-title ((entry entry))
  (with-accessors ((cat category)
                   (title title))
      entry
    (concatenate 'string *blog-root-directory*
                 (str:replace-all " " "-" cat)
                 "/"
                 (str:replace-all " " "-" title))))

(defmacro easy-blog-entry ((blog-class number category title date acceptor
                            &optional (let-bindings-to-override-global-vars nil)) &body body)
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
             (new-blog-entry (blog ,acceptor) ',blog-class ,number ,title ,category ,date
                             (lambda (entry) (declare (ignorable entry)) (spinneret:with-html ,@body)))))
       (add-route
        (make-route :GET
                    (normalize-category-and-title ,new-entry)
                    (lambda ()
                      (if ',let-bindings-to-override-global-vars
                          (let ,let-bindings-to-override-global-vars
                            (to-html ,new-entry))
                          (to-html ,new-entry))))
        ,acceptor))))

(defun add-blog (acceptor blog-class)
  "Initializes the main blog page at PATH"
  (unless (slot-boundp acceptor 'blog)
    (setf (blog acceptor)
          (make-instance blog-class)))
  (add-route
   (make-route :get (url (make-instance blog-class))
               (lambda ()
                 (let ((blog (blog acceptor)))
                   (to-html blog))))
   acceptor))

(defun add-index (acceptor index-class)
  "Initializes the main blog index at PATH"
  (add-route
   (make-route :get (url (make-instance index-class))
               (lambda ()
                 (let ((index (make-instance index-class :blog (blog acceptor))))
                   (to-html index))))
   acceptor))

(defmethod delete-entry ((acceptor bloggy-acceptor) (entry entry))
  (with-accessors ((blog blog)
                   (routes routes))
      acceptor
    (let ((uri (normalize-category-and-title entry)))
      (setf routes
            (remove uri routes :key #'second :test #'string-equal)
            (entries blog)
            (remove entry (entries blog) :test #'eq)))))

(defmethod delete-entry ((acceptor bloggy-acceptor) (entry null)))

(defmethod delete-entry ((acceptor bloggy-acceptor) (entry number))
  (let* ((blog (blog acceptor))
         (entries (entries blog)))
    (delete-entry acceptor (find entry entries :key #'order :test #'=))))




