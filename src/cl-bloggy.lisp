;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defgeneric process-uri (e key)
  (:documentation "Generates the correct url for E. Key is the method to use."))

(defmethod process-uri ((entry entry) (key (eql :encode)))
  (with-accessors ((cat category)
                   (title title))
      entry
    (let ((names (category-names cat)))
      (reduce #'str:concat
              (append
               (list (url (blog entry)))
               (mapcar (lambda (name)
                         (str:concat (do-urlencode:urlencode name) "/"))
                       names)
               (list (do-urlencode:urlencode title)))))))

(defmethod process-uri ((uri string) (key (eql :decode)))
  (let ((split (str:split "/" uri)))
    (format nil "~{~A~^/~}"
            (mapcar (lambda (e)
                      (do-urlencode:urldecode e))
                    split))))

(defmethod category-names ((category category))
  (nreverse (append (list (name category))
                    (loop :for cat :in (parents category)
                          :collect (name cat)))))

(defun entries-in-category (category blog)
  (loop :for entry :in (entries blog)
        :when (or (eq (category entry) category)
                  (with-accessors ((parents parents))
                      category
                    (when parents
                      (loop :for cat :in parents
                              :thereis (eq (category entry) cat)))))
          :collect entry))

(defmacro easy-blog-entry ((blog-class number categories title date acceptor
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
             (new-blog-entry (blog ,acceptor) ',blog-class ,number ,title
                             (first (find-categories ',categories (blog ,acceptor)))
                             (apply #'new-date-timestamp ',date)
                             (lambda (entry) (declare (ignorable entry)) (spinneret:with-html ,@body)))))
       (add-route
        (make-route :GET
                    (process-uri ,new-entry :encode)
                    (lambda ()
                      (if ',let-bindings-to-override-global-vars
                          (let ,let-bindings-to-override-global-vars
                            (to-html ,new-entry))
                          (to-html ,new-entry))))
        ,acceptor))))

(defun find-category (name blog &optional (createp t))
  "Attempts to find the category denoted by the string NAME, if it can't makes a new one."
  (with-accessors ((categories categories))
      blog
    (let ((category (find name categories :key #'name :test #'string-equal)))
      (if category
          category
          (if createp 
              (let ((cat (make-instance 'category :name name)))
                (push cat categories)
                cat)
              nil)))))
        
(defun find-categories (names blog &optional (cats nil))
  "name is a list"
  (with-accessors ((categories categories))
      blog
    (loop :for (name . children) :on names
          :with cats := ()
          :do (let ((category (find-category name blog)))
                (setf (subcategories category)
                      (find-categories children blog cats)
                      (parents category)
                      cats)
                (push category cats))
          :finally (return cats))))

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


(defgeneric format-timestamp (stream timestamp way)
  (:documentation "formats timestamp into stream by WAY."))

(defmethod format-timestamp (stream timestamp (way (eql :site)))
  (local-time:format-timestring stream timestamp
                                :format '(:ordinal-day " of " :long-month " "
                                          :year  " at " :hour12 ":" :min :ampm)))

(defmethod format-timestamp (stream timestamp (way (eql :rss)))
  (local-time:format-timestring stream timestamp 
                                :format local-time:+rfc-1123-format+))

