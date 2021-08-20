(in-package #:cl-bloggy)

(define-condition bloggy-condition (error)
  ((message
    :reader message
    :initarg :message
    :initform ""
    :documentation "An optional message"))
  (:documentation "Top level condition for cl-bloggy.")
  (:report (lambda (obj stream) (display-condition stream obj :internal))))

(define-condition missing-required-feature (bloggy-condition)
  ((feature
    :accessor feature
    :initarg :feature
    :documentation "The required feature that is missing.")
   (instructions
    :accessor instructions
    :initarg :instructions
    :type string
    :documentation "A string telling the user how to fix the problem"))
  (:documentation "Signalled when trying to use a function that needs a feature, 
for example you try to use easy-image but havent called (add-blog ...)."))

(define-condition unknown-content (bloggy-condition)
  ((content
    :accessor content
    :initarg :content
    :documentation "The content you looked for. A keyword."))
  (:documentation "Signalled when content is missing."))

(define-condition request-condition (bloggy-condition)
  ((http-code
    :reader http-code
    :initarg :http-code
    :initform 400
    :documentation "The returned HTTP code.")
   (blog
    :reader blog
    :initarg :blog
    :documentation "the blog"))
  (:documentation "All conditions that are related to user requests.")
  (:report
   (lambda (obj stream) (display-condition stream obj :internal))))

(define-condition exceeded-category-depth (request-condition)
  ((http-code :initform 400)
   (cat-list
    :accessor cat-list
    :initarg :cat-list
    :type list
    :documentation "the category request found while processing the URI"))
  (:documentation "Signalled by find-category when the category list is greater than 
*max-category-depth*")
  (:report
   (lambda (obj stream)
     (display-condition stream obj :internal))))

(define-condition rss%bad-categories (request-condition)
  ((http-code :initform 404)
   (category
    :reader category
    :initarg :category
    :initform nil 
    :documentation "List of bad categories provided when rss request is made."))
  (:documentation "Signalled when someone makes a request to rss.xml but 
they have used bad categories."))

(define-condition missing-categories (request-condition)
  ((http-code :initform 404)
   (category
    :reader category
    :initarg :category
    :initform nil
    :documentation "List of the categories the user provided."))
  (:documentation "Signalled when someone makes a request for sorting by categories 
but the categories they provided can't be found."))

(define-condition missing-content (request-condition)
  ((http-code :initform 404))
  (:documentation "Signalled when someone makes a request to a url that doesn't exist."))

(define-condition malformed-url (request-condition)
  ((http-code :initform 400))
  (:documentation "Signalled when someone makes a request that is neither for the index 
or main but is missing blog/main."))

(defgeneric display-condition (stream condition way &rest args)
  (:documentation "Displays the condition to the user. 
WAY is a keyword argument, right now there are two :internal and :html, 
:internal displays the condition as normal, while :html is used for rendering HTML for
the condition. If you look in src/generate-html.lisp you can see how the 
request-conditions are formatted for HTML output"))

(defmethod display-condition :around (stream (condition request-condition)
                                      (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format stream "ERROR: ~A. HTTP-CODE: ~A. ~A. MESSAGE: ~A."
          (class-name (class-of condition))
          (http-code condition)
          (call-next-method)
          (message condition)))

(defmethod display-condition (stream condition (way (eql :html)) &rest args)
  (declare (ignore args))
  (to-html condition))

(defmethod display-condition (stream condition way &rest args)
  (declare (ignore args))
  "")

(defmethod display-condition (stream (condition rss%bad-categories)
                              (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format nil "CATEGORIES: ~{~A~^, ~}" (category condition)))

(defmethod display-condition (stream (condition missing-categories)
                              (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format nil "CATEGORIES: ~{~A~^, ~}" (category condition)))

(defmethod display-condition (stream (condition missing-required-feature)
                              (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format stream "You are missing the feature ~A.~%Follow: ~A."
          (feature condition)
          (instructions condition)))

(defmethod display-condition (stream (condition unknown-content)
                              (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format stream "Couldn't find the content under key: ~A.~% MESSAGE ~A"
          (content condition)
          (message condition)))

