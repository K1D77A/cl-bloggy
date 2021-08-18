(in-package #:cl-bloggy)

(defclass display-condition ()
  ((c
    :reader c    
    :initarg :c
    :documentation "the condition"))
  (:documentation "used to make the generation of HTML customizable."))

(define-condition bloggy-condition (error)
  ()
  (:documentation "Top level condition for cl-bloggy."))

(define-condition request-condition (bloggy-condition)
  ((http-code
    :reader http-code
    :initarg :http-code
    :initform 400
    :documentation "The returned HTTP code.")
   (blog
    :reader blog
    :initarg :blog
    :documentation "the blog")
   (message
    :reader message
    :initarg :message
    :initform ""
    :documentation "An optional message"))
  (:documentation "All conditions that are related to user requests.")
  (:report
   (lambda (obj stream) (display-condition stream obj :internal))))

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

(defgeneric display-condition (stream condition way &rest args)
  (:documentation "Displays the condition to the user."))

(defmethod display-condition :around (stream (condition request-condition)
                                      (way (eql :internal)) &rest args)
  (declare (ignore args))
  (format stream "ERROR: ~A. HTTP-CODE: ~A. ~A. MESSAGE: ~A."
          (class-name (class-of condition))
          (http-code condition)
          (call-next-method)
          (message condition)))

(defmethod display-condition (stream (condition request-condition)
                              (way (eql :html)) &rest args)
  (declare (ignore args))
  (let ((page (make-instance (condition-display-class (blog condition))
                             :c condition)))
    (to-html page)))

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
