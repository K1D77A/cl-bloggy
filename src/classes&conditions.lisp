(in-package #:cl-bloggy)

(defparameter *blog-root-directory* "/blog/")
(defparameter *blog-index-directory* (str:concat *blog-root-directory* "index"))

(defun minute-day-month-year-now (stream)
  (local-time:with-decoded-timestamp (:minute minute :hour hour :day day
                                      :month month :year year)
                                     (local-time:now)
    (format stream "~A-~A-~A at ~A:~A"
            day month year hour minute)))

(defclass blog-entry ()
  ((category
    :accessor category
    :initarg :category
    :type string)
   (creation-date
    :initform (minute-day-month-year-now nil)
    :reader creation-date)
   (creation-date-universal
    :initform (get-universal-time)
    :accessor creation-date-universal)
   (css-rules
    :initform *blog-entry-css-rules*
    :accessor css-rules)
   (title
    :accessor title
    :initarg :title
    :type string)
   (id
    :reader id
    :initarg :id
    :type string
    :documentation "An amalgamation of title and category")
   (content
    :accessor content
    :initarg :content
    :type function)))

(defclass blog ()
  ((entries
    :accessor entries
    :initform nil
    :type list)
   (css-rules
    :accessor css-rules
    :initform *blog-css-rules*)
   (title
    :accessor title
    :initarg :title
    :initform "Main page"
    :type string)))

(defclass blog-index (blog)
  ((blog
    :accessor blog
    :initarg :blog
    :type blog)
   (css-rules
    :accessor css-rules
    :initform *index-css-rules*)))

(defun make-blog (main-title)
  (make-instance 'blog :title main-title))

(defun clean-string (string)
  "downcases, replaces spaces with hyphens and removes white space"
  (string-downcase (str:replace-all " " "-" (str:trim string))))

(defun make-id (category title)
  (clean-string (str:concat category title)))

(defmethod add-new-blog ((blog blog) (entry blog-entry))
  (setf (entries blog)
        (delete-if (lambda (ent)
                     (string-equal (id ent) (id entry)))
                   (entries blog)))
  (push entry (entries blog)))

(defmethod new-blog-entry ((blog-entries blog) blog-class title category content)
  (check-type content function)
  (let ((entry (make-instance blog-class 
                              :category category
                              :title title
                              :content content
                              :id (make-id category title))))
    (add-new-blog blog-entries entry)
    entry))

