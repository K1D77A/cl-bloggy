(in-package #:cl-bloggy)

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
    :accessor creation-date)
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
   (title
    :accessor title
    :initarg :title
    :initform "Main page"
    :type string)
   (headers
    :accessor headers
    :initarg :headers
    :type list)
   (footers
    :accessor footers
    :initarg :footers
    :type list)))

(defun make-blog (main-title)
  (make-instance 'blog :title main-title))

(defun clean-string (string)
  "downcases, replaces spaces with hypens and removes white space"
  (string-downcase (str:replace-all " " "-" (str:trim string))))

(defun make-id (category title)
  (clean-string (str:concat category title)))

(defmethod add-new-blog ((blog blog) (entry blog-entry))
  (setf (entries blog)
        (delete-if (lambda (ent)
                     (string-equal (id ent) (id entry)))
                   (entries blog)))
  (push entry (entries blog)))

(defmethod new-blog-entry ((blog-entries blog) title category content)
  (check-type content function)
  (let ((entry (make-instance 'blog-entry
                              :category category
                              :title title
                              :content content
                              :id (make-id category title))))
    (add-new-blog blog-entries entry)
    entry))

