;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defun minute-day-month-year-now (stream)
  (local-time:with-decoded-timestamp (:minute minute :hour hour :day day
                                      :month month :year year)
                                     (local-time:now)
    (format stream "~A-~A-~A at ~A:~A"
            day month year hour minute)))

(defclass html-skeleton ()
  ((title
    :initform "testtitle"
    :accessor title
    :initarg :title)
   (headers
    :accessor headers
    :initarg :headers
    :initform (list
               "/libraries/google-roboto/roboto.css"
               "/libraries/normalize/normalize.css"
               "/libraries/milligram-master/dist/milligram.min.css")
    :type list)
   (body
    :accessor body
    :initform "Look at me I'm a test")
   (footer
    :accessor footer)))

(defun make-html-skeleton (title)
  (make-instance 'html-skeleton :title title))

(defclass blog-entry ()
  ((html
    :accessor html
    :initarg :html
    :type html-skeleton)
   (css
    :accessor css
    :initarg :css
    :initform (list "/default/blog.css")
    :type list)
   (category
    :accessor category
    :initarg :category
    :type string)
   (creation-date
    :initform (minute-day-month-year-now nil)
    :accessor creation-date)
   (title
    :accessor title
    :initarg :title
    :type string)))

(defclass blog-entries ()
  ((entries
    :accessor entries
    :type list)))

(defun make-blog-entry (title category)
  (let* ((html-skel (make-html-skeleton title))
         (blog-entry (make-instance 'blog-entry :title title :category category)))
    (setf (headers html-skel) (append (headers html-skel) (css blog-entry))
          (html blog-entry) html-skel)
    blog-entry))

(defmethod to-html ((entry blog-entry))
  (let ((html (html entry)))
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:title (title html))
        (dolist (header (headers html))
          (:link :rel "style sheet" :href header)))
       (:body
        (:h1 (title html))
        (:h2 (category entry))
        (:h3 (creation-date entry))
        (:p (body html)))))))

(defun normalize-category-and-title (cat title)
  (concatenate 'string "/blog/"
               (str:replace-all " " "-" cat)
               "/"
               (str:replace-all " " "-" title)))

(defmethod to-route ((entry blog-entry))
  (with-accessors ((title title)
                   (category category))
      entry
    (add-route (make-route :GET (normalize-category-and-title category title)
                           (lambda ()(to-html entry)))
               *server*)))
