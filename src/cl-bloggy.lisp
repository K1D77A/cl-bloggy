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
    :initarg :body
    :documentation "This slot contains a spinneret HTML generation form")
   (footer
    :accessor footer)))

(defun make-html-skeleton (title content)
  (make-instance 'html-skeleton :title title :body content))

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
    :initform nil
    :type list)
   (title
    :accessor title
    :initform "Main page"
    :type string)
   (headers
    :accessor headers
    :initarg :headers
    :initform (list "/libraries/google-roboto/roboto.css"
                    "/libraries/normalize/normalize.css"
                    "/libraries/milligram-master/dist/milligram.min.css"
                    "/default/blog.css")
    :type list)))

(defun make-blog-entry (title category content)
  (let* ((html-skel (make-html-skeleton title content))
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
          (:link :rel "stylesheet" :href header)))
       (:body
        (:div :class "content"
              (:h1 (title html))
              (:h2 (category entry))
              (:h3 (creation-date entry))
              (funcall (body html))))))))

(defmethod to-html-body-only ((entry blog-entry))
  (let* ((html (html entry))
         (body (body html)));a list
    (spinneret:with-html
      (:body
       (:div :class "content"
             (:h1 (title html))
             (:h2 (category entry))
             (:h3 (creation-date entry))
             (funcall body))))))

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

(defmethod to-html ((blogs blog-entries))
  (with-accessors ((entries entries))
      blogs
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:title (title blogs))
        (dolist (header (headers blogs))
          (:link :rel "stylesheet" :href header)))
       (:body 
        (dolist (blog entries)
          (:div :class "entry" (to-html-body-only blog))))))))
  
  
;;;; now we have the means to convert some text into a HTML entry
;;;;but we don't want the user to just enter text, instead I want them
;;;;to be able to write their entries using spinneret
