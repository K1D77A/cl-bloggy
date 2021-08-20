(in-package #:cl-bloggy)

(defparameter *blog-root-directory* "/blog/main")
(defparameter *blog-index-directory* "/blog/index")

(defclass special-request ()
  ((acceptor
    :reader acceptor
    :initarg :acceptor)
   (request
    :reader request
    :initarg :request)
   (uri
    :reader uri
    :initarg :uri)
   (split-uri
    :reader split-uri
    :initarg :split-uri)
   (category
    :accessor category
    :initarg :category)
   (r-method
    :reader r-method
    :initarg :r-method)))

(defclass rss-request (special-request)
  ())

(defclass category-request (special-request)
  ())

(defclass rss-category-request (category-request rss-request)
  ())

(defclass atom-request (special-request)
  ())


(defclass entry ()
  ((category
    :accessor category
    :initarg :category
    :type category)
   (date
    :reader date
    :initarg :date 
    :type local-time:timestamp
    :documentation "The date")
   (sym
    :reader sym
    :initarg :sym
    :type keyword
    :documentation "A keyword used to reference this entry.")
   (title
    :accessor title
    :initarg :title
    :initform nil
    :type (or null function))
   (subtitle
    :accessor subtitle
    :initarg :subtitle
    :initform nil
    :type (or null function))
   (id
    :reader id
    :initarg :id
    :type string
    :documentation "An amalgamation of title and category")
   (content
    :accessor content
    :initarg :content
    :type function)
   (description
    :accessor description
    :initarg :description
    :initform nil
    :type (or function null))
   (blog
    :accessor blog
    :initarg :blog
    :type blog)))

(defclass blog ()
  ((entries
    :accessor entries
    :initarg :entries
    :initform nil
    :type list)
   (title
    :accessor title
    :initarg :title
    :initform (lambda (blog)
                (declare (ignore blog))
                "Main page")
    :type (or function nil))
   (categories
    :accessor categories
    :initarg :categories
    :initform ()
    :type list)
   (author
    :accessor author
    :initarg :author
    :initform (lambda (blog)
                (declare (ignore blog))
                "Author")
    :type (or function nil))
   (domain
    :accessor domain
    :initarg domain
    :type string)
   (description
    :accessor description
    :initarg :description
    :initform (lambda (blog)
                (declare (ignore blog))
                "My blog")
    :type (or function nil))
   (language
    :accessor language
    :initarg :language
    :initform "en-gb"
    :type string)
   (index
    :accessor index
    :initarg :index
    :type index)
   (content
    :accessor content
    :initarg :content
    :type content)
   (url
    :reader url
    :initarg :url
    :initform *blog-root-directory*)))

(defclass category ()
  ((name
    :accessor name
    :initarg :name)
   (sym
    :accessor sym
    :initarg :sym)
   (children
    :accessor children
    :initarg :children
    :initform nil)
   (parent
    :accessor parent
    :initarg :parent
    :initform nil)))

(defmethod print-object ((ob category) stream)
  (print-unreadable-object (ob stream) :type t
    (format stream "Name: ~A" (name ob))))

(defclass content ()
  ((url
    :reader url
    :initform "/blog/content")
   (blog
    :reader blog
    :initarg :blog)
   (content
    :accessor content
    :initarg :content
    :initform ()
    :type list)))

(defclass index (blog)
  ((blog
    :reader blog
    :initarg :blog)
   (url
    :initform *blog-index-directory*)))

(defun make-blog (main-title)
  (make-instance 'blog :title main-title))

(defun clean-string (string)
  "downcases, replaces spaces with hyphens and removes white space"
  (string-downcase (str:replace-all " " "-" (str:trim string))))

(defun make-id (category title)
  (clean-string (reduce #'str:concat (append (category-names category)
                                             (list title)))))

(defun new-date-timestamp (&key (nsec 0)
                             (sec 0)
                             (minute 0)
                             (hour 0)
                             (day 1)
                             (month 1)
                             (year 2021)
                             (timezone local-time:*default-timezone*)
                             offset
                             into)
  (local-time:encode-timestamp nsec sec minute hour day
                               (%convert-month-to-n month) year
                               :timezone timezone :offset offset :into into))

(defmethod %convert-month-to-n ((month fixnum))
  month)

(defmethod %convert-month-to-n ((month string))
  (let ((months
          '("january"
            "february"
            "march"
            "april"
            "may"
            "june"
            "july"
            "august"
            "september"
            "october"
            "november"
            "december")))
    (loop :for mon :in months
          :for x :from 1 :to (length months)
          :when (str:containsp (string-downcase month) mon)
            :return  x)))

(defmethod add-new-blog ((blog blog) (entry entry))
  (with-accessors ((entries entries))
      blog
    (setf entries
          (delete-if (lambda (ent)
                       (eq (sym ent) (sym entry)))
                     entries))
    (push entry entries)
    (setf entries
          (sort entries #'local-time:timestamp>= :key #'date))))

(defun new-blog-entry (blog blog-class title sym category date content
                       &key (subtitle nil)
                         (description nil))
  (check-type content function)
  (let ((entry (make-instance blog-class 
                              :category category
                              :date date
                              :sym sym
                              :subtitle
                              (if (stringp subtitle)
                                  (lambda (e) (declare (ignore e))
                                    subtitle)
                                  subtitle)
                              :description 
                              (if (stringp description)
                                  (lambda (e) (declare (ignore e))
                                    description)
                                  description)
                              :title
                              (if (stringp title)
                                  (lambda (e) (declare (ignore e))
                                    title)
                                  title)
                              :content content
                              :blog blog 
                              :id (make-id category title))))
    (add-new-blog blog entry)
    entry))

