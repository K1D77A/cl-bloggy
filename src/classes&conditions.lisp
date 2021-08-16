(in-package #:cl-bloggy)

(defparameter *blog-root-directory* "/blog/")
(defparameter *blog-index-directory* (str:concat *blog-root-directory* "index"))

(defclass entry (blog)
  ((category
    :accessor category
    :initarg :category
    :type category)
   (date
    :reader date
    :initarg :date 
    :type string
    :documentation "The date")
   (title
    :accessor title
    :initarg :title
    :type string)
   (order
    :accessor order
    :initarg :order
    :type fixnum)
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
    :type list
    :allocation :class)
   (title
    :accessor title
    :initarg :title
    :initform "Main page"
    :type string
    :allocation :class)
   (categories
    :accessor categories
    :initarg :categories
    :initform ()
    :type list)
   (url
    :reader url
    :initarg :url
    :initform *blog-root-directory*)))

(defclass category ()
  ((name
    :accessor name
    :initarg :name)
   (subcategories
    :accessor subcategories
    :initarg :subcategories)
   (parents
    :accessor parents
    :initarg :parents)))

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

(defmethod add-new-blog ((blog blog) (entry entry))
  (setf (entries blog)
        (delete-if (lambda (ent)
                     (or  (= (order ent) (order entry))
                          (string-equal (id ent) (id entry))))
                   (entries blog)))
  (push entry (entries blog)))

(defun new-blog-entry (blog blog-class number title category date content)
  (check-type content function)
  (let ((entry (make-instance blog-class 
                              :category category
                              :date date
                              :order number
                              :title title
                              :content content
                              :id (make-id category title))))
    (add-new-blog blog entry)
    entry))

