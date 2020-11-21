;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defparameter *global-css*
  '("https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css" "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css"))

(defparameter *global-fonts*
  '("https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"))

(defparameter *blog-entry-css* 
  '("/default/css/blog-entry.css"))
;;maybe I will add an extra parameter

(defparameter *blog-entries-css*
  '("/default/css/blog-entries.css"))

(defparameter *blog-entry-footer* '())
(defparameter *blog-entries-footer* '())

(defmethod to-html ((entry blog-entry))
  (with-accessors ((title title)
                   (content content))
      entry
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:title title)
        (dolist (header (append *global-fonts* *global-css* *blog-entry-css*))
          (:link :rel "stylesheet" :href header)))
       (:body
        (:div :class "content"
              (:h1 title)
              (:h2 (category entry))
              (:h3 (creation-date entry)
                   (funcall content))))))))
;;need a way to handle footers

(defmethod to-html-body-only ((entry blog-entry))
  (with-accessors ((content content)
                   (title title)
                   (category category)
                   (creation-date creation-date))
      entry
    (spinneret:with-html
      (:body
       (:div :class "content"
             (:h1 title)
             (:h2 category)
             (:h3 creation-date)
             (funcall content))))))

(defun normalize-category-and-title (cat title)
  (concatenate 'string "/blog/"
               (str:replace-all " " "-" cat)
               "/"
               (str:replace-all " " "-" title)))

(defmethod to-html ((blog blog))
  (with-accessors ((entries entries))
      blog
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:title (title blog))
        (dolist (header (append *global-css* *global-fonts* *blog-entries-css*))
          (:link :rel "stylesheet" :href header)))
       (:body 
        (dolist (blog entries)
          (:div :class "entry"
                :id (id blog)
                (to-html-body-only blog))))))))
  
  
;;;; now we have the means to convert some text into a HTML entry
;;;;but we don't want the user to just enter text, instead I want them
;;;;to be able to write their entries using spinneret
