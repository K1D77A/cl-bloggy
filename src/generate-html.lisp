(in-package #:cl-bloggy)

(defgeneric to-html (e)
  (:documentation "The entry function used to create HTML pages. This method calls
'html-headers' html-body' and 'html-footer' in that order in order to render a 
page. You can create your own version of this method to modify the functionality
for your own subclasses the same goes for the three methods it calls."))

(defmethod to-html ((entry entry))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (html-headers entry))
     (:body
      (:a :id "home-link" :href "/blog/" "Home")
      (html-body entry)
      (:footer
       (html-footer entry))))))

(defmethod to-html ((blog blog))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (html-headers blog))
     (:body 
      (html-body blog))
     (:footer
      (html-footer blog)))))

(defmethod html-headers :around (page)
  (spinneret:with-html
    (:link :rel "stylesheet" :href
           "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css")
    (:link :rel "stylesheet" :href
           "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css")
    (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic")
    (call-next-method)
    (page-css page)))

(defmethod html-headers (page)
  nil)

(defmethod html-headers ((entry entry))
  (spinneret:with-html
    (:title :id "header-title" (title entry))))

(defmethod html-headers ((blog blog))
  (spinneret:with-html
    (:title (title blog))))

(defgeneric html-body (page)
  (:documentation "Displays PAGE the correct way."))

(defmethod html-body ((entry entry))
  (with-accessors ((id id)
                   (title title)
                   (date date)
                   (content content))
      entry
    (spinneret:with-html
      (:div :class "content"
            (:div :id id
                  (scoped-css entry)
                  (:a :href
                      (normalize-category-and-title entry)
                      (:h2 :class "purple" title))
                  (:h4 :class "purple" date)
                  (:div :id "user-content"
                        (funcall content entry)))))))

(defmethod html-body ((index index))
  (spinneret:with-html
    (:div :id "entries-list"
          (dolist (entry (entries (blog index)))
            (:div :class "index-entry"
                  (:a :href (normalize-category-and-title entry)
                      (format nil "Title: ~A Date: ~A"
                              (title entry) (date entry))))))))

(defmethod html-body ((blog blog))
  (spinneret:with-html
    (:h1 :class "purple" (title blog))
    (:div :id "all-entries"
          (dolist (blog (sort (entries blog) #'< :key #'order))
            (:div :class "entry"
                  :id (id blog)
                  (html-body blog))))))

(defmethod html-footer :around (page)
  (spinneret:with-html
    (:div :id "footer"
          (call-next-method))))

(defmethod html-footer (page)
  nil)
