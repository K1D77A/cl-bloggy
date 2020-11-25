(in-package #:cl-bloggy)

(defmethod global-css (item)
  (spinneret:with-html
    (:link :rel "stylesheet" :href
           "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css")
    (:link :rel "stylesheet" :href
           "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css")))

(defmethod global-fonts (item)
  (spinneret:with-html
    (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic")))

(defmethod specific-css ((entry blog-entry))
  (spinneret:with-html
    (:link :rel "stylesheet" :href "/default/css/blog-entry.css")))

(defmethod specific-css ((blog blog))
  (spinneret:with-html
    (:link :rel "stylesheet" :href "/default/css/blog-entries.css")))

(defmethod specific-css (item)
  nil)

(defmethod global-footer (item)
  nil)

(defmethod specific-footer ((entry blog-entry))
  (spinneret:with-html
    (:p :id "in-footer" "made with cl-bloggy")))

(defmethod specific-footer ((blog blog))
  (spinneret:with-html
    (:p :id "in-footer" "made with cl-bloggy")))

(defmethod to-html ((entry blog-entry))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (html-headers entry))
     (:body
      (:a :id "home-link" :href "/blog" "Home")
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
      (html-body blog)
      (:footer
       (html-footer blog))))))

(defmethod html-headers ((entry blog-entry))
  (spinneret:with-html
    (:title :id "header-title" (title entry))
    (dolist (header (list 'global-css 'global-fonts 'specific-css))
      (funcall header entry))
    (to-css entry)))

(defmethod html-headers ((blog blog))
  (spinneret:with-html
    (:title (title blog))
    (dolist (header (list 'global-css 'global-fonts 'specific-css))
      (funcall header blog))
    (to-css blog)))

(defmethod html-body ((entry blog-entry))
  (spinneret:with-html
    (:div :id "content"          
          (:a :id "body-title"
              :href (normalize-category-and-title entry)
              (title entry))
          (:h2 :id "body-h2" (category entry))
          (:h3 :id "body-h3" (creation-date entry))
          (:div :id "user-content"
                (funcall (content entry))))))

(defmethod html-body ((index blog-index))
  (spinneret:with-html
    (:div :id "entries-list"
          (dolist (entry (entries (blog index)))
            (:div :class "index-entry"
                  (:a  :href (normalize-category-and-title entry)
                       (format nil "Title: ~A Date: ~A"
                               (title entry) (creation-date entry))))))))

  (defmethod html-body ((blog blog))
    (spinneret:with-html
      (:div :id "all-entries"
            (dolist (blog (sort (entries blog) #'< :key #'creation-date-universal))
              (:div :class "entry"
                    :id (id blog)
                    (html-body blog))))))

(defmethod html-footer ((entry blog-entry))
  (spinneret:with-html
    (:div :id "footer"
          (dolist (footer (list 'global-footer 'specific-footer))
            (funcall footer entry)))))

(defmethod html-footer ((blog blog))
  (spinneret:with-html
    (:div :id "footer"
          (dolist (footer (list 'global-footer 'specific-footer))
            (funcall footer blog)))))
