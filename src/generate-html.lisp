(in-package #:cl-bloggy)

(defparameter *default-css*
  '("https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css"
    "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css"
    "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"))

(defgeneric to-html (e)
  (:documentation "The entry function used to create HTML pages. This method calls
'html-headers' html-body' and 'html-footer' in that order in order to render a 
page. You can create your own version of this method to modify the functionality
for your own subclasses the same goes for the three methods it calls."))

(defmethod to-html :around (e)
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (html-headers e))
     (:body
      (call-next-method)
      (html-body e)
      (:footer
       (html-footer e))))))

(defmethod to-html ((entry entry))
  (spinneret:with-html 
    (:a :id "home-link" :href (url (blog entry)) "Home")
    (:a :id "index-link" :href (url (index (blog entry))) "Index")))

(defmethod to-html ((c request-condition))
  (spinneret:with-html
    (:a :id "home-link" :href (url (blog c)) "Home")
    (:a :id "index-link" :href (url (index (blog c))) "Index")))

(defmethod to-html ((index index))
  (spinneret:with-html
    (:a :id "home-link" :href (url (blog index)) "Home")))

(defmethod to-html (page)
  nil)

(defgeneric html-headers (page)
  (:documentation "Applies the default css sheets listed in *default-css* to the header
and then evaluates call-next-method."))

(defmethod html-headers :around (page)
  (spinneret:with-html
    (dolist (css *default-css*)
      (:link :rel "stylesheet" :href css))
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
  (:documentation "Displays PAGE the correct way. If you wanted to change the layout
of a certain page, you would create a new version of html-body for your subclass of 
that page. In that case it would be best to simply copy and paste the code for the 
superclass and then play with it that way, you dont want to end up breaking functionality."))

(defun %format-tags (blog category)
  (spinneret:with-html 
    (:span "Tags: "
           (mapc (lambda (tag url)
                   (:span
                    (:a :href url
                        (format nil "~:(~A~)" tag) " ")))
                 (category-names category)
                 (category-all-urls category blog)))))


(defmethod html-body ((entry entry))
  (with-accessors ((id id)
                   (title title)
                   (date date)
                   (content content)
                   (category category)
                   (subtitle subtitle)
                   (blog blog))
      entry
    (spinneret:with-html
      (:div :class "content"
            (:div :id id
                  (:a :href
                      (process-uri entry :encode)
                      (:h2 :class "title" (funcall title entry)))
                  (:h3 :class "subtitle"
                       (when subtitle (funcall subtitle entry)))
                  (:h4 :class "date" (format-timestamp nil date :site))
                  (:div :class "tags"
                        (%format-tags blog category))
                  (:div :id "user-content"
                        (funcall content entry)))))))

(defmethod html-body ((index index))
  (with-accessors ((title title)
                   (blog blog))
      index 
    (spinneret:with-html
      (:div :id "entries-list"
            (dolist (entry (entries blog))
              (:div :class "index-entry"
                    (:a :href (process-uri entry :encode)
                        (:h2 :class "index-title title"
                             (funcall (title entry) entry))
                        (:h4 :class "index-tags tags"
                             (%format-tags blog (category entry)))
                        (:h3 :class "index-date date"
                             (format-timestamp nil (date entry) :site))
                        (:h4 :class "index-description description"
                             (when (description entry)
                               (funcall (description entry) entry))))))))))

(defmethod html-body ((blog blog))
  (with-accessors ((title title)
                   (description description)
                   (entries entries)
                   (content content))
      blog
    (spinneret:with-html
      (:div :class "title-box"
            (:a :id "home-link" :href (url blog) "Home")
            (:a :id "index-link" :href (url (index blog)) "Index")
            (:div :class "title-and-icons"
                  (:h1 :class "blog-title title" (funcall title  blog))
                  (:a :class "rss-link" :href (format nil "~A/rss.xml"
                                                      (url blog))
                      (:img :class "rss-icon"
                            :src (url (find-content blog :rss-png)))))
            (:h2 :class "blog-description description" (funcall description blog)))
      (:div :class "entries"
            (dolist (blog entries)
              (:div :class "entry"
                    :id (id blog)
                    (html-body blog)))))))

(defmethod html-body ((c request-condition))
  (with-accessors ((http-code http-code)
                   (message message))
      c 
    (spinneret:with-html
      (:h1 :class "http-code" http-code)
      (:h3 :class "message" message))))

(defgeneric html-footer (page)
  (:documentation "Appends a footer to PAGE."))

(defmethod html-footer :around (page)
  (spinneret:with-html
    (:div :id "footer"
          (call-next-method))))


(defmethod html-footer (page)
  nil)

