(in-package #:cl-bloggy)

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
    (:a :id "home-link" :href (url (blog entry)) "Home")))

(defmethod to-html ((c display-condition))
  (spinneret:with-html
    (:a :id "home-link" :href (url (blog (c c))) "Home")))



(defmethod to-html ((blog blog))
  nil)

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
                   (content content)
                   (category category))
      entry
    (spinneret:with-html
      (:div :class "content"
            (:div :id id
                  (scoped-css entry)
                  (:a :href
                      (process-uri entry :encode)
                      (:h2 :class "purple" title))
                  (:h4 :class "purple" (format-timestamp nil date :site))
                  (:div :class "tags"
                        (:span "Tags: ")
                        (dolist (name (category-names category))
                          (:span name " ")))
                  (:div :id "user-content"
                        (funcall content entry)))))))

(defmethod html-body ((index index))
  (spinneret:with-html
    (:div :id "entries-list"
          (dolist (entry (entries (blog index)))
            (:div :class "index-entry"
                  (:a :href (process-uri entry :encode)
                      (format nil "Title: ~A Date: ~A"
                              (title entry) (format-timestamp nil (date entry) :site))))))))

(defmethod html-body ((blog blog))
  (spinneret:with-html
    (:h1 :class "purple" (title blog))
    (:div :id "all-entries"
          (dolist (blog (sort (entries blog) #'> :key #'order))
            (:div :class "entry"
                  :id (id blog)
                  (html-body blog))))))

(defmethod html-body ((c display-condition))
  (with-accessors ((http-code http-code)
                   (message message))
      (c c)
    (spinneret:with-html
      (:h1 :class "purple" http-code)
      (:h3 :class "purple" message))))

(defmethod html-footer :around (page)
  (spinneret:with-html
    (:div :id "footer"
          (call-next-method))))

(defmethod html-footer (page)
  nil)

(defgeneric generate-rss (stream object)
  (:documentation "Converts object into RSS using xml-emitter."))

(defmethod generate-rss :around (stream object)  
  (call-next-method))

(defmethod generate-rss (stream (category category))
  (let ((names (category-names category)))
    (format nil "~{~:(~A~)~^ ~}"
            (mapcar (lambda (name)
                      (if (find #\Space name)
                          (format nil "'~A'" name)
                          name))
                    names))))

(defmethod generate-rss (stream (blog blog))
  (with-accessors ((domain domain)
                   (url url)
                   (title title)
                   (description description)
                   (entries entries)
                   (language language))
      blog
    (xml-emitter:rss-channel-header
     title (format nil "~A~A" domain url)
     :description description :language language)
    (mapc (lambda (entry)
            (generate-rss stream entry))
          entries)))

(defmethod generate-rss (stream (entry entry))
  (with-accessors ((blog blog)
                   (title title)
                   (date date)
                   (content content)
                   (category category)
                   (description description))
      entry
    (xml-emitter:rss-item
     title :link (format nil "~A~A" (domain blog)
                         (process-uri entry :encode))
     :category (generate-rss stream category)
     :pubdate (format-timestamp stream date :rss)
     :author (author blog)
     :description
     (let* ((my-stream (make-string-output-stream))
            (*standard-output* my-stream))
       (typecase description
         (string description)
         (function (funcall description entry))
         (null (funcall content entry)))
       (get-output-stream-string my-stream)))))


