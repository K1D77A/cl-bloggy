(in-package #:cl-bloggy)

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
     (funcall title blog) (format nil "~A~A" domain url)
     :description
     (funcall description blog) :language language)
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
     (funcall title entry)
     :link (format nil "~A~A" (domain blog)
                   (process-uri entry :encode))
     :category (generate-rss stream category)
     :pubdate (format-timestamp stream date :rss)
     :author (funcall (author blog) blog)
     :description
     (let* ((my-stream (make-string-output-stream))
            (*standard-output* my-stream))
       (if description
           (funcall description entry)
           (funcall content entry))
       (get-output-stream-string my-stream)))))
