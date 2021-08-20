(in-package #:cl-bloggy)

(defgeneric generate-rss (stream object)
  (:documentation "Converts object into RSS using xml-emitter."))

(defmethod generate-rss (stream (category category))
  "Converts the category into a string for use within the generate-rss, however
I basically have just guessed that you should wrap categories that contain spaces with 
single ' marks. This may change."
  (let ((names (category-names category)))
    (format nil "~{~:(~A~)~^ ~}"
            (mapcar (lambda (name)
                      (if (find #\Space name)
                          (format nil "'~A'" name)
                          name))
                    names))))

(defmethod generate-rss (stream (blog blog))
  "Just applies the rss-channel-header and then executes generate-rss on all of the entries."
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
  "Generates the rss for an ENTRY object, fills in the default values and in the case that
description is non nil then evaluates that function, if it is nil then evaluates 
the content function instead."
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
