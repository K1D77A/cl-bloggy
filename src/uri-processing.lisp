(in-package #:cl-bloggy)

(defgeneric process-uri (e key &rest args)
  (:documentation "Generates the correct url for E. Key is the method to use."))

(defmethod process-uri ((entry entry) (key (eql :encode)) &rest args)
  "Default URI encoding for entries, this is so that entries once created can be 
found at the correct uri."
  (declare (ignore args))
  (with-accessors ((cat category)
                   (title title))
      entry
    (let ((names (category-names cat)))
      (reduce #'str:concat
              (append
               (list (url (blog entry)) "/")
               (mapcar (lambda (name)
                         (str:concat (do-urlencode:urlencode name) "/"))
                       names)
               (list (do-urlencode:urlencode (funcall title entry))))))))

(defmethod process-uri ((uri string) (key (eql :decode)) &rest args)
  "Decodes the URI string. Used when requests are made to tbnl. Makes use of 
*max-category-depth* in an attempt to stop DoS attacks which use very large URIs full of
fake categories."
  (declare (ignore args))
  (let ((split (str:split "/" uri :omit-nulls t :limit (* 2 *max-category-depth*))))
    (values 
     (format nil "~{~A~^/~}"
             (let ((decoded 
                     (mapcar (lambda (e)
                               (do-urlencode:urldecode e))
                             split)))
               (setf split decoded)
               decoded))
     split)))

(defmethod process-uri ((im content) (key (eql :upload)) &rest args)
  "Giving IM which is an instance of content, generates a url at ../generic/.."
  (declare (ignore args))
  (let* ((ac (acceptor im))
         (blog (blog ac))
         (content (content blog)))
    (format nil "~A/generic/~A" (url content) (name im))))

(defmethod process-uri ((im image-content) (key (eql :upload)) &rest args)
  "Generates the URL for im, this is different from the normal content in that the routes 
are at /images/ rather than /generic/"
  (declare (ignore args))
  (let* ((ac (acceptor im))
         (blog (blog ac))
         (content (content blog)))
    (format nil "~A/images/~A" (url content) (name im))))

(defmethod process-uri ((blog blog) (key (eql :category-url)) &rest args)
  "When (first args) is a category, generates a url for that category."
  (format nil "~A/~{~A~^/~}" (url blog)
          (mapcar #'do-urlencode:urlencode
                  (category-names (first args)))))
