(in-package #:cl-bloggy)

(defgeneric process-uri (e key &rest args)
  (:documentation "Generates the correct url for E. Key is the method to use."))

(defmethod process-uri ((entry entry) (key (eql :encode)) &rest args)
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
  (declare (ignore args))
  (let* ((ac (acceptor im))
         (blog (blog ac))
         (content (content blog)))
    (format nil "~A/generic/~A" (url content) (name im))))

(defmethod process-uri ((im image-content) (key (eql :upload)) &rest args)
  (declare (ignore args))
  (let* ((ac (acceptor im))
         (blog (blog ac))
         (content (content blog)))
    (format nil "~A/images/~A" (url content) (name im))))

(defmethod process-uri ((blog blog) (key (eql :category-url)) &rest args)
  (format nil "~A/~{~A~^/~}" (url blog) (category-names (first args))))
