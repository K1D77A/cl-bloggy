(in-package #:cl-bloggy)

(defgeneric handle-unknown-uri (acceptor request uri method)
  (:documentation "Attempts to handle a request to an unknown URI."))

(defmethod handle-unknown-uri (acceptor request uri method)
  (let* ((split (str:split "/" uri :omit-nulls t))
         (last (do-urlencode:urldecode (first (last split)))))
    (process-special-request 
     (make-instance (determine-request-type acceptor last)
                    :acceptor acceptor :request request :uri uri :r-method method
                    :split-uri split))))

(defgeneric determine-request-type (acceptor string)
  (:documentation "Given a string determines the special request type"))

(defmethod determine-request-type (acceptor (string string))
  (cond ((string-equal string "rss.xml")
         'rss-request)
        ((string-equal string "atom")
         'atom-request)
        ((find-category string (blog acceptor) nil)
         'category-request)
        (t 'special-request)))

(defgeneric process-special-request (special-request)
  (:documentation "Properly handles the special request type and serves the content 
expected."))

(defmethod process-special-request (request)
  "unknown")

(defmethod process-special-request ((request rss-request))
  (with-accessors ((blog blog))
      (acceptor request)
    (let ((stream (make-string-output-stream)))
      (setf (tbnl:content-type*) "application/xml")
      (setf (tbnl:return-code*) 200)
      (xml-emitter:with-rss2 (stream)
        (generate-rss stream blog))
      (get-output-stream-string stream))))

(defmethod process-special-request ((request category-request))
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor))
      request
    (let* ((blog (blog acceptor))
           (category (find-category (first (last split-uri)) blog nil))
           (entries (entries-in-category category blog)))
      (if entries
          (with-accessors ((categories categories))
              blog
            (to-html (make-instance 'blog :categories categories
                                          :title (name category)
                                          :entries entries)))
          "unknown"))))
