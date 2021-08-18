(in-package #:cl-bloggy)

(defgeneric handle-unknown-uri (acceptor request uri method)
  (:documentation "Attempts to handle a request to an unknown URI."))

(defmethod handle-unknown-uri (acceptor request uri method)
  (let* ((split (str:split "/" uri :omit-nulls t))
         (cats (set-difference split (str:split "/" (url (blog acceptor))
                                                :omit-nulls t)
                               :test #'string-equal))
         (last (do-urlencode:urldecode (first (last split))))
         (special
           (make-instance 'special-request 
                          :acceptor acceptor :request request :uri uri :r-method method
                          :split-uri split :category (find-category (first (last cats))
                                                                    (blog acceptor)
                                                                    nil))))
    (process-special-request (change-class special (determine-request-type special last)))))

(defgeneric determine-request-type (special end)
  (:documentation "Given a string determines the special request type"))

(defmethod determine-request-type ((special special-request) end)
  (cond ((and (string-equal end "rss.xml")
              (category special))
         'rss-category-request)
        ((string-equal end "rss.xml")
         'rss-request)
        ((category special)
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

(defmethod process-special-request ((request rss-category-request))
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor)
                   (category category))
      request
    (print category)
    (let* ((blog (blog acceptor))
           (entries (entries-in-category category blog)))
      (if entries
          (with-accessors ((categories categories))
              blog
            (to-html (make-instance 'blog :categories categories
                                          :title (name category)
                                          :entries entries)))
          "unknown"))))
