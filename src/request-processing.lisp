(in-package #:cl-bloggy)

(defgeneric handle-unknown-uri (acceptor request uri method)
  (:documentation "Attempts to handle a request to an unknown URI."))

(defmethod handle-unknown-uri (acceptor request uri method)
  (with-accessors ((blog blog))
      acceptor
    (multiple-value-bind (clean-uri uri-list)
        (process-uri uri :decode)
                                        ;  (print uri-list)
      (let* ((clean-uri-list
               (nreverse (set-difference uri-list
                                         (str:split "/" (url blog) :omit-nulls t)
                                         :test #'string-equal)))
             (special
               (make-instance 'special-request 
                              :acceptor acceptor :request request
                              :uri clean-uri :r-method method
                              :split-uri clean-uri-list)))
        (process-special-request
         (change-class special (determine-request-type special)))))))

(defgeneric determine-request-type (special)
  (:documentation "Given a string determines the special request type"))

(defmethod determine-request-type ((special special-request))
  (let ((end (first (last (split-uri special)))))
    (if (string-equal end "rss.xml");we now know its an rss request
        (%rss-request-type special)
        (%non-rss-request-type special))))

(defun %rss-request-type (special)
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor)
                   (category category)) 
      special 
    (let* ((remainder (nbutlast split-uri))
           (cat (find-category remainder (blog acceptor) nil)))
      (if cat
          (progn (setf category cat)
                 'rss-category-request)
          'rss-request))))

(defun %non-rss-request-type (special)
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor)
                   (category category)) 
      special
                                        ;  (print split-uri)
    (let ((cat (find-category split-uri (blog acceptor) nil)))
      (if cat
          (progn (setf category cat)
                 'category-request)
          'special-request))))
        

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
                   (acceptor acceptor)
                   (category category))
      request
    (print split-uri)
    (print category)
    (let* ((blog (blog acceptor))
           (entries (entries-in-category category blog)))
      (if entries
          (with-accessors ((categories categories))
              blog
            (to-html (make-instance (class-of (blog acceptor))
                                    :categories categories
                                    :title (name category)
                                    :entries entries)))
          "unknown"))))

(defmethod process-special-request ((request rss-category-request))
  (with-accessors ((acceptor acceptor)
                   (category category))
      request
    (print category)
    (let* ((blog (blog acceptor))
           (entries (entries-in-category category blog))
           (stream (make-string-output-stream)))
      (setf (tbnl:content-type*) "application/xml")
      (setf (tbnl:return-code*) 200)
      (print entries)
      (xml-emitter:with-rss2 (stream)
        (with-accessors ((categories categories))
            blog
          (generate-rss stream
                        (make-instance (class-of blog)
                                       :categories categories
                                       :title (name category)
                                       :entries entries))))
      (get-output-stream-string stream))))
