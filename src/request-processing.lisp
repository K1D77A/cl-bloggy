(in-package #:cl-bloggy)

(defgeneric handle-unknown-uri (acceptor request uri method)
  (:documentation "Attempts to handle a request to an unknown URI.
Tries to determine the type of request, which are subclasses of 'special-request. 
Decodes the uri, checks it is valid and then creates the correct subclass. 
All conditions that are a subclass of 'request-condition
 are caught by :around and the conditions 
are passed to display-condition with the :html key. 
If a condition is a subclass of 'error a new condition is created and that is passed 
to display-condition with the :html key. 
If all goes well process-special-request is evaluated with the generated request."))

(defmethod handle-unknown-uri :around (acceptor request uri method)
  (handler-case
      (call-next-method)
    (request-condition (c)
      (setf (tbnl:return-code*) (http-code c))
      (display-condition nil c :html))
    (error ()
      (setf (tbnl:return-code*) 500)
      (display-condition nil 
                         (make-condition 'request-condition
                                         :blog (blog acceptor)
                                         :message "Unknown error"
                                         :http-code 500)
                         :html))))

(defun %validate-url (uri-list blog)
  "Attemps to validate the uri which has been split in URI-LIST by comparing it with 
the url listed for (url BLOG). If it is not then signals 'malformed-url."
  (let* ((split-url (str:split "/" (url blog) :omit-nulls t))
         (uri-len (length split-url)))
    (unless (and (<= uri-len (length uri-list))
                 (tree-equal (subseq uri-list 0 uri-len)
                             split-url :test #'string-equal))
      (error 'malformed-url :message #.(format nil "Malformed URL.")
             :blog blog))))

(defmethod handle-unknown-uri (acceptor request uri method)
  (with-accessors ((blog blog))
      acceptor
    (multiple-value-bind (clean-uri uri-list)
        (process-uri uri :decode)
      (%validate-url uri-list blog)
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
  (:documentation "Given an instance of the special-request class (not subclasses)
attempts to determine which subclass it should change-class that instance into. 
Can possibly signal 'rss%bad-categories, 'missing-categories, 'missing-content"))

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
          (if remainder
              (error 'rss%bad-categories
                     :category remainder
                     :blog (blog acceptor)
                     :message #.(format nil "Request made for an rss feed ~
                                        for categories that do not exist."))
              'rss-request)))))

(defun %non-rss-request-type (special)
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor)
                   (category category)
                   (uri uri))
      special
    (let ((cat (find-category split-uri (blog acceptor) nil)))
      (if cat
          (progn (setf category cat)
                 'category-request)
          (if (char= (aref uri (1- (length uri))) #\/)
              (error 'missing-categories 
                     :category split-uri
                     :blog (blog acceptor)
                     :message #.(format nil "Request made to sort by categories ~
                                             but the categories do not exist."))
              (error 'missing-content :blog (blog acceptor)
                                      :message "No content at URL"))))))
        

(defgeneric process-special-request (special-request)
  (:documentation "Properly handles the subclass of special-request and serves the content 
expected."))

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
  "Generate the HTML for content within the categories discovered in the request url.
Can signal 'missing-categories and 'missing-content."
  (with-accessors ((split-uri split-uri)
                   (acceptor acceptor)
                   (category category)
                   (uri uri))
      request
    (let* ((blog (blog acceptor))
           (entries (entries-in-category category blog)))
      (if entries
          (with-accessors ((categories categories))
              blog
            (to-html (make-instance (class-of (blog acceptor))
                                    :categories categories
                                    :content (content blog)
                                    :acceptor acceptor
                                    :index (index blog)
                                    :title (lambda (blog)
                                             (declare (ignore blog))
                                             (format nil "Category: ~:(~A~)"
                                                     (name category)))
                                    :url (process-uri blog :category-url category)
                                    :entries entries)))
          (if (char= (aref uri (1- (length uri))) #\/)
              (error 'missing-categories :category split-uri
                                         :blog (blog acceptor)
                                         :message "No such categories")
              (error 'missing-content :blog (blog acceptor)
                                      :message "No content at URL"))))))

(defmethod process-special-request ((request rss-category-request))
  "Attempts to generate an RSS feed for the category in request."
  (with-accessors ((acceptor acceptor)
                   (category category))
      request
    (let* ((blog (blog acceptor))
           (entries (entries-in-category category blog))
           (stream (make-string-output-stream)))
      (setf (tbnl:content-type*) "application/xml")
      (setf (tbnl:return-code*) 200)
      (xml-emitter:with-rss2 (stream)
        (with-accessors ((categories categories))
            blog
          (generate-rss stream
                        (make-instance (class-of blog)
                                       :categories categories
                                       :acceptor acceptor
                                       :title
                                       (lambda (blog)
                                         (declare (ignore blog))
                                         (format nil "Category: ~:(~A~)" (name category)))
                                       :index (index blog)
                                       :content (content blog)
                                       :url (process-uri blog :category-url category)
                                       :entries entries))))
      (get-output-stream-string stream))))
