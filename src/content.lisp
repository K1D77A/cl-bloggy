(in-package #:cl-bloggy)

(defclass uploaded-content ()
  ((path
    :accessor path
    :initarg :path)
   (mime
    :accessor mime
    :initarg :mime)
   (acceptor
    :accessor acceptor
    :initarg :acceptor)
   (data
    :accessor data
    :initarg :data
    :type function)
   (name
    :accessor name
    :initarg :name)
   (url
    :accessor url
    :initarg :url)))

(defclass image-content (uploaded-content)
  ())

(defgeneric add-content (e content key)
  (:documentation "Stores content under key in E."))

(defmethod add-content ((blog blog) (content uploaded-content) key)
  "adds CONTENT to BLOG under KEY."
  (unless (and (slot-boundp blog 'content)
               (typep (slot-value blog 'content) 'content))
    (error 'missing-required-feature
           :feature "Content"
           :instructions #.(format nil "To remedy you need to use the function ~
                         (add-content ..) to initialize the content object in your blog.")))
  (setf (getf (content (content blog)) key) content))

(defmethod add-content ((acceptor bloggy-acceptor) (content uploaded-content) key)
  (add-content (blog acceptor) content key))

(defgeneric find-content (e key)
  (:documentation "Finds content under KEY from E."))

(defmethod find-content :around (e key)
  (let ((content (call-next-method)))
    (or content 
        (error 'unknown-content
               :content key
               :message #.(format nil "Missing. Please add with add-content.")))))

(defmethod find-content ((content content) key)
  (with-accessors ((content content))
      content
    (getf content key)))

(defmethod find-content ((blog blog) key)
  (find-content (content blog) key))

(defmethod find-content ((acceptor bloggy-acceptor) key)
  (find-content (blog acceptor) key))

(defmethod find-content ((entry entry) key)
  (find-content (blog entry) key))

(defmethod find-content ((index index) key)
  (find-content (blog index) key))


(defun easy-image (acceptor image-path key)
  (let* ((image (alexandria:read-file-into-byte-vector image-path))
         (mime (tbnl:mime-type image-path))
         (uploader (make-instance 'image-content
                                  :path image-path
                                  :name
                                  (format nil "~A.~A"
                                          (pathname-name image-path)
                                          (pathname-type image-path))
                                  :mime mime
                                  :data (lambda (uploader)
                                          (declare (ignore uploader))
                                          image)
                                  :acceptor acceptor)))
    (setf (url uploader) (process-uri uploader :upload))
    (add-content acceptor uploader key)
    (add-route
     (make-route :GET
                 (url uploader)
                 (lambda ()
                   (with-accessors ((mime mime)
                                    (data data))
                       uploader
                     (let ((stream (tbnl:send-headers))
                           (resolved-data (funcall data uploader)))
                       (setf (hunchentoot:content-type*) mime)
                       (setf (hunchentoot:content-length*) (length resolved-data))
                       (write-sequence resolved-data stream)))))
     acceptor)))
