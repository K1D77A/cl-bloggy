(in-package :cl-bloggy)

(defclass bloggy-acceptor (hunchentoot:easy-acceptor)
  ((routes
    :initform (make-hash-table :test #'equal)
    :accessor routes)
   (blog
    :initarg :blog
    :accessor blog)))

(deftype route () `(satisfies routep))

(defun routep (x)
  (and (= (length x) 3)
       (destructuring-bind (method route handler)
           x
         (and (keywordp method)
              (stringp route)
              (or (functionp handler)
                  (symbolp handler))))))

(defun make-route (method url handler)
  (check-type method keyword)
  (check-type url string)
  (check-type handler (or function symbol))
  (list method url handler))

(defmethod add-route (route (acceptor bloggy-acceptor))
  "Adds a route to your acceptor, the acceptor is what you used to start 
hunchentoot"
  (check-type route route)
  (with-accessors ((routes routes))
      acceptor
    (destructuring-bind (method url handler)
        route
      (declare (ignore handler))
      (setf (gethash url routes)
            route))))

(defmethod remove-route (route (acceptor bloggy-acceptor))
  (check-type route route)
  (with-accessors ((routes routes))
      acceptor
    (destructuring-bind (method url handler)
        route
      (declare (ignore handler))
      (remhash url routes))))

(defclass image-upload ()
  ((image-path
    :accessor image-path
    :initarg :image-path)
   (image-name
    :accessor image-name
    :initarg :image-name)
   (image
    :accessor image
    :initarg :image)
   (mime
    :accessor mime
    :initarg :mime)
   (acceptor
    :accessor acceptor
    :initarg :acceptor)))

(defun easy-image (acceptor image-path)
  (let* ((image (alexandria:read-file-into-byte-vector image-path))
         (mime (tbnl:mime-type image-path))
         (uploader (make-instance 'image-upload
                                  :image-path image-path
                                  :image-name
                                  (format nil "~A.~A"
                                          (pathname-name image-path)
                                          (pathname-type image-path ))
                                  :mime mime
                                  :image image
                                  :acceptor acceptor)))
    (add-route
     (make-route :GET
                 (process-uri uploader :upload)
                 (lambda ()
                   (with-accessors ((mime mime)
                                    (image image))
                       uploader
                     (let ((stream (tbnl:send-headers)))
                       (setf (hunchentoot:content-type*) mime)
                       (setf (hunchentoot:content-length*) (length image))
                       (write-sequence image stream)))))
     acceptor)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor bloggy-acceptor) request)
  (let* ((method (hunchentoot:request-method* request))
         (uri (hunchentoot:request-uri* request)))
    (print (process-uri uri :decode))
    (let ((route (gethash uri (routes acceptor))))
      (if route 
          (destructuring-bind (method2 url handler)
              route
            (declare (ignore url))
            (if (equal method2 method)
                (handler-case 
                    (funcall handler)
                  (error ()
                    (setf (tbnl:return-code*) 500)
                    (display-condition nil 
                                       (make-condition 'request-condition
                                                       :blog (blog acceptor)
                                                       :message "Unknown error"
                                                       :http-code 500)
                                       :html)))
                (call-next-method)))
          (handle-unknown-uri acceptor request uri method)))))






