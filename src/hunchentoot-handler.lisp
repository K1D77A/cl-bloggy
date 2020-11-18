(in-package :cl-bloggy)

(defclass bloggy-acceptor (hunchentoot:acceptor)
  ((routes
    :initform ()
    :accessor routes)))

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
      (setf routes
            (delete-if (lambda (route2)
                         (destructuring-bind (method2 url2 handler2)
                             route2
                           (declare (ignore handler2))
                           (and (equal method method2)
                                (string-equal url url2))));checks for capitals
                       routes))
      (push route routes))))

(defmethod remove-route (route (acceptor bloggy-acceptor))
  (check-type route route)
  (with-accessors ((routes routes))
      acceptor
    (destructuring-bind (method url handler)
        route
      (declare (ignore handler))
      (setf routes
            (delete-if (lambda (route2)
                         (destructuring-bind (method2 url2 handler2)
                             route2
                           (declare (ignore handler2))
                           (and (equal method method2)
                                (string-equal url url2))));checks for capitals
                       routes)))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor bloggy-acceptor) request)
  (let* ((method (hunchentoot:request-method* request))
         (uri (hunchentoot:request-uri* request)));not grabbing the params
    (dolist (route (routes acceptor) (call-next-method))
      (destructuring-bind (method2 url handler)
          route
        (when (and (equal method2 method)
                   (string-equal url uri))
          (return (funcall handler)))))))


(defun doof ()
  "doofff")
