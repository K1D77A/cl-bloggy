(in-package :cl-bloggy)


#||
This file contains the code for a custom hunchentoot acceptor, a subclass of easy
acceptor. It doesn't do anything special other than provide a way to add routes outside of
tbnl:define-easy-handler. The custom route system uses a hash table within the acceptor.
||#

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

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor bloggy-acceptor) request)
  "Check the URI in the request against the routes stored within (routes acceptor), 
if the route is found then serves the content, otherwise executes 
handle-unknown-uri to try and process the request."
  (let* ((method (hunchentoot:request-method* request))
         (uri (hunchentoot:request-uri* request)))
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






