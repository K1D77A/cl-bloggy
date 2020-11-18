(in-package :cl-bloggy)

;; (defparameter *routes-list*
;;   '((:GET "/" 'boof)
;;     (:GET "/boof" (lambda () "boof2"))))

;; (defun update-routeslist ()
;;   (setf simple-routes:*routeslist*
;;         (append '(list)
;;                 (mapcar (lambda (route)
;;                           (simple-routes::routespec-compile (first route)
;;                                                             (second route)
;;                                                             (third route)))
;;                         *routes-list*))))

;; (defun new-route (method url fun)
;;   (push (list method url fun) *routes-list*)
;;   (update-routeslist))

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :document-root "./"
                                      :port 4203 :name 'main))
(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

;; (defmacro new-entry ((title category) ))

(hunchentoot:define-easy-handler (root-document :uri "/")
    ()
  (to-html (make-blog-entry "I am a title" "I am the category")))

