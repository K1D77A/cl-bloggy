(in-package :cl-bloggy)

(defclass my-index (index)
  ())

(defclass my-entry (entry)
  ())

(defclass my-blog (blog)
  ((title :initform "K1D77A's Test Blog")))

(defparameter *server* (make-instance 'bloggy-acceptor                                      
                                      :document-root "./"
                                      :port 4203 :name 'main))

(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))

(hunchentoot:define-easy-handler (root :uri "/dd")
    ()
  "boof")

(add-blog *server* 'my-blog)
(add-index *server* 'my-index)

(easy-blog-entry (my-entry 1 ("general" "test") "A story to tell" "23rd of June" *server*)
  (:div :class "elp"
        (:p "once upon a time in a land far away")))

(easy-blog-entry (my-entry 2 ("general" "gaming" "FFXIV") "Fun on FFXIV" "24th of June" *server* )
  (:div :class "elp"
        (:p "Another player started complaining at me because I called myself retarded. To explain I repeatedly hit the wrong button casting a ill-applicable spell causing the tank to die and as a consequence we all died.. anyway another player became quite upset after I called myself retarded... eventually they quit because we weren't sensitive enough.")))

(easy-blog-entry (my-entry 3 ("general") "I just don't understand." "26th of June" *server*)
  (:div :class "elp"
        (dolist (item (lorem-ipsum:paragraphs 5))
          (:p item))))

(easy-blog-entry (my-entry 4 ("general" "programming" "common lisp" "generics")
                  "Creating elegant and extensible programs using defgeneric"
                  "30th of June"
                  *server*)
  (:div :class "elp"
        (:h4 "defgeneric more like defmagic!")
        (:p "The power and flexibility of Common Lisp's defgeneric form cannot be overstated.")))



