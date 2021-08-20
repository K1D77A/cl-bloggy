(in-package :cl-bloggy)

(defclass my-index (index)
  ())

(defclass my-entry (entry)
  ())

(defclass my-content (content)
  ())

(defclass my-blog (blog)
  ((title :initform
          (lambda (b) (declare (ignore b)) "K1D77A's Test Blog"))
   (domain :initform "https://k1d77a.com")
   (description :initform
                (lambda (b) (declare (ignore b)) "My Test blog for random stuff."))
   (author :initform
           (lambda (b) (declare (ignore b)) "K1D77A"))))

(defparameter *server* (make-instance 'bloggy-acceptor                                      
                                      :document-root "./blog/"
                                      :port 4203 :name 'main))

(defun start ()
  (hunchentoot:start *server*))
(defun stop ()
  (hunchentoot:stop *server*))


(new-blog *server* 'my-blog)
(new-index *server* 'my-index)
(new-content *server* 'my-content)

(easy-image *server* "blog-content/images/rss.png" :rss-png)

(easy-blog-entry (*server* my-entry  ("general" "test") "A story to tell"
                  (:month "June" :day 23 :hour 14 :minute 32))
  (:div :class "elp"
        (:p "once upon a time in a land far away")))

(easy-blog-entry (*server* my-entry  ("general" "gaming" "FFXIV") "Fun on FFXIV"
                  (:month "June" :day 24))
  (:div :class "elp"
        (:p "Another player started complaining at me because I called myself retarded. To explain I repeatedly hit the wrong button casting a ill-applicable spell causing the tank to die and as a consequence we all died.. anyway another player became quite upset after I called myself retarded... eventually they quit because we weren't sensitive enough.")))

(easy-blog-entry (*server* my-entry  ("general") "I just don't understand."
                  (:month "June" :day 26))
  (:div :class "elp"
        (dolist (item (lorem-ipsum:paragraphs 5))
          (:p item))))

(easy-blog-entry (*server* my-entry  ("general" "programming" "common lisp" "generics")
                  "Creating elegant and extensible programs using defgeneric"
                  (:month "June" :day 30)
                  :subtitle "My love for Generic programming!")
  (:div :class "elp"
        (:h4 "defgeneric more like defmagic!")
        (:p "The power and flexibility of Common Lisp's defgeneric form cannot be overstated.")))

(easy-blog-entry (*server* my-entry  ("general" "programming" "common lisp" "bloggy")
                  "Demonstrating images and code in CL-BLOGGY"
                  (:month "July" :day 1)
                  :subtitle "How-to insert images and code into CL-BLOGGY posts."
                  :description "A quick demonstration of images and code.")
  (:p "To insert an image into cl-bloggy simply use an (:img ) like this:")
  (:img :src (url (find-content entry :rss-png))
        :width "50")
  (:br)
  (:pre
   (:code 
    "(:img :src (url (find-content entry :rss-png))
                :width \"50\")"))
  (:p "How did I enter that code?? Like this:")
  (:br)
  (:pre
   (:code (%5-code-block)))
  (:p "Unfortunately you have to drop the second line to the same column as the first for it to format correctly."))

(defun %5-code-block ()
  "(:pre 
   (:code 
      (:img :src (url (find-content entry :rss-png))
            :width \"50\")))")


