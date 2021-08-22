(in-package :cl-bloggy)

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

(defclass my-index (index my-blog)
  ())

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

(easy-blog-entry (*server* my-entry  ("general" "test") "A story to tell" :test1
                  (:month "June" :day 23 :hour 14 :minute 32))
  (:div :class "elp"
        (:p "once upon a time in a land far away")))
(entry-test1)

(easy-blog-entry (*server* my-entry  ("general" "gaming" "FFXIV") "Fun on FFXIV"
                  :test2
                  (:month "June" :day 24))
  (:div :class "elp"
        (:p "oooooooooooooooooooooooooooooooooof.")))
(entry-test2)

(easy-blog-entry (*server* my-entry  ("general") "I just don't understand." :test3
                  (:month "June" :day 26 :minute 1))
  (:div :class "elp"
        (dolist (item (lorem-ipsum:paragraphs 5))
          (:p item))))
(entry-test3)

(easy-blog-entry (*server* my-entry  ("general" "programming" "common lisp" "generics")
                  "Creating elegant and extensible programs using defgeneric"
                  :test4
                  (:month "June" :day 30)
                  :subtitle "My love for Generic programming!")
  (:div :class "elp"
        (:h4 "defgeneric more like defmagic!")
        (:p "The power and flexibility of Common Lisp's defgeneric form cannot be overstated.")))
(entry-test4)

(easy-blog-entry (*server* my-entry  ("general" "programming" "common lisp" "bloggy")
                  "Demonstrating images and code in CL-BLOGGY"
                  :test5
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
(entry-test5)

(defun %5-code-block ()
  "(:pre 
   (:code 
      (:img :src (url (find-content entry :rss-png))
            :width \"50\")))")


