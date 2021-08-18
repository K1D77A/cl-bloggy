(in-package #:cl-bloggy)


(defgeneric page-css (page)
  (:method-combination append :most-specific-last)
  (:documentation "Generates CSS for page"))

(defmethod page-css :around (page)
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (call-next-method)))))

(defmethod page-css append (page)
  `((html
     :background-color "#212529"
     :padding 5vw
     :padding-right 10vw
     :margin-left 0
     :margin-right 0
     :max-height 100%
     :height 100%)
    ("html p"
     :font-size 15pt)))

(defmethod page-css append ((page entry))
  `((body
     :text-align center
     :margin-left 25%
     :margin-right 25%)
    (:media "(orientation: portrait)"
            (body
             :text-align center
             :margin-left 3%
             :margin-right 3%))
    ("#home-link"
     :font-weight bold)
    ("#content"
     ;;general rules for all the content
     )
    ("#body-title"
     :font-size 4vw)
    ("#body-h2")
    ("#body-h3")
    ("#user-content")
    ("#body-h3")))

(defmethod page-css append ((page blog))
  `((.purple
     :color "#9B4DCA")
    (.wrapper
     :color "#868e96")
    ("media (orientation: portrait)"
     (body
      :text-align center
      :margin-right 3%
      :margin-left 3%))))

(defmethod page-css append ((page index))
  `((body
     :text-align left
     :margin-left 25%
     :margin-right 25%)
    (":media (orientation: portrait)"
     (body
      :text-align center
      :margin-right 3%
      :margin-left 3%))))

(defmethod page-css append ((c display-condition))
  nil)

(defgeneric scoped-css (page)
  (:documentation "Generates scoped css for an entry."))

(defmethod scoped-css :around (page)
  (spinneret:with-html
    (:style :type "text-css"
      (apply #'lass:compile-and-write (call-next-method)))))

(defmethod scoped-css (page)
  nil)

(defmethod scoped-css ((page entry))
  `((body
     :background-color "green")))
