(in-package #:cl-bloggy)

(defparameter *blog-entry-css-rules*
  '((body
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
    ("#body-h3")
    (footer))
  "The default LASS used by each the class 'blog-entry'")

(defparameter *blog-css-rules*
  '((body
     :text-align center
     :margin-left 25%
     :margin-right 25%)
    (:media "(orientation: portrait)"
     (body
      :text-align center
      :margin-right 3%
      :margin-left 3%)))
  "The default LASS used to render the main blog page")

(defparameter *index-css-rules*
  '((body
     :text-align center
     :margin-left 25%
     :margin-right 25%)
    (:media "(orientation: portrait)"
     (body
      :text-align center
      :margin-right 3%
      :margin-left 3%)))
  "The default LASS used to render the main blog index.")

(defgeneric to-css (e)
  (:documentation "to-css simply appends a the CSS as an inline style tag. See
(css-rules e) for the list of CSS, the CSS is infact a list of LASS rules"))

(defmethod to-css ((entry blog-entry))
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (css-rules entry)))))

(defmethod to-css ((blog blog))
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (css-rules blog)))))
