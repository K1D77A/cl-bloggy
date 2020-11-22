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
    (footer)))


;;blog-entry h3 :font-size large
;;my-blog-entry h3 :font-size small

(defmethod to-css ((entry blog-entry))
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write *blog-entry-css-rules*))))

