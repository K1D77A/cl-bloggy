(in-package #:cl-bloggy)




(defgeneric page-css (page)
  (:method-combination append :most-specific-last)
  (:documentation "Generates CSS for page"))

(defmethod page-css :around (page)
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (call-next-method)))))

(defmethod page-css append (page)
  `((":root"
     :--colourone "#d5d7d6";pinky
     :--colourtwo "#00314f";dark blue
     :--colourthree "#736a7b";purply grey
     :--colourfour "#5e8930";green
     :--background "var(--colourone)"
     :--fprimary "var(--colourtwo)"
     :--fsecondary "var(--colourfour)"
     :--fnormal "var(--colourtwo)"
     :--fsize "15pt"
     :--bordercolour "var(--colourtwo)")
    (".title"
     :color "var(--fprimary)")
    (".date"
     :color "var(--fsecondary)")
    (".description"
     :color "var(--fsecondary)")
    (".tags"
     :colour "var(--fsecondary)")
    (".http-code"
     :color "var(--fprimary)")
    (".message"
     :colour "var(--fsecondary)")
    (p
     :font-size "var(--fsize)")
    (html
     :background-color "var(--background)"
     :padding 5vw
     :padding-right 10vw
     :color "var(--fnormal)"     
     :margin-left 0
     :margin-right 0
     :max-height 100%
     :height 100%)))

(defmethod page-css append ((page entry))
  nil)

(defmethod page-css append ((page blog))
  `((".title-box"
     :margin-bottom 5vw)
    (".entry"
     :padding-left 10px)
    (".entry:hover"
     :border-color "var(--bordercolour)"
     :border-left-style solid
     :border-left-width 3px
     :padding-left 7px)))

(defmethod page-css append ((page index))
  nil)

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
