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
     :color "var(--fsecondary)")
    (a
     :color "var(--fsecondary)")
    ("#home-link"
     :padding-right 1vw)
    (p
     :font-size "var(--fsize)")
    (pre
     :border-color "var(--fsecondary)")
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
    (".title-and-icons"
     :display flex
     :flex-display row)
    (".rss-link"
     :margin-left 1vw)
    (".rss-icon"
     :height 20px)
    (".entry:hover"
     :border-color "var(--bordercolour)"
     :border-left-style solid
     :border-left-width 3px
     :padding-left 7px)))

(defmethod page-css append ((page index))
  `((".index-entry"
     :padding-left 10px
     :margin-bottom 10px)
    (".index-title"
     :margin-bottom 3px)
    (".index-date"
     :margin-left 5px)
    (".index-tags")
    (".index-entry:hover"
     :border-color "var(--bordercolour)"
     :border-left-style solid
     :border-left-width 3px
     :padding-left 7px)))

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
