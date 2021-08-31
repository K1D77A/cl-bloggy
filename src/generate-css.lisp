(in-package #:cl-bloggy)

(defparameter *colourone* "#d5d7d6");pinky
(defparameter *colourtwo* "#00314f");dark blue
(defparameter *colourthree* "#736a7b");purply grey
(defparameter *colourfour* "#5e8930");green

(defgeneric page-css (page)
  (:method-combination append :most-specific-last)
  (:documentation "Generates CSS for page. Uses append method so that each subclass of 
page has its CSS appended after. Works with most-specific-last meaning that the 
methods are applied like so page -> entry -> my-entry, assuming you have subclassed 
something and then created your own version of page-css for it. This is ideal for 
CSS as CSS cascades. If you wanted to override the default colours you could create a 
new :root and override the values of :--colourone etc."))

(defmethod page-css :around (page)
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (call-next-method)))))

(defmethod page-css append (page)
  "Provide the default CSS for all objects within cl-bloggy."
  `((":root"
     :--colourone ,*colourone*
     :--colourtwo ,*colourtwo*
     :--colourthree ,*colourthree*
     :--colourfour ,*colourfour*
     :--background "var(--colourone)"
     :--fprimary "var(--colourtwo)"
     :--fsecondary "var(--colourfour)"
     :--fnormal "var(--colourtwo)"
     :--fsizelarge "20pt"
     :--fsize "15pt"
     :--fsizesmall "13pt"
     :--bordercolour "var(--colourtwo)")
    (".title"
     :color "var(--fprimary)"
     :margin-bottom 1rem)
    (".date"
     :font-size "var(--fsizesmall)"
     :margin-top 1rem
     :margin-bottom 3rem
     :color "var(--fsecondary)")
    (".description"
     :color "var(--fsecondary)")
    (".tags a"
     :color "var(--colourfive)")
    (".subtitle"
     :margin-bottom 0)
    (".http-code"
     :color "var(--fprimary)")
    (".message"
     :color "var(--fsecondary)")
    (".return-links"
     :margin-bottom 3rem)
    (a
     :color "var(--fsecondary)")
    (".tags span"
     :font-size "var(--fsizesmall)")
    (".tags a"
     :color "var(--fsecondary)")
    (".tags span"
     :margin-left 0.8rem)
    (".tags span:first-child"
     :margin-left 0)
    ("#home-link"
     :margin-right 1vw)
    (abbr
     :font-size "var(--fsize)")
    (summary
     :font-size "var(--fsize)")
    (p
     :font-size "var(--fsize)")
    (span
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
     :height 100%)
    (:media "screen and (min-width: 600px)"
            (":root"
             :color "red"
             :--fsizelarge "35pt"
             :--fsize "25pt"
             :--fsizesmall "20pt"
             ))
    (:media "screen and (min-width: 1000px)"
            (":root"
             :color "red"
             :--fsizelarge "65pt"
             :--fsize "50pt"
             :--fsizesmall "25pt"))
    (:media "screen and (min-width: 1500px)"
            (":root"
             :color "red"
             :--fsizelarge "35pt"
             :--fsize "20pt"
             :--fsizesmall "15pt"))))

(defmethod page-css append ((page entry))
  nil)

(defmethod page-css append ((page blog))
  "Provide default css for all blogs and subclasses of blog."
  `((".title-box"
     :margin-bottom 5vw)
    (".entry"
     :padding-left 10px
     :margin-bottom 5vh)
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
  "Provide default css for all indexes and subclasses of index."
  `((".index-entry"
     :padding-left 10px
     :margin-bottom 3rem)
    (".index-title"
     :margin-bottom 0.3rem
     :font-size "var(--fsizelarge)")
    (".index-description"
     :margin-bottom 0.7rem)
    (".tags"
     :font-size 15px
     :margin-bottom 0.5rem)
    (".index-date"
     :font-size 15px
     :margin-bottom 1rem)
    (".index-entry:hover"
     :border-color "var(--bordercolour)"
     :border-left-style solid
     :border-left-width 3px
     :padding-left 7px)))

