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
     :--entrypadding "1rem"
     :--entrypaddingborder "0.7rem"
     :--borderwidth "0.3rem"
     :--fsizelarge "3.5rem"
     :--fsize "2.5rem"
     :--fsizesmall "1.8rem"
     :--bordercolour "var(--colourtwo)")
    (".title"
     :color "var(--fprimary)"
     :margin-bottom 1rem)
    (h1 :font-size 6rem)
    (h2 :font-size 5rem)
    (h3 :font-size 4rem)
    (h4 :font-size 3.5rem)
    (h5 :font-size 3rem)
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
    ("code"
     :font-size "var(--fsizesmall)")
    ("#home-link"
     :margin-right 1vw
     :font-size "var(--fsizelarge)")
    ("#index-link"
     :font-size "var(--fsizelarge)")
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
    (body
     :width 100%
     :display block)
    (html
     :background-color "var(--background)"
     :padding 5rem 
     :color "var(--fnormal)"
     :margin-left 0
     :margin-right 0
     :max-height 100%
     :height 100%)
    (:media "only screen and (max-width: 600px)"
            (":root"
             :--fsizelarge "5rem"
             :--fsize "4rem"
             :--fsizesmall "3rem"
             )
            (html
             :padding 1rem)
            )
    (:media "only screen and (min-width: 600px)"
            (":root"
             :--fsizelarge "5rem"
             :--fsize "4rem"
             :--fsizesmall "3rem"             
             ))
    (:media "only screen and (min-width: 768px)"
            (":root"
             :--fsize "4rem"
             :--fsizesmall "3rem"
             ))    
    (:media "only screen and (min-width: 992px)"
            (":root"
             :--fsize "4.5rem"
             :--fsizesmall "3.5rem"))
    (:media "only screen and (min-width: 1200px) and (orientation:landscape)"
            (":root"
             :--fsizelarge "3.5rem"
             :--fsize "2.5rem"
             :--fsizesmall "1.5rem"))
    (:media "only screen and (min-width: 1200px) and (orientation:portrait)"
            (":root"
             :--fsizelarge "4.5rem"
             :--fsize "4rem"
             :--fsizesmall "3.5rem"))))

(defmethod page-css append ((page entry))
  `((".content"
     :margin-bottom 5rem)))

(defmethod page-css append ((page blog))
  "Provide default css for all blogs and subclasses of blog."
  `((".title-box"
     :margin-bottom 5rem)
    (".entry"
     :padding-left "var(--entrypadding)"
     :margin-bottom 5rem)
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
     :border-left-width "var(--borderwidth)"
     :padding-left "var(--entrypaddingborder)")))

(defmethod page-css append ((page index))
  "Provide default css for all indexes and subclasses of index."
  `((".index-entry"
     :padding-left "var(--entrypadding)"
     :margin-bottom 3rem)
    (".index-title"
     :margin-bottom 0.3rem
     :font-size "var(--fsizelarge)")
    (".index-description"
     :margin-bottom 0.7rem)
    (".tags"
     :font-size "var(--fsizesmall)"
     :margin-bottom 0.5rem)
    (".index-date"
     :font-size "var(--fsizesmall)"
     :margin-bottom 1rem)
    (".index-entry:hover"
     :border-color "var(--bordercolour)"
     :border-left-style solid
     :border-left-width "var(--borderwidth)"
     :padding-left "var(--entrypaddingborder)")))

