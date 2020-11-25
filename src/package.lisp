;;;; package.lisp

(defpackage #:cl-bloggy
  (:use #:cl)
  (:nicknames :bloggy)
  (:export #:blog-entry
           #:blog
           #:blog-index
           #:category
           #:creation-date
           #:creation-date-universal
           #:css-rules
           #:title
           #:id
           #:content
           #:entries
           #:blog
           ;;accessors above
           #:*blog-entry-css-rules*
           #:*blog-css-rules*
           #:*index-css-rules*
           #:to-css
           #:global-css
           #:global-fonts
           #:specific-css
           ;;css processing above
           #:specific-footer
           #:to-html
           #:html-headers
           #:html-body
           ;;html rendering above
           #:bloggy-acceptor))
           
           
