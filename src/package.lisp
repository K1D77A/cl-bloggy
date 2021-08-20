;;;; package.lisp

(defpackage #:cl-bloggy
  (:use #:cl)
  (:nicknames :bloggy)
  (:export
   ;;;;hunchentoot-handler.lisp
   #:make-route
   #:add-route
   #:remove-route

   ;;;;classes.lisp
   ;;;global vars
   #:*blog-root-directory*
   #:*blog-index-directory*
   ;;;classes
   ;;special request class
   #:special-request
   #:acceptor
   #:request
   #:uri
   #:split-uri
   #:category
   #:r-method

   #:rss-request
   #:category-request
   #:rss-category-request
   #:atom-request

   ;;entry class
   #:entry
   #:date
   #:title
   #:subtitle
   #:id
   #:content
   #:description
   #:blog

   ;;blog class
   #:entries
   #:categories
   #:author
   #:domain
   #:description
   #:language
   #:index

   ;;category class
   #:name
   #:sym
   #:children
   #:parent

   ;;content class
   ;;already exported

   ;;index class
   ;;already exported

   ;;;;conditions.lisp
   ;;;conditions
   ;;toplevel condition
   #:bloggy-condition
   #:message

   ;;missing-required-feature condition
   #:missing-required-feature
   #:feature
   #:instructions

   ;;unknown-content condition
   #:content

   ;;request-condition and subclasses
   #:request-condition
   #:http-code

   #:exceeded-category-depth
   #:cat-list
   
   #:rss%bad-categories

   #:missing-categories

   #:missing-content

   #:malformed-url

   ;;;condition functions
   #:display-condition

   
   ;;;;hunchentoot-handler.lisp
   ;;; related to the special handler
   ;;most remains unexported as it is unimportant.
   #:bloggy-acceptor
   #:routes


   ;;;;request-processing.lisp
   #:handle-unknown-uri
   #:determine-request-type
   #:process-special-request

   ;;;;content.lisp
   ;;;classes
   ;;uploaded-content class
   #:uploaded-content
   #:path
   #:mime
   #:data

   ;;image-content class
   #:image-content

   ;;;helpers
   #:add-content
   #:find-content
   #:easy-image
   

   ;;;;uri-processing.lisp
   ;;only one thing to export
   #:process-uri

   
   ;;;;generate-rss.lisp
   ;;only one generic to export
   #:generate-rss


   ;;;;generate-css.lisp
   ;;;global vars
   #:*colourone*
   #:*colourtwo*
   #:*colourthree*
   #:*colourfour*
   
   ;;;only one generic to export
   ;;please note that page-css is using the append method combination.
   #:page-css

   
   ;;;;generate-html.lisp
   ;;;global vars
   #:*default-css*
   ;;;generics
   #:to-html
   #:html-headers
   #:html-body
   #:html-footer

   ;;;;cl-bloggy.lisp
   ;;;global vars
   #:*max-category-depth*
   ;;;categories
   #:category-names
   #:all-children
   #:entries-in-category
   #:find-category
   #:find-categories
   #:delete-category
   #:clean-category
   
   ;;;primary macro for making entries
   #:easy-blog-entry

   ;;;functions to perform setup
   #:new-blog
   #:new-index
   #:new-content

   ;;;entries
   #:delete-entry
   #:find-entry

   ;;;timestring helpers
   #:format-timestamp))



