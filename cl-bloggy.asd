;;;; cl-bloggy.asd

(asdf:defsystem #:cl-bloggy
  :description "A self hosted and extensible blogging system built atop Hunchentoot. 
The user writes blog entries by connecting to their remote image with sly/slime and 
creates new entries by writing HTML using spinneret."
  :author "K1D77A"
  :license  "GPL-3"
  :version "2.0.0"
  :serial t
  :depends-on (#:hunchentoot
               #:spinneret
               #:alexandria
               #:lass
               #:str
               #:xml-emitter
               #:do-urlencode
               #:local-time
               #:lorem-ipsum)
  :pathname "src"
  :components ((:file "package")
               (:file "classes")
               (:file "conditions")
               (:file "hunchentoot-handler")
               (:file "request-processing")
               (:file "generate-css")
               (:file "generate-html")
               (:file "cl-bloggy")))
