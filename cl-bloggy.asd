;;;; cl-bloggy.asd

(asdf:defsystem #:cl-bloggy
  :description "A simple extendable blogging system to use with Hunchentoot"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
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
               (:file "classes&conditions")
               (:file "hunchentoot-handler")
               (:file "request-processing")
               (:file "generate-css")
               (:file "generate-html")
               (:file "cl-bloggy")))
