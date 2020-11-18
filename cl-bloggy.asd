;;;; cl-bloggy.asd

(asdf:defsystem #:cl-bloggy
  :description "A simple extendable blogging system to use with Hunchentoot"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:spinneret
               #:lass
               #:str
               #:local-time)
  :pathname "src"
  :components ((:file "package")
               (:file "hunchentoot-handler")
               (:file "test-server")
               (:file "cl-bloggy")))
