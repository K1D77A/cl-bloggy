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
               #:local-time)
  :pathname "src"
  :components ((:file "package")
               (:file "cl-bloggy")))
