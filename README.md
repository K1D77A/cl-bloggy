# cl-bloggy

This library implements a simple but highly extensible plugin blogging system for
Hunchentoot.

It uses its own custom handler to add routes but but you can still use hunchentoots
define-easy-handler.

# README needs a rewrite.. 
if you want to try it, clone the repo to quicklisp/local-projects, register local projects 
```lisp 
(ql:quickload :cl-bloggy)
(in-package :cl-bloggy)
<compile the contents of src/test-server.lisp using sly/slime>
(start)
```
Navigate to `http://127.0.0.1:4203/blog/main` if the categories haven't loaded properly 
then just recompile each easy-blog-entry individually with C-c C-c. 

## Getting started

To get started you need to start up hunchentoot with a new acceptor called 'bloggy-
acceptor'

```lisp

(defparameter *server* (make-instance 'bloggy-acceptor
                                      :blog (make-instance 'blog
                                                           :title "Main")
                                      :document-root "./"
                                      :port 4203 :name 'main))
                                      
```

I need to rewrite this, things have drastically changed since v2.

## Screenshot

### Main page 
![Screenshot](https://imgur.com/ydX0PrT.png "main")

### Index page 

![Screenshot](https://imgur.com/6MUih5Z.png "index")

### Single entry

![Screenshot](https://imgur.com/0FRTEoZ.png "entry")
