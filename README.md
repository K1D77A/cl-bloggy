# cl-bloggy

This library implements a simple but highly extensible plugin blogging system for
Hunchentoot.

It uses its own custom handler to add routes but but you can still use hunchentoots
define-easy-handler.

This is designed to be used with Sly/SLIME's remote connection ability, when you want to 
add a new entry you would open your ssh tunnel, connect with sly/slime and then 
compile your new entry straight into the source written on your own machine. 
Ofcourse you could use a makefile so that you can just deploy the image anywhere with 
all of your previous posts ready to go. 

CL-BLOGGY is in use with its default theme [@my blog](https://k1d77a.com/blog/main)


# README needs a rewrite.
if you want to try it, clone the repo to quicklisp/local-projects, register local projects 
```lisp 
(ql:quickload :cl-bloggy)
(in-package :cl-bloggy)
<compile the contents of src/test-server.lisp using sly/slime>
(start)
```
Navigate to `http://127.0.0.1:4203/blog/main` 
