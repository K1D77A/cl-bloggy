# cl-bloggy

This library implements a simple but highly extensible plugin blogging system for
Hunchentoot.

It uses its own custom handler to add routes but but you can still use hunchentoots
define-easy-handler.

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

![Screenshot](https://imgur.com/wcUMrdt.png "Default theme screenshot")
