# cl-bloggy

This library implements a simple but highly extensible plugin blogging system for
Hunchentoot.

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
You can see a new initarg called :blog this must be an instance of 'blog or a
subclass of blog.

Next add the main blog page and the main index page

```lisp

(add-blog)
(add-index)

```
This will produce two pages, one at *blog-root-directory* (default "/blog/") and
*blog-index-directory* (default "/blog/index") which you can browse but you will not
see much.

![initial index & blog](https://imgur.com/IZ2agj3.png)

Next we must add a simple entry using `easy-blog-entry`
This macro accepts 5 arguments, the first is the class of the blog-entry you want
to create, the next the category, the next the title of the entry and last the
hunchentoot server; the last and most important variable is a form that
would be valid when passed to spinnerets 'with-html'. Here is an example:

```lisp

(easy-blog-entry (blog-entry "general" "entry1" *server*)
  (:div :class "elp"
        (:h1 "A story to tell")
        (:p "once upon a time in a land far away")))

```
As you can see /blog/ and /blog/index have changed.

![blog 1 entry](https://imgur.com/ULUx26I.png)

and the index:

![index 1 entry](https://imgur.com/QOFntiK.png)


## License

MIT

