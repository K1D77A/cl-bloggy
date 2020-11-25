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

You can see a new initarg called :blog this must be an instance of 'blog or a
subclass of blog.

Next add the main blog page and the main index page

```lisp

(add-blog)
(add-index 'blog-index)

```
This will produce two pages, one at \*blog-root-directory\* (default "/blog/") and
\*blog-index-directory\* (default "/blog/index") which you can browse but you will not
see much (the argument 'blog-index is provided as it is the class being
instantiated, this will come in handy later when learning how to customize the blog).

![initial index & blog](https://imgur.com/cwUfqja.png)

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

## Customizing

Because this library is built from top to bottom using generic functions it is
very easy to modify the behaviour of this library, I will walk you through a few
examples of how to do this.

First lets take a look at how we would customize an aspect of how a blog-entry is
rendered; this is done by calling these methods in this specific order: 

- to-html
- html-headers
  - global-css
  - global-fonts
  - specific-css
  - to-css
- html-body
- html-footer
  - global-footer
  - specific-footer

Each of these methods can be specialized for your own subclass in order to customize
the behaviour of any of these aspects.

Say for example you wished to add a custom footer for an entry, to do this
you would first create a subclass of blog-entry

```lisp

(defclass new-blog-entry (blog-entry)
  ())

```

Then you simply create a new version of html-footer for your new class, specialize
html-footer and create a new entry passing new-blog-entry as the first argument to
easy-blog-entry.

```lisp

(defmethod html-footer ((entry new-blog-entry))
  (spinneret:with-html
    (:p "i'm a different footer!!")))

(easy-blog-entry (new-blog-entry "general" "entryboof" *server*)
  (:p "im a new entry"))

```

You will notice that you have to use spinneret to modify the HTML, thats because
at every step in the HTML rendering spinneret:with-html is called, so you must do
the same for your variant.
If we compile and browse to /blog/general/entryboof we will see:

![custom footer](https://imgur.com/7YJp5EZ.png)

If you wish to customize the default blog page then simply change the (make-instance ) when creating your hunchentoot server to a subclass of 'blog where you have
created your own version of the methods in the HTML rendering pipeline.

### Customizing the CSS

This is quite easy, the last method called in html-headers is a method called to-css
this method adds inline CSS to the document to control the styling. It is quite
easy to overwrite this using the same technique as above, simply create your own
subclass of what you wish to modify and add your CSS like so:

```lisp

(defclass new-blog-entry2 (blog-entry)
  ((css-rules
    :initform '(()))));; removing the default CSS rules

```

now the default version of to-css for blog-entry and blog (so subsequently blog-index as this is a subclass of blog) simply renders the contents of the slot
css-rules and returns that:

```lisp

(defmethod to-css ((entry blog-entry))
  (spinneret:with-html
    (:style :type "text/css"
            (apply #'lass:compile-and-write (css-rules entry)))))

```

Both blog, blog-entry and blog-index have default CSS which can be found in
\*blog-css-rules\* \*blog-entry-css\* and \*index-css-rules\* respectively, these
will serve as examples (see /src/generate-css.lisp) for you to follow on how
to properly form the CSS; the CSS is all written using Shinmeras LASS,
so you must make (css-rules ..) a list of lists. To understand which classes/id's
are used when generating the HTML see /src/generate-html.lisp and browse through
the spinneret forms to see what you can modify, \*blog-entry-css-rules\* also
contains all the ids for each main part of an entry.

I will finish with one final example of how you would go about changing the default
behaviour of the blog; by default entries are listed from newest to oldest,
I will quickly show you how to list them from oldest to newest.

The functionality for controlling the listing of entries is within the method
html-body the default is:

```lisp

(defmethod html-body ((blog blog))
  (spinneret:with-html
    (:div :id "all-entries"
          (dolist (blog (sort (entries blog) #'> :key #'creation-date-universal))
            (:div :class "entry"
                  :id (id blog)
                  (html-body blog))))))

```

So we want to reverse the order of listing, this is very simple as you can see
all we are doing is creating a div and within it listing all of the entries
contained within the object 'blog', but we are sorting this list by the key
#'creation-date-universal, so to modify this functionality we will create the
following method on our own subclass of blog called new-blog here:

```lisp

(defmethod html-body ((blog new-blog))
  (spinneret:with-html
    (:div :id "all-entries"
          (dolist (blog (sort (entries blog) #'< :key #'creation-date-universal))
            (:div :class "entry"
                  :id (id blog)
                  (html-body blog))))))

```

Now with the new blog you will have to modify the 'blog within your \*server\* instance
to a new instance using new-blog instead of the default blog.

I hope this helps.


## Extra

Take a look at /src/test-server.lisp for a few examples.

Take a look at /src/generate-html.lisp to see how the default behaviour is implemented.

To modify the index page, subclass blog-index and call (make-index ..) with your
version of blog-index.




## License

MIT

