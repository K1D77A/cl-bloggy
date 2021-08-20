;;;; cl-bloggy.lisp

(in-package #:cl-bloggy)

(defparameter *max-category-depth* 10)


(defun %recurse-categories-parents (category func)
  "Recurses over the category and all of its parents executing func with the current 
category and the accumulator as arguments, the result of the funcall is pushed to the 
accumulator."
  (labels ((rec (cat acc)
             (with-accessors ((parent parent))
                 cat
               (if (null parent)
                   (push (funcall func cat acc) acc)
                   (progn
                     (push (funcall func cat acc) acc)
                     (rec parent acc))))))
    (rec category nil)))

(defun %recurse-categories-children (category func)
  "Recurses over the category and all of its children executing func with the current 
category and the accumulator as arguments, the result of the funcall is pushed to the 
accumulator."
  (let ((res ()))
    (labels ((rec (cat)
               (cond ((null cat)
                      nil)
                     ((listp cat)
                      (rec (first cat))
                      (rec (rest cat)))
                     (t (push (funcall func cat) res)
                        (rec (children cat))))))
      (rec category)
      res)))

(defmethod category-all-urls ((category category) (blog blog))
  "Returns a list of all of the URLs for category"
  (%recurse-categories-parents category
                               (lambda (cat acc)
                                 (declare (ignore acc))
                                 (process-uri blog :category-url cat))))

(defmethod category-names ((category category))
  "Returns the names of all of the names of categories in category."
  (%recurse-categories-parents category
                               (lambda (cat acc)
                                 (declare (ignore acc))
                                 (name cat))))

(defmethod all-children ((category category))
  "Returns all of the children for CATEGORY."
  (%recurse-categories-children category (lambda (x) x)))

(defun entries-in-category (category blog)
  "Returns all of the entries that are associated with CATEGORY within BLOG."
  (let ((categories (all-children category)))
    (loop :for entry :in (entries blog)
          :when (some (lambda (cat)
                        (eq (category entry) cat))
                      categories)
            :collect entry)))

(defmacro easy-blog-entry ((acceptor entry-class categories title sym date
                            &key (subtitle nil)
                              (description nil)                            
                              (let-bindings-to-override-global-vars nil)) &body body)
  "
ACCEPTOR - The acceptor you want to add this entry to, ie your instance of 'bloggy-acceptor.
ENTRY-CLASS - The class you want your blog entry to be, normally this would be your 
own subclass of 'entry, this is used to determine the HTML and CSS that is generated and 
allows you to maximize customizability.
CATEGORIES - This is a list of categories like ('general' 'programming'), a category object
is searched for and if one cannot be found then one is generated for later. 
Categories are found via that list, so a category is the product of its parents, and 
their parents parents etc, this means that ('general' 'programming' 'lisp' 'common lisp')
and ('general' 'programming' 'common lisp') do not resolve to the same category, however
they would both be children of the category 'programming'.
TITLE - The title for the entry.
SYM - A keyword used to reference this blog within CL.
DATE - Is a list that is passed to the function 'new-date-timestamp' this accepts 
keyword arguments that would be used with local-time:encode-timestamp in order to 
generate a timestamp for that blog entry. This determines the order they are displayed in 
both the index and the main page.
SUBTITLE - This is an optional subtitle.
DESCRIPTION - This is an option description. In the case this is provided then the RSS
feed will display this instead of the result of evaluating the function in content. 
The description is also displayed on the index page.
LET-BINDINGS-TO-OVERRIDE-GLOBAL-VARS - This gives you an opportunity to per entry 
lexically bind any of the global variables. Although I have not tested this so idk if
it works.
"
  (alexandria:with-gensyms (new-entry category timestamp)
    `(defun ,(intern (string-upcase  (format nil "entry-~A" sym))) ()
       (let* ((,category (find-category ',categories (blog ,acceptor)))
              (,timestamp (apply #'new-date-timestamp ',date))
              (,new-entry
                (new-blog-entry (blog ,acceptor) ',entry-class ,title ,sym
                                ,category ,timestamp
                                (lambda (entry) (declare (ignorable entry))
                                  (spinneret:with-html ,@body))
                                :subtitle ,subtitle
                                :description ,description)))
         (add-route
          (make-route :GET
                      (process-uri ,new-entry :encode)
                      (lambda ()
                        (if ',let-bindings-to-override-global-vars
                            (let ,let-bindings-to-override-global-vars
                              (to-html ,new-entry))
                            (to-html ,new-entry))))
          ,acceptor)))))

(defun find-category (list blog &optional (createp t))
  "Attempts to find the category that is in list.
List contains the names of the categories, say ('general' 'programming' 'common lisp' 
'generics') the correct category would be the very final one, that is a child of 
each of those categories successively, if you swapped 'common lisp' for 'clojure' then 
the final category would have to be new. So categories are found by their parents."
  (unless (<= (length list) *max-category-depth*)
    (error 'exceeded-category-depth :cat-list list
                                    :blog blog
                                    :message "Exceeded maximum category depth"))
  (with-accessors ((categories categories))
      blog
    (labels ((check-for-kids (category names)
               (if (null names)
                   category
                   (let ((found? (find (first names) (children category)
                                       :key #'name :test #'string-equal)))
                     (if found? 
                         (check-for-kids found? (rest names))
                         (when createp
                           (make-children category names)
                           (check-for-kids category names)))))))
      (let* ((current (first list))
             (remainder (rest list))
             (found? (find current categories :key #'name :test #'string-equal)))
        (if found?
            (check-for-kids found? remainder)
            (when createp
              (let ((cat (make-instance 'category :name current
                                                  :sym (intern current))))
                (push (make-children cat remainder) categories)
                (find-category list blog nil))))))))

(defun make-children (current names)
  "Generates the children for the category CURRENT with the names listed in NAMES."
  (labels ((rec (parent childs)
             (with-accessors ((children children))
                 parent
               (let ((child (first childs))
                     (remainder (rest childs)))
                 (unless (null child)
                   (let ((cat (make-instance 'category :name child
                                                       :sym (intern child)
                                                       :parent parent)))
                     (push cat children)
                     (rec cat remainder)))))))
    (rec current names)
    current))

(defun find-categories (names blog &optional (cats nil))
  (with-accessors ((categories categories))
      blog
    (loop :for (name . children) :on names
          :with cats := ()
          :do (let ((category (find-category name blog)))
                (setf (subcategories category)
                      (find-categories children blog cats)
                      (parents category)
                      cats)
                (push category cats))
          :finally (return cats))))

(defun new-blog (acceptor blog-class)
  "Initializes the main blog within ACCEPTOR. This must be called before both 
new-index and new-content. BLOG-CLASS should ideally be a subclass of blog. 
Uses (url (make-instance BLOG-CLASS)) to determine the url to put the main page.
Defaults to /blog/main."
  (unless (slot-boundp acceptor 'blog)
    (setf (blog acceptor)
          (make-instance blog-class)))
  (add-route
   (make-route :get (url (make-instance blog-class))
               (lambda ()
                 (let ((blog (blog acceptor)))
                   (to-html blog))))
   acceptor))

(defun new-index (acceptor index-class)
  "Initializes the main index page within (blog acceptor) using the class INDEX-CLASS.
Ideally INDEX-CLASS will be your subclass of 'index. Uses (url (make-instance INDEX-CLASS))
to determine where to put the route for the index page.
Defaults to /blog/index"
  (let ((index (make-instance index-class :blog (blog acceptor))))
    (setf (index (blog acceptor)) index)
    (add-route
     (make-route :get (url (make-instance index-class))
                 (lambda ()
                   (to-html index)))
     acceptor)))

(defun new-content (acceptor content-class)
  "Initialize the main content repo within (blog acceptor) using the class CONTENT-CLASS.
Ideally CONTENT-CLASS will be a subclass of content."
  (let ((content (make-instance content-class :blog (blog acceptor))))
    (setf (content (blog acceptor)) content)))

(defgeneric delete-entry (acceptor entry)
  (:documentation "Deletes a blog entry from (blog acceptor)"))

(defmethod delete-entry ((acceptor bloggy-acceptor) (entry entry))
  (with-accessors ((blog blog)
                   (routes routes))
      acceptor
    (let ((uri (process-uri entry :encode)))
      (remhash uri routes)
      (setf (entries blog)
            (remove entry (entries blog) :test #'eq)))))

(defmethod delete-entry ((acceptor bloggy-acceptor) (entry null)))

(defgeneric format-timestamp (stream timestamp way)
  (:documentation "formats timestamp into stream by WAY."))

(defmethod format-timestamp (stream timestamp (way (eql :site)))
  (local-time:format-timestring stream timestamp
                                :format '(:ordinal-day " of " :long-month " "
                                          :year  " at " (:hour 2) ":" (:min 2) :ampm " "
                                          :timezone)))

(defmethod format-timestamp (stream timestamp (way (eql :rss)))
  (local-time:format-timestring stream timestamp 
                                :format local-time:+rfc-1123-format+))


(defgeneric find-entry (check blog)
  (:documentation "Uses CHECK to try and find entry in BLOG."))

(defmethod find-entry ((check symbol) blog)
  (find check (entries blog) :key #'sym :test #'eq))

(defmethod delete-category ((category list) blog)
  (delete-category (find-category category blog nil) blog))

(defmethod delete-category ((category null) blog)
  nil)

(defmethod delete-category ((category category) blog)
  "Deletes a category."
  (with-accessors ((parent parent))
      category
    (if parent
        (with-accessors ((children children))
            parent
          (setf (children parent)
                (remove category children :test #'eq)))
        ;;if no parent then we know its top level
        (with-accessors ((categories categories))
            blog
          (setf categories (remove category categories :test #'eq))))))

(defgeneric clean-category (acceptor category)
  (:documentation "Deletes all the entries that are in that category and then deletes it."))

(defmethod clean-category (acceptor (category category))
  (with-accessors ((blog blog))
      acceptor 
    (mapcar (lambda (entry)
              (delete-entry acceptor entry))
            (entries-in-category category blog))
    (delete-category category blog)))
