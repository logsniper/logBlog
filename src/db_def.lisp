(in-package :logsniper.logBlog)
(use-package :elephant)

; Open the store where our data is stored
(defvar *elephant-store* (open-store '(:clsql (:sqlite3 "./database/blog.db"))))

(defclass blog-paragraph ()
           ((content :initarg :content
                     :accessor content
                     :initform "")
            (para-type :initarg :para-type ; value of type : 'ptype-head, 'ptype-body, 'ptype-image
                       :accessor para-type
                       :initform 'ptype-body)))

(defpclass message-post ()
           ((author :initarg :author
                    :accessor author)
            (email :initarg :email
                    :accessor email)
            (content :initarg :content
                     :accessor content)
            (timestamp :accessor timestamp
                       :initform (get-universal-time)
                       :index t)
            (ip-addr :initarg :ip-addr
                :accessor ip-addr
                :initform nil)))

(defpclass blog-post ()
  ((blogid
        :initarg :blogid
        :accessor blogid
        :index t)
   (title :initarg :title
          :accessor title
          :initform "")
   (tags :initarg :tags
         :accessor tags)
   (body :initarg :body ; list of blog-paragraph
         :accessor body)
   (timestamp :initarg :timestamp
              :accessor timestamp
              :initform (get-universal-time)
              :index t)
   (messages :initarg :messages ; list of message-post
             :accessor messages
             :initform ())
   (visitor-count :initform 0
                  :accessor visitor-count)
   (last-modified-time :initform (get-universal-time)
                       :accessor last-modified-time)))

(defpclass userinfo ()
           ((email :initarg :email
                   :accessor email
                   :index t)
            (author :initarg :author
                  :accessor author)
            (password :initarg :password
                      :accessor password
                      :initform nil)
            (last-time :initarg :last-time
                       :accessor last-time
                       :initform (get-universal-time))
            (last-ip :initarg :last-ip
                     :accessor last-ip
                     :initform nil)))

(defmacro def-elephant-root (pset-name)
  (with-gensyms (item pset-tag)
                `(defvar ,pset-name (or (get-from-root ',pset-tag)
                                        (let ((,item (make-pset)))
                                          (add-to-root ',pset-tag ,item)
                                          ,item)))))

(def-elephant-root *blog-posts*)
(def-elephant-root *blog-posts-old-version*)
(def-elephant-root *user-pset*)
