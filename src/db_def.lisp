(in-package :logsniper.logBlog)
(use-package :elephant)

; Open the store where our data is stored
(defparameter *store-spec* '(:clsql (:sqlite3 "./database/blog.db")))
(open-store *store-spec*)

(defclass blog-paragraph ()
           ((content :initarg :content
                     :accessor content
                     :initform "")
            (para-type :initarg :para-type ; value of type : 'ptype-head, 'ptype-body, 'ptype-image
                       :accessor para-type
                       :initform 'ptype-body)))

(defpclass items-counter ()
           ((blog-count :initform 0
                        :accessor blog-count)
            (msg-count :initform 0
                       :accessor msg-count)
            (user-count :initform 0
                        :accessor user-count)
            (pageview-count :initform 0
                            :accessor pageview-count)))

(defpclass message-post ()
           ((msgid :initarg :msgid
                   :accessor msgid
                   :index t)
            (owner-blogid :initarg :owner-blogid
                          :accessor owner-blogid)
            (author :initarg :author
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
                :initform nil)
            (repliers :initform ()
                      :accessor repliers)))

(defpclass blog-post ()
  ((blogid
        :initarg :blogid
        :accessor blogid
        :index t)
   (title :initarg :title
          :accessor title
          :initform "")
   (tags :initarg :tags ; list of tags
         :accessor tags
         :initform '())
   (body :initarg :body ; list of blog-paragraph
         :accessor body)
   (timestamp :initarg :timestamp
              :accessor timestamp
              :initform (get-universal-time)
              :index t)
   (messages :initarg :messages ; list of message-post
             :accessor messages
             :initform ())
   (msg-num :initform 0
            :accessor msg-num)
   (visitor-count :initform 0
                  :accessor visitor-count)
   (last-modified-time :initform (get-universal-time)
                       :accessor last-modified-time)
   (published :initform nil
              :accessor published)))

(defpclass userinfo ()
           ((userid :initarg :userid
                   :accessor userid
                   :index t)
            (email :initarg :email
                   :accessor email
                   :index t)
            (author :initarg :author
                  :accessor author)
            (salt :initarg :salt
                  :accessor salt
                  :initform (generate-salt))
            (password :initarg :password
                      :accessor password
                      :initform nil)
            (last-time :initarg :last-time
                       :accessor last-time
                       :initform (get-universal-time))
            (last-ip :initarg :last-ip
                     :accessor last-ip
                     :initform nil)
            (token :initform nil
                   :accessor token
                   :index t)
            (token-expire :initform 0
                          :accessor token-expire)
            (forbidden :initform 0
                       :accessor forbidden)
            (manager :initform nil
                     :accessor manager)))

(defmacro def-elephant-root (pset-name)
  (with-gensyms (item)
                `(let ((pset-tag (string ',pset-name)))
                   (defvar ,pset-name (or (get-from-root pset-tag)
                                          (let ((,item (make-pset)))
                                            (add-to-root pset-tag ,item)
                                            ,item))))))

(def-elephant-root *items-counter*)
(def-elephant-root *blog-posts*)
(def-elephant-root *blog-posts-old-version*)
(def-elephant-root *user-pset*)

(defun get-items-counter ()
  (let ((counter (car (pset-list *items-counter*))))
    (if (not counter)
      (progn
        (setq counter (make-instance 'items-counter))
        (insert-item counter *items-counter*)))
    counter))

(close-store)
