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
            (content :initarg :content
                     :accessor content)
            (timestamp :accessor timestamp
                       :initform (get-universal-time)
                       :index t)
            (ip-addr :initarg :ip-addr
                :accessor ip-addr
                :initform "nil")))

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

; Container for all our blog posts
(defvar *blog-posts* (or (get-from-root "blog-posts")
                         (let ((blog-posts (make-pset)))
                           (add-to-root "blog-posts" blog-posts)
                           blog-posts)))
(defvar *blog-posts-old-version* (or (get-from-root "blog-posts-old-version")
                         (let ((blog-posts (make-pset)))
                           (add-to-root "blog-posts-old-version" blog-posts)
                           blog-posts)))

(defun get-blogid ()
  (let ((bloglist (pset-list *blog-posts*))
        (newest-blogid -1))
    (loop for blog in bloglist
          do (if (and blog (> (blogid blog) newest-blogid))
               (setq newest-blogid (blogid blog))))
    (1+ newest-blogid)))

(defun get-empty-blog ()
  (make-instance 'blog-post :blogid (get-blogid) :body (loop for idx from 0 to *max-paragraph-num* collect (make-instance 'blog-paragraph))))

(defun get-blog (blogid)
  (if blogid
    (find-item blogid *blog-posts* :key #'blogid :test #'equal)
    nil))

(defun get-non-nil-blog (blogid)
  (let ((blog (get-blog blogid)))
    (if blog blog (get-empty-blog))))

(defun save-blog (blog)
  (let ((old-blog (get-blog (blogid blog))))
    (if old-blog
      (progn
        (insert-item old-blog *blog-posts-old-version*)
        (remove-item old-blog *blog-posts*))))
  (insert-item blog *blog-posts*))

(defun add-visitor-count (blog)
  (if blog (incf (visitor-count blog))))

(defun add-message (author content ip-addr)
  (unless (or (equal author nil) (string= author "") (equal content nil) (string= content ""))
    (let ((newest-msg (car (nreverse (get-instances-by-range 'message-post 'timestamp nil nil)))))
      (unless (and newest-msg (string= ip-addr (ip-addr newest-msg)) (string= content (content newest-msg)))
        (make-instance 'message-post :author author :content content :ip-addr ip-addr)))))
