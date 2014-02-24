(in-package :logsniper.logBlog)
(use-package :elephant)

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

(defun get-message (msgid)
  (if msgid
    (get-instance-by-value 'message-post 'msgid msgid)
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

(defun add-message (content ip-addr owner-blogid)
  (let ((userinfo (update-user-info)))
    (if (and userinfo (none-of-them-is-empty content ip-addr owner-blogid))
      (make-instance 'message-post :msgid (incf (msg-count (get-items-counter))) 
                                   :author (author userinfo)
                                   :email (email userinfo)
                                   :content content :ip-addr ip-addr
                                   :owner-blogid owner-blogid))))
