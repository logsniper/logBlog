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

(defun get-user (email)
  (if email
    (find-item email *user-pset* :key #'email :test #'equal)
    nil))

(defun add-message (author email content ip-addr)
  (if (update-user-info)
    (unless (or (not author) (string= author "") (not email) (string= email "") (equal content nil) (string= content ""))
      (let ((newest-msg (car (nreverse (get-instances-by-range 'message-post 'timestamp nil nil)))))
        (unless (and newest-msg (string= email (email newest-msg)) (string= content (content newest-msg)))
          (make-instance 'message-post :author author :email email :content content :ip-addr ip-addr))))))
