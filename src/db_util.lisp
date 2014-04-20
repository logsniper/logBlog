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
  (make-instance 'blog-post :blogid (get-blogid) :body (loop for idx from 0 to *init-paragraph-num* collect (make-instance 'blog-paragraph))))

(defun get-blog (blogid)
  (if blogid
    (find-item blogid *blog-posts* :key #'blogid :test #'equal)
    nil))

(defun get-message (msgid)
  (if msgid
    (get-instance-by-value 'message-post 'msgid msgid)
    nil))

(defun get-all-messages ()
  (nreverse (get-instances-by-range 'message-post 'timestamp nil nil)))

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
      (let ((new-msg (make-instance 'message-post :msgid (incf (msg-count (get-items-counter))) 
                                                  :author (author userinfo)
                                                  :email (email userinfo)
                                                  :content content :ip-addr ip-addr
                                                  :owner-blogid owner-blogid)))
        (if new-msg
          (progn
            (incf (msg-num (get-blog owner-blogid)))
            (log-info "[new-msg][succ]email:~a,content:'~a',time:~a,ip:~a"
                      (email new-msg) (content new-msg) (timestamp new-msg) (ip-addr new-msg)))
          (log-warning "[new-msg][fail]"))
        new-msg))))

(defun blog-filter-with-tag (blog need-tag)
  ; If need-tag is not nil, this function will return whether the blog can pass this filter.
  ; If need-tag is nil, return "pass" directly.
  ; If the blog isn't published, cut it off directly
  (if (published blog)
    (if need-tag
      (let ((flag nil))
        (loop for tag in (tags blog)
              do (if (string= need-tag tag)
                   (setq flag t)))
        flag)
      t)
    nil))

(defun summarise-blog-tags ()
  (let ((counter (make-hash-table :test #'equal)))
    (loop for blog in (pset-list *blog-posts*)
          when (published blog)
          do (loop for tag in (tags blog)
                   do (if (gethash tag counter)
                        (incf (gethash tag counter))
                        (setf (gethash tag counter) 1))))
    (let ((counter-list (loop for k being the hash-keys in counter using (hash-value v)
                              collect (list k v))))
      (sort counter-list #'> :key #'second))))

(defun refresh-database-connection ()
  (close-store)
  (open-store *store-spec*)
  (log-warning "refreshed db connection, pv count:~a" (pageview-count (get-items-counter))))

(defun get-prev-next-blogid (blogid)
  (let ((res-prev -1) (res-next -1) (prev -1))
    (loop for blog-post in (pset-list *blog-posts*)
          when (blog-filter-with-tag blog-post nil)
          do (let ((cur-blogid (blogid blog-post))) 
               (if (= blogid cur-blogid)
                 (setf res-prev prev))
               (if (= blogid prev)
                 (setf res-next cur-blogid))
               (setf prev cur-blogid)))
    (list res-prev res-next)))
