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

(defun add-visitor-count (blog)
  (if blog (incf (visitor-count blog))))

(defparameter *msg-count-mutex* (sb-thread:make-mutex :name "msg-count-mutex"))

(defun get-msg-id ()
  (sb-thread:with-mutex (*msg-count-mutex*)
    (incf (msg-count (get-items-counter)))))

(defun add-message (content ip-addr owner-blogid)
  (let ((userinfo (update-user-info)))
    (if (and userinfo (none-of-them-is-empty content ip-addr owner-blogid))
      (let ((new-msg (make-instance 'message-post :msgid (get-msg-id)
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

(defun blog-filter-with-month (blog need-month)
  ; similar algorithm with blog-filter-with-tag
  (if (published blog)
    (or (not need-month) (string= need-month (timestamp-to-year-month (timestamp blog))))
    nil))

(defun deep-summarise-blog-tags ()
  (let ((tag-hash (make-hash-table :test #'equal))
        (blog-list (pset-list *blog-posts*)))
    (loop for blog in blog-list
          when (published blog)
          do (loop for tag in (tags blog)
                   do (if (gethash tag tag-hash)
                        (incf (gethash tag tag-hash))
                        (setf (gethash tag tag-hash) 1))))
    (loop for k being the hash-keys in tag-hash using (hash-value v)
          collect (list k v))))

(defun summarise-blog-months ()
  (let ((month-hash (make-hash-table :test #'equal))
        (blog-list (pset-list *blog-posts*)))
    (loop for blog in blog-list
          when (published blog)
          do (let ((year-month (timestamp-to-year-month (timestamp blog))))
               (if (gethash year-month month-hash)
                 (incf (gethash year-month month-hash))
                 (setf (gethash year-month month-hash) 1))))
    (loop for k being the hash-keys in month-hash using (hash-value v)
          collect (list k v))))

(defun refresh-database-connection ()
  (close-store)
  (open-store *store-spec* :thread t)
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

(defparameter *blog-tags-list* (list))
(defparameter *blog-months-list* (list))

(defun refresh-blog-tag-month-list ()
  (setf *blog-tags-list* (sort (deep-summarise-blog-tags) #'string< :key #'first))
  (setf *blog-months-list* (sort (summarise-blog-months) #'string< :key #'first)))

(defun summarise-blog-tags ()
  (sort *blog-tags-list* #'> :key #'second))

(defun save-blog (blog)
  (let ((old-blog (get-blog (blogid blog))))
    (if old-blog
      (progn
        (insert-item old-blog *blog-posts-old-version*)
        (remove-item old-blog *blog-posts*))))
  (insert-item blog *blog-posts*)
  (refresh-blog-tag-month-list))

(defun output-all-blog-to-text (filename)
  (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((blog-list (pset-list *blog-posts*)))
      (loop for blog in blog-list
            do (progn
                 (format stream "-------------------------------------~%")
                 (format stream "id: ~a~%" (blogid blog))
                 (format stream "title: ~a~%" (title blog))
                 (format stream "tags: ~a~%" (join-string-with-comma (tags blog)))
                 (format stream "timestamp: ~a~%" (timestamp blog))
                 (loop for para in (body blog)
                       for i from 0 to *max-paragraph-num*
                       do (format stream "No.~a [type:~a] ~a~%" i (para-type para) (content para))))))))

