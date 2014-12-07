(in-package :logsniper.logBlog)

(defun query-userinfo-by-email (email)
  (if email
    (find-item email *user-pset* :key #'email :test #'equal)))

(defun query-userinfo-by-token (tok)
  (if tok
    (find-item tok *user-pset* :key #'token :test #'equal)))

(defun query-userinfo-by-author (author)
  (if author
    (find-item author *user-pset* :key #'author :test #'equal)))

(defun generate-token (userinfo)
  (if userinfo
    (let ((string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream string)
        (format stream "~a#~a#~a#~a"
                (email userinfo)
                (password userinfo)
                (last-time userinfo)
                (last-ip userinfo))
        (log-info "[gen-token]~a" string)
        (md5sum string)))
    "none"))

(defun combine-password-salt (psw slt)
  (if psw (format nil "~a~a" psw slt)))

(defun check-authentication (userinfo password)
  (and userinfo (string= (md5sum (combine-password-salt password (salt userinfo)))
                             (password userinfo))))

(defun get-cookie-user-info ()
  (let* ((token (hunchentoot:cookie-in "__token"))
         (userinfo (query-userinfo-by-token token)))
        (if (and userinfo (< (get-universal-time) (token-expire userinfo)))
          userinfo)))

(defun get-post-user-info ()
  (let ((email (hunchentoot:post-parameter "email")))
    (if email
      (query-userinfo-by-email email))))

(defun update-user-info-db (userinfo &key author password)
  (if (none-of-them-is-empty author) (setf (author userinfo) author))
  (if (none-of-them-is-empty password)
      (setf (password userinfo) (md5sum (combine-password-salt password (salt userinfo))))))

(defun update-user-info-cookie (userinfo)
  (setf (token userinfo) (generate-token userinfo))
  (setf (token-expire userinfo) (+ (get-universal-time) *cookie-effective-period*))
  (let ((token-kv (hunchentoot:set-cookie "__token" :value (token userinfo))))
    (hunchentoot:set-cookie* token-kv)))

(defun add-user (email author password)
  (let ((info-complete-p (none-of-them-is-empty email author password))
        (email-exist-p (query-userinfo-by-email email))
        (author-exist-p (query-userinfo-by-author author))
        (hintid 1))
    (if (and info-complete-p (not email-exist-p) (not author-exist-p))
      (let ((salt (generate-salt)))
        (insert-item (make-instance 'userinfo :userid (incf (user-count (get-items-counter))) :author author :email email
                                              :salt salt :password (md5sum (combine-password-salt password salt))) 
                     *user-pset*))
      (if info-complete-p (setf hintid 3)
        (if email-exist-p (setf hintid 2)
          (if author-exist-p (setf hintid 4)))))
    hintid))

(defun update-user-info ()
  (let* ((email (hunchentoot:post-parameter "email"))
         (author (hunchentoot:post-parameter "author"))
         (password (hunchentoot:post-parameter "password"))
         (token (hunchentoot:cookie-in "__token"))
         (userinfo-e (query-userinfo-by-email email))
         (userinfo-t (query-userinfo-by-token token)))
    (if userinfo-t
      ; if token is checked ok, return t directly no matter what "email", "author" and "password" are
      userinfo-t
      (if userinfo-e
        ; if fail to check token but email&password combination is ok, it is also valid and the cookie should be updated
        (if (check-authentication userinfo-e password)
          (progn
            (update-user-info-db userinfo-e)
            (update-user-info-cookie userinfo-e)
            userinfo-e))
        ; if both token and email&password combination is invalid, but none of post items is empty, new user will be created
        (let ((hintid (add-user email author password)))
          (if (= 1 hintid) ; successfully create new user
            (let ((userinfo-e (query-userinfo-by-email email)))
              (if userinfo-e
                (progn 
                  (update-user-info-cookie userinfo-e)
                  userinfo-e)))))))))

(defun logout (userinfo)
  (if userinfo
    (setf (token userinfo) nil)
    (log-warning "failed to logout.")))

(defun output-active-users ()
  (sb-thread:with-mutex (*active-user-hash-mutex*)
    (let ((unreg-num 0)
          (userlist (list)))
      (loop for k being the hash-keys in *active-user-hash* using (hash-value user-status)
            do (let ((userinfo (query-userinfo-by-email k)))
                 (if userinfo
                   (push (author userinfo) userlist)
                   (incf unreg-num))))
      (setf *active-user-num* (+ (length userlist) unreg-num))
      (list userlist unreg-num))))

(defmacro with-cookie-user ((userinfo) &body body)
  `(let ((,userinfo (get-cookie-user-info))
         (user-key nil)
         (replaced-key nil)
         (block-it nil))
     (incf (pageview-count (get-items-counter)))
     (if ,userinfo
       (progn
         (setf user-key (email ,userinfo))
         (setf replaced-key (hunchentoot:cookie-in "unreg")))
       (let ((unreg (hunchentoot:cookie-in "unreg")))
         (if (not (none-of-them-is-empty unreg))
           ;; If this is someone who has no cookie info, set cookie and use a default string as its key for once.
           (progn
             (hunchentoot:set-cookie "unreg" :value (get-random-string))
             (setf user-key *default-user-key*))
           (setf user-key unreg))))
     (let ((user-status (update-active-user user-key :replaced-key replaced-key)))
       ;; Update user status. If current user has sent too many requests, then block it.
       (sb-thread:with-mutex (*active-user-hash-mutex*)
         (if (and user-status (> (request-in-this-minute user-status) *max-request-per-user-per-minute*))
           (setf block-it t))))
     (unless block-it
       ,@body)))
