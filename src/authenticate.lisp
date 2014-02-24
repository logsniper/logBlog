(in-package :logsniper.logBlog)

(defun query-userinfo-by-email (email)
  (if email
    (find-item email *user-pset* :key #'email :test #'equal)))

(defun query-userinfo-by-token (tok)
  (if tok
    (find-item tok *user-pset* :key #'token :test #'equal)))

(defun generate-token (userinfo)
  (if userinfo
    (let ((string (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
      (with-output-to-string (stream string)
        (format stream "~a#~a#~a#~a"
                (email userinfo)
                (password userinfo)
                (last-time userinfo)
                (last-ip userinfo))
        (format t "token:~a~%" string)
        (md5sum string)))
    "none"))

(defun combine-password-salt (psw slt)
  (if psw
    (with-output-to-string (stream)
      (format stream "~a~a" psw slt)
      stream)))

(defun check-authentication (userinfo password)
  (and userinfo (or (not (password userinfo))
                    (string= (md5sum (combine-password-salt password (salt userinfo)))
                             (password userinfo)))))

(defun get-cookie-user-info ()
  (let* ((token (hunchentoot:cookie-in "token"))
         (userinfo (query-userinfo-by-token token)))
        (if (and userinfo (< (get-universal-time) (token-expire userinfo)))
          (progn
            (setf (last-time userinfo) (get-universal-time))
            (setf (last-ip userinfo) (hunchentoot:real-remote-addr))
            userinfo))))

(defun get-post-user-info ()
  (let ((email (hunchentoot:post-parameter "email")))
    (if email
      (query-userinfo-by-email email))))

(defun update-user-info-db (userinfo author password)
  (if author (setf (author userinfo) author))
  (if password 
    (progn
      (setf (password userinfo) (md5sum (combine-password-salt password (salt userinfo)))))))

(defun update-user-info-cookie (userinfo)
  (setf (token userinfo) (generate-token userinfo))
  (setf (token-expire userinfo) (+ (get-universal-time) *cookie-effective-period*))
  (let ((token-kv (hunchentoot:set-cookie "token" :value (token userinfo))))
    (hunchentoot:set-cookie* token-kv)))

(defun add-user (email author password)
  (if (and (none-of-them-is-empty email author password) (not (query-userinfo-by-email email)))
    (let ((salt (generate-salt)))
      (insert-item (make-instance 'userinfo :userid (incf (user-count (get-items-counter))) :author author :email email
                                            :salt salt :password (md5sum (combine-password-salt password salt))) 
                   *user-pset*))))

(defun update-user-info ()
  (let* ((email (hunchentoot:post-parameter "email"))
         (author (hunchentoot:post-parameter "author"))
         (password (hunchentoot:post-parameter "password"))
         (token (hunchentoot:cookie-in "token"))
         (userinfo-e (query-userinfo-by-email email))
         (userinfo-t (query-userinfo-by-token token)))
    (if userinfo-t
      ; if token is checked ok, return t directly no matter what "email", "author" and "password" are
      userinfo-t
      (if userinfo-e
        ; if fail to check token but email&password combination is ok, it is also valid and the cookie should be updated
        (if (check-authentication userinfo-e password)
          (progn
            (update-user-info-cookie userinfo-e)
            userinfo-e))
        ; if both token and email&password combination is invalid, but none of post items is empty, new user will be created
        (if (none-of-them-is-empty email author password)
          (progn
            (add-user email author password)
            (let ((userinfo-e (query-userinfo-by-email email)))
              (if userinfo-e
                (progn 
                  (update-user-info-cookie userinfo-e)
                  userinfo-e)))))))))

(defun logout (userinfo)
  (setf (token userinfo) nil))

(defmacro with-cookie-user ((userinfo) &body body)
  `(let ((,userinfo (get-cookie-user-info)))
     (incf (pageview-count (get-items-counter)))
     (if ,userinfo (format t "user-info ~a ~a ~a~%" (author ,userinfo) (email ,userinfo) (last-ip ,userinfo)))
     ,@body))
