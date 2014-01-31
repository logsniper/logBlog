(in-package :logsniper.logBlog)

(defun query-userinfo-by-email (email)
  (if email
    (find-item email *user-pset* :key #'email :test #'equal)))

(defun check-authentication (userinfo password)
  (and userinfo (or (equal password (password userinfo))
                    (not (password userinfo)))))

(defun get-cookie-user-info ()
  (let ((email (hunchentoot:cookie-in "email"))
        (password (hunchentoot:cookie-in "password")))
      (let ((userinfo (query-userinfo-by-email email)))
        (if (check-authentication userinfo password)
          (progn
            (setf (last-time userinfo) (get-universal-time))
            (setf (last-ip userinfo) (hunchentoot:real-remote-addr))
            userinfo)))))

(defun get-post-user-info ()
  (let ((email (hunchentoot:post-parameter "email")))
    (if email
      (query-userinfo-by-email email))))

(defun update-user-info-db (userinfo author password)
  (if author (setf (author userinfo) author))
  (if password (setf (password userinfo) (md5sum password)))
  t)

(defun update-user-info-cookie (userinfo)
  (let ((author-kv (hunchentoot:set-cookie "author" :value (author userinfo)))
        (email-kv (hunchentoot:set-cookie "email" :value (email userinfo))))
    (hunchentoot:set-cookie* author-kv)
    (hunchentoot:set-cookie* email-kv))
  (if (password userinfo)
    (let ((password-kv (hunchentoot:set-cookie "password" :value (password userinfo))))
      (hunchentoot:set-cookie* password-kv))))

(defun add-user (email author &optional (password nil))
  (insert-item (make-instance 'userinfo :author author :email email :password (md5sum password)) 
               *user-pset*))

(defun update-user-info ()
  (let ((email (hunchentoot:post-parameter "email"))
        (author (hunchentoot:post-parameter "author"))
        (password (hunchentoot:post-parameter "password")))
    (if (and email author)
      (let ((userinfo (query-userinfo-by-email email)))
        (if (check-authentication userinfo password)
          (and (update-user-info-db userinfo author password)
               (update-user-info-cookie userinfo))
          (if (not userinfo)
            (and (add-user email author password)
                 (update-user-info-cookie (query-userinfo-by-email email)))))))))

(defmacro with-cookie-user ((userinfo) &body body)
  `(let ((,userinfo (get-cookie-user-info)))
     (if ,userinfo (format t "user-info ~a ~a ~a~%" (author ,userinfo) (email ,userinfo) (last-ip ,userinfo)))
     ,@body))
