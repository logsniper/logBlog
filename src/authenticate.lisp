(in-package :logsniper.logBlog)

(defun query-userinfo-by-email (email)
  (if email
    (find-item email *user-pset* :key #'email :test #'equal)))

(defun check-authentication (userinfo password)
  (and userinfo (or (not (password userinfo))
                    (string= password (password userinfo)))))

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
  (if password (setf (password userinfo) (md5sum password))))

(defun update-user-info-cookie (userinfo)
  (let ((author-kv (hunchentoot:set-cookie "author" :value (author userinfo)))
        (email-kv (hunchentoot:set-cookie "email" :value (email userinfo))))
    (hunchentoot:set-cookie* author-kv)
    (hunchentoot:set-cookie* email-kv))
  (if (password userinfo)
    (let ((password-kv (hunchentoot:set-cookie "password" :value (password userinfo))))
      (hunchentoot:set-cookie* password-kv))))

(defun add-user (email author &optional (password nil))
  (insert-item (make-instance 'userinfo :userid (incf (user-count (get-items-counter))) :author author :email email :password (md5sum password)) 
               *user-pset*))

(defun update-user-info ()
  (let ((email (hunchentoot:post-parameter "email"))
        (author (hunchentoot:post-parameter "author"))
        (password (hunchentoot:cookie-in "password")))
    (if (and email author)
      (let ((userinfo (query-userinfo-by-email email)))
        (if (check-authentication userinfo password)
          (progn (update-user-info-db userinfo author password)
                 (update-user-info-cookie userinfo)
                 t)
          (if (not userinfo)
            (progn (add-user email author)
                   (update-user-info-cookie (query-userinfo-by-email email))
                   t)))))))

(defmacro with-cookie-user ((userinfo) &body body)
  `(let ((,userinfo (get-cookie-user-info)))
     (incf (pageview-count (get-items-counter)))
     (if ,userinfo (format t "user-info ~a ~a ~a~%" (author ,userinfo) (email ,userinfo) (last-ip ,userinfo)))
     ,@body))
