(in-package :logsniper.logBlog)

(defun login-response-json ()
  (with-cookie-user (cookie-userinfo)
    (with-output-to-string (stream)
      (let* ((password (hunchentoot:post-parameter "password"))
             (email (hunchentoot:post-parameter "email"))
             (userinfo (query-userinfo-by-email email)))
        (if (check-authentication userinfo password)
          (progn (update-user-info-cookie userinfo)
                 (format stream "{'status':'11'}"))
          (format stream "{'status':'12'}")))
      stream)))

(defun cancel-unread-message ()
  (with-cookie-user (cookie-userinfo)
    (if cookie-userinfo
      (with-output-to-string (stream)
        (let ((msgid (string-to-int (hunchentoot:get-parameter "msgid"))))
          (setf (new-reply cookie-userinfo)
                (remove-given-value-from-list (new-reply cookie-userinfo) msgid)))
        (format stream "{'unread_num': ~a}" (length (new-reply cookie-userinfo)))))))

(defun ajax-submit-message-response ()
  (with-cookie-user (userinfo)
    (let ((blog (get-blog (string-to-int (hunchentoot:post-parameter "blogid")))))
      (if blog
        (let* ((new-msg (add-message (hunchentoot:post-parameter "content")
                                     (hunchentoot:real-remote-addr)
                                     (blogid blog)))
               (replied-msgid (string-to-int (hunchentoot:post-parameter "rpmsg")))
               (replied-msg (get-message replied-msgid))) 
          (if new-msg
            (if (and replied-msgid replied-msg)
              (progn
                (push new-msg (repliers replied-msg))
                (push (msgid new-msg) (new-reply (query-userinfo-by-email (email replied-msg)))))
              (push new-msg (messages blog))))
          (log-info "[reply-info]isreply:~a,replied_author:~a" (and new-msg replied-msgid replied-msg) (if replied-msg (author replied-msg) nil))
          (recursively-decorate-message new-msg :depth (string-to-int (hunchentoot:post-parameter "hierarchy"))))))))

(defun mark-all-unread-messages ()
  (with-cookie-user (userinfo)
    (if userinfo
      (setf (new-reply userinfo) ()))
    "{'status': 'success'}"))
