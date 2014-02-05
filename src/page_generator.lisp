(in-package :logsniper.logBlog)

(setq html-template:*default-template-pathname* *template-path*)
(setq html-template:*string-modifier* #'identity)

(defun decorate-paragraph (para)
  (with-output-to-string (stream)
    (case (para-type para)
      (ptype-head (format stream "<h2>~a</h2>~%" (content para)))
      (ptype-body (format stream "<div>~a</div>~%" (content para)))
      (ptype-image (format stream "<img src=\"~a\"/>~%" (content para)))
      (t (format t "failed to judge para-type:~a.~%" (string (para-type para)))))
    stream))

(defun merge-paragraphs (blog)
  (let ((body ""))
    (loop for para in (body blog)
          do (setq body (concatenate 'string body (decorate-paragraph para))))
    body))

(defmacro def-generate-static-page (func-name file-name)
  `(defun ,func-name ()
     (with-cookie-user (userinfo)
        (with-output-to-string (stream)
          (html-template:fill-and-print-template
            ,file-name
            ()
            :stream stream)))))

(def-generate-static-page generate-register-page #P"./register.tmpl")
(def-generate-static-page generate-login-page #P"./login.tmpl")
(def-generate-static-page generate-404-page #P"./404.tmpl")

(defun generate-register-response-page ()
  (with-cookie-user (cookie-userinfo)
    (let* ((password (hunchentoot:post-parameter "password"))
           (email (hunchentoot:post-parameter "email"))
           (author (hunchentoot:post-parameter "author"))
           (userinfo (get-user email)))
      (if (and author email password)
        (with-output-to-string (stream)
          (if (not userinfo)
            (progn (add-user email author password)
                   (update-user-info-cookie (get-user email))
                   (html-template:fill-and-print-template #P"./reg_response.tmpl"
                                                          (list :succ t)
                                                          :stream stream))
            (if (not (password userinfo))
              (progn (update-user-info-db userinfo author password)
                     (update-user-info-cookie userinfo)
                     (html-template:fill-and-print-template #P"./reg_response.tmpl"
                                                            (list :succ t)
                                                            :stream stream))
              (html-template:fill-and-print-template #P"./reg_response.tmpl"
                                                   (list :succ nil)
                                                   :stream stream))))
        (generate-404-page)))))

(defun generate-index-page ()
  "Generate the index page on which lists all the blog posts"
  (with-cookie-user (userinfo)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./index.tmpl"
      (list :blog-posts
            (loop for blog-post in (pset-list *blog-posts*)
                  collect (list :host-address *host-address*
                                :title (title blog-post) :blogid (blogid blog-post)
                                :body (merge-paragraphs blog-post))))
      :stream stream))))

(defun generate-login-response-page ()
  (with-cookie-user (cookie-userinfo)
    (let* ((password (hunchentoot:post-parameter "password"))
           (email (hunchentoot:post-parameter "email"))
           (userinfo (get-user email)))
      (if (and email password userinfo (string= (md5sum password) (password userinfo)))
        (progn (update-user-info-cookie userinfo)
               (generate-index-page))
        (generate-login-page)))))

(defun recursively-decorate-message (message)
  (if message
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
        #P"./recursive_message.tmpl"
        (list :author (author message)
              :content (content message)
              :msgid (msgid message)
              :timestamp (timestamp-to-string (timestamp message))
              :ip-addr (ip-addr message)
              :host-address *host-address*
              :blogid (owner-blogid message)
              :reply-list (loop for rpmsg in (repliers message)
                                collect (list :sub-html (recursively-decorate-message rpmsg))))
        :stream stream))))

(defun recursive-messages-of-blog (blog)
  (if (and blog (messages blog))
    (with-output-to-string (stream)
      (loop for msg in (messages blog)
            do (format stream "~a~%" (recursively-decorate-message msg))))))

(defun generate-blog-view-page ()
  (with-cookie-user (userinfo)
  (let* ((blog (get-blog (string-to-int (hunchentoot:get-parameter "blogid"))))
        (replied-msgid (string-to-int (hunchentoot:get-parameter "rpmsg")))
        (replied-msg (get-message replied-msgid)))
    (if blog
      (with-output-to-string (stream)
        (let* ((new-msg (add-message (hunchentoot:post-parameter "author")
                                    (hunchentoot:post-parameter "email")
                                    (hunchentoot:post-parameter "content")
                                    (hunchentoot:real-remote-addr)
                                    (blogid blog)))
               (replied-msgid (string-to-int (hunchentoot:post-parameter "rpmsg")))
               (replied-msg (get-message replied-msgid))) 
          (if new-msg
            (if (and replied-msgid replied-msg)
              (push new-msg (repliers replied-msg))
              (push new-msg (messages blog)))))
        (add-visitor-count blog)
        (format t "reply-info:~a,~a,~a~%" (and replied-msgid replied-msg) replied-msgid (if replied-msg (author replied-msg) ""))
        (html-template:fill-and-print-template
          #P"./view_blog.tmpl"
          (list :blogid (blogid blog)
                :title (title blog)
                :timestamp (timestamp-to-string (timestamp blog))
                :last-modified-time (timestamp-to-string (last-modified-time blog))
                :visitor-count (visitor-count blog)
                :body (merge-paragraphs blog)
                :author (if userinfo (author userinfo) "")
                :email (if userinfo (email userinfo) "")
                :is-reply (and replied-msgid replied-msg)
                :replied-msgid replied-msgid
                :replied-author (if replied-msg (author replied-msg) "")
                :recursive-messages (recursive-messages-of-blog blog)
                :messages (loop for message-post in (messages blog)
                                collect (list :host-address *host-address*
                                              :author (author message-post)
                                              :msgid (msgid message-post)
                                              :content (content message-post)
                                              :timestamp (timestamp-to-string (timestamp message-post))
                                              :ip-addr (ip-addr message-post))))
          :stream stream))
      (generate-404-page)))))

(defun generate-message-page ()
  (with-cookie-user (userinfo)
  (add-message (hunchentoot:post-parameter "author")
               (hunchentoot:post-parameter "email")
               (hunchentoot:post-parameter "content")
               (hunchentoot:real-remote-addr)
               nil)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./message.tmpl"
      (list :author (if userinfo (author userinfo) "")
            :email (if userinfo (email userinfo) "")
            :message-posts
            (loop for message-post in (nreverse (get-instances-by-range 'message-post 'timestamp nil nil))
                  collect (list :author (author message-post)
                                :msgid (msgid message-post)
                                :content (content message-post)
                                :timestamp (timestamp-to-string (timestamp message-post))
                                :ip-addr (ip-addr message-post))))
      :stream stream))))

(defun parse-blog-submitter (blog)
  (let ((blog-title (hunchentoot:post-parameter "title"))
        (blog-body (list)))
    (loop for idx from 0 to *max-paragraph-num*
          do (let ((ptext (hunchentoot:post-parameter (concatenate 'string "para_text_" (write-to-string idx))))
                   (ptype (hunchentoot:post-parameter (concatenate 'string "para_type_" (write-to-string idx)))))
                (if (and ptext (not (string= ptext "")))
                  (progn
                    (if (not ptype) (setq ptype "ptype-body"))
                    (push (make-instance 'blog-paragraph :content ptext :para-type (string-to-symbol ptype)) blog-body)))))
    (if (not (and 
               (or 
                 (not blog-title) 
                 (string= "" blog-title)) 
               (equal () blog-body)))
      (progn
        (setf (title blog) blog-title)
        (setf (body blog) (nreverse blog-body))
        (setf (last-modified-time blog) (get-universal-time))
        (save-blog blog)))))

(defun generate-blog-editor-page ()
  (with-cookie-user (userinfo)
  (let ((blog (get-non-nil-blog (string-to-int (or (hunchentoot:get-parameter "blogid")
                                                   (hunchentoot:post-parameter "blogid"))))))
    (parse-blog-submitter blog)
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
        #P"./create_edit_blog.tmpl"
        (list :title (title blog)
              :blogid (blogid blog)
              :body-paragraph
              (loop for idx from 0 to *max-paragraph-num*
                    for para in (body blog)
                    collect (list :sequence idx :content (content para)
                                  :headp (equal (para-type para) 'ptype-head)
                                  :bodyp (equal (para-type para) 'ptype-body)
                                  :imagep (equal (para-type para) 'ptype-image))))
        :stream stream)))))

(setq hunchentoot:*dispatch-table*
      (list 
        (hunchentoot:create-regex-dispatcher "^/message$" 'generate-message-page)
        (hunchentoot:create-regex-dispatcher "^/view$" 'generate-blog-view-page)
        (hunchentoot:create-regex-dispatcher "^/register$" 'generate-register-page)
        (hunchentoot:create-regex-dispatcher "^/reg_response$" 'generate-register-response-page)
        (hunchentoot:create-regex-dispatcher "^/login$" 'generate-login-page)
        (hunchentoot:create-regex-dispatcher "^/login_response$" 'generate-login-response-page)
        (hunchentoot:create-regex-dispatcher *create-and-edit-label* 'generate-blog-editor-page)
        (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
        (hunchentoot:create-prefix-dispatcher "/" 'generate-404-page)))
