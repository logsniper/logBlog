(in-package :logsniper.logBlog)

(setq html-template:*default-template-pathname* *template-path*)
(setq html-template:*string-modifier* #'identity)

(defun decorate-paragraph (para)
  (with-output-to-string (stream)
    (case (para-type para)
      (ptype-head (format stream "<h2>~a</h2>~%" (content para)))
      (ptype-body (format stream "<p>~a</p>~%" (content para)))
      (ptype-image (format stream "<img src=\"~a\"/>~%" (content para)))
      (t (log-warning "[decorate-paragraph]failed to judge para-type:~a." (string (para-type para)))))
    stream))

(defun merge-paragraphs (blog)
  (let ((body ""))
    (loop for para in (body blog)
          do (setq body (concatenate 'string body (decorate-paragraph para))))
    body))

(defun generate-navigator-page ()
  (with-cookie-user (cookie-userinfo)
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
        #P"./navigator.tmpl"
        (list :username (if cookie-userinfo
                          (author cookie-userinfo)
                          nil))
      :stream stream))))

(defun generate-recent-messages ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./message.tmpl"
      (list :host *host-address*
            :message-posts
            (loop for i from 1 to 5
                  for message-post in (nreverse (get-instances-by-range 'message-post 'timestamp nil nil))
                  collect (list :author (author message-post)
                                :owner-blogid (owner-blogid message-post)
                                :content (content message-post))))
      :stream stream)))

(defun generate-sidebar ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./sidebar.tmpl"
      (list :host *host-address*
            :tags (loop for item in (summarise-blog-tags)
                        collect (list :tag (first item)
                                      :count (second item)))
            :recent-messages (generate-recent-messages))
      :stream stream)))

(defmacro def-generate-static-page (func-name file-name)
  `(defun ,func-name ()
     (with-cookie-user (userinfo)
        (with-output-to-string (stream)
          (html-template:fill-and-print-template
            ,file-name
            (list :navigator (generate-navigator-page)
                  :sidebar (generate-sidebar))
            :stream stream)))))

(def-generate-static-page generate-register-page #P"./register.tmpl")
(def-generate-static-page generate-login-page #P"./login.tmpl")
(def-generate-static-page generate-about-page #P"./about.tmpl")

(defun get-hintinfo-by-id (hintid)
  (case hintid
    (1 "Register success.")
    (2 "Register failed. Your Email has already existed.")
    (3 "Register failed. Name/Email/Password cannot be empty.")
    (11 "Login success.")
    (12 "Login failed. Your Email&Password combination is invalid.")
    (22 "Sorry, this blog cannot be accessed.")
    (31 "Logout success.")
    (-1 "Page not found.")
    (t "Unknown hint number. Please contact the blogger(logsniper@outlook.com)")))

(defun generate-register-response-page ()
  (with-cookie-user (cookie-userinfo)
    (let* ((password (hunchentoot:post-parameter "password"))
           (email (hunchentoot:post-parameter "email"))
           (author (hunchentoot:post-parameter "author"))
           (userinfo (query-userinfo-by-email email)))
      (if (none-of-them-is-empty author email password)
        (with-output-to-string (stream)
          (if (not userinfo)
            (progn (add-user email author password)
                   (update-user-info-cookie (query-userinfo-by-email email))
                   (hunchentoot:redirect "/hint?v=1" :host *host-address* :protocol :http :code 303))
            (if (not (password userinfo))
              (progn (update-user-info-db userinfo :author author :password password)
                     (update-user-info-cookie userinfo)
                     (hunchentoot:redirect "/hint?v=1" :host *host-address* :protocol :http :code 303))
              (hunchentoot:redirect "/hint?v=2" :host *host-address* :protocol :http :code 303))))
        (hunchentoot:redirect "/hint?v=3" :host *host-address* :protocol :http :code 303)))))

(defun generate-hint-response-page ()
  (with-cookie-user (userinfo)
    (let ((hintid (string-to-int (hunchentoot:get-parameter "v")))
          (refer-url (hunchentoot:referer)))
      (with-output-to-string (stream)
        (log-info "[hint]hintid:~a,refer:~a" hintid refer-url)
        (html-template:fill-and-print-template
          #P"./hint.tmpl"
          (list :navigator (generate-navigator-page)
                :sidebar (generate-sidebar)
                :hintinfo (get-hintinfo-by-id hintid)
                :refer-url refer-url
                :main-page *host-address*)
          :stream stream)))))

(defun generate-tags-linker (blog)
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./tags_linker.tmpl"
      (list :host *host-address*
            :tags (loop for tag in (tags blog)
                        collect (list :tag tag)))
      :stream stream)))

(defun generate-blog-list-page ()
  "Generate the index page on which lists all valid blog posts"
  (with-cookie-user (userinfo)
    (refresh-database-connection)
    (let ((need-tag (hunchentoot:get-parameter "tag")))
      (with-output-to-string (stream)
        (html-template:fill-and-print-template
          #P"./blog_list.tmpl"
          (list :navigator (generate-navigator-page)
                :blog-posts
                (nreverse
                  (loop for blog-post in (pset-list *blog-posts*)
                        when (blog-filter-with-tag blog-post need-tag)
                        collect (list :host-address *host-address*
                                      :title (title blog-post)
                                      :blogid (blogid blog-post)
                                      :time (timestamp-to-string (timestamp blog-post))
                                      :msg-num (msg-num blog-post)
                                      :visitor-count (visitor-count blog-post)
                                      :tags (generate-tags-linker blog-post))))
                :sidebar (generate-sidebar))
          :stream stream)))))

(defun generate-login-response-page ()
  (with-cookie-user (cookie-userinfo)
    (let* ((password (hunchentoot:post-parameter "password"))
           (email (hunchentoot:post-parameter "email"))
           (userinfo (query-userinfo-by-email email)))
      (if (check-authentication userinfo password)
        (progn (update-user-info-cookie userinfo)
               (hunchentoot:redirect "/hint?v=11" :host *host-address* :protocol :http :code 303))
        (hunchentoot:redirect "/hint?v=12" :host *host-address* :protocol :http :code 303)))))

(defun recursively-decorate-message (message &key (depth 1))
  (if message
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
        #P"./recursive_message.tmpl"
        (list :hierarchy (if (oddp depth) "odd" "even")
              :author (author message)
              :content (content message)
              :msgid (msgid message)
              :timestamp (timestamp-to-string (timestamp message))
              :host-address *host-address*
              :blogid (owner-blogid message)
              :reply-list (loop for rpmsg in (repliers message)
                                collect (list :sub-html (recursively-decorate-message rpmsg :depth (1+ depth)))))
        :stream stream))))

(defun recursive-messages-of-blog (blog)
  (if (and blog (messages blog))
    (with-output-to-string (stream)
      (loop for msg in (messages blog)
            do (format stream "~a~%" (recursively-decorate-message msg))))))

(defun answer-submit-message ()
  (with-cookie-user (userinfo)
    (let ((blog (get-blog (string-to-int (hunchentoot:get-parameter "blogid")))))
      (if blog
        (let* ((new-msg (add-message (hunchentoot:post-parameter "content")
                                     (hunchentoot:real-remote-addr)
                                     (blogid blog)))
               (replied-msgid (string-to-int (hunchentoot:post-parameter "rpmsg")))
               (replied-msg (get-message replied-msgid))) 
          (if new-msg
            (if (and replied-msgid replied-msg)
              (push new-msg (repliers replied-msg))
              (push new-msg (messages blog))))
          (log-info "[reply-info]isreply:~a,replied_author:~a" (and new-msg replied-msgid replied-msg) (if replied-msg (author replied-msg) nil))
          (hunchentoot:redirect (concatenate 'string "/view?nincf=1&blogid="
                                             (hunchentoot:get-parameter "blogid"))
                                :host *host-address* :protocol :http :code 303))))))

(defun generate-blog-view-page ()
  (with-cookie-user (userinfo)
  (let* ((blog (get-blog (string-to-int (hunchentoot:get-parameter "blogid"))))
        (force-view (string-to-int (hunchentoot:get-parameter "fv")))
        (replied-msgid (string-to-int (hunchentoot:get-parameter "rpmsg")))
        (not-incf-vc (string-to-int (hunchentoot:get-parameter "nincf")))
        (replied-msg (get-message replied-msgid)))
    (if (and blog (or (> force-view 0) (published blog)))
      (with-output-to-string (stream)
        (unless (and not-incf-vc (> not-incf-vc 0))
          (add-visitor-count blog))
        (html-template:fill-and-print-template
          #P"./view_blog.tmpl"
          (list :navigator (generate-navigator-page)
                :blogid (blogid blog)
                :title (title blog)
                :tags (generate-tags-linker blog)
                :timestamp (timestamp-to-string (timestamp blog))
                :last-modified-time (timestamp-to-string (last-modified-time blog))
                :visitor-count (visitor-count blog)
                :msg-num (msg-num blog)
                :body (merge-paragraphs blog)
                :non-cookie (not userinfo)
                :author (if userinfo (author userinfo) "")
                :email (if userinfo (email userinfo) "")
                :is-reply (and replied-msgid replied-msg)
                :replied-msgid replied-msgid
                :replied-author (if replied-msg (author replied-msg) "")
                :recursive-messages (recursive-messages-of-blog blog)
                :sidebar (generate-sidebar))
          :stream stream))
      (generate-hint-response-page)))))

(defun parse-blog-submitter (blog)
  (let ((blog-title (hunchentoot:post-parameter "title"))
        (blog-tags (trim-and-split (hunchentoot:post-parameter "tags")))
        (blog-status (hunchentoot:post-parameter "blog_status"))
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
        (setf (tags blog) blog-tags)
        (setf (body blog) (nreverse blog-body))
        (setf (published blog) (string= "published" blog-status))
        (setf (last-modified-time blog) (get-universal-time))
        (save-blog blog)))))

(defun generate-blog-editor-page ()
  (with-cookie-user (userinfo)
    (if (and userinfo (manager userinfo))
      (let ((blog (get-non-nil-blog (string-to-int (or (hunchentoot:get-parameter "blogid")
                                                       (hunchentoot:post-parameter "blogid"))))))
        (parse-blog-submitter blog)
        (with-output-to-string (stream)
          (log-info "[blog tags]blogid:~a,tags:~a" (blogid blog) (join-string-with-comma (tags blog)))
          (html-template:fill-and-print-template
            #P"./create_edit_blog.tmpl"
            (list :navigator (generate-navigator-page)
                  :title (title blog)
                  :published (published blog)
                  :timestamp (timestamp-to-string (timestamp blog))
                  :last-modified-time (timestamp-to-string (last-modified-time blog))
                  :tags (join-string-with-comma (tags blog))
                  :blogid (blogid blog)
                  :body-paragraph
                  (loop for idx from 0 to *max-paragraph-num*
                        for para in (body blog)
                        collect (list :sequence idx :content (content para)
                                      :headp (equal (para-type para) 'ptype-head)
                                      :bodyp (equal (para-type para) 'ptype-body)
                                      :imagep (equal (para-type para) 'ptype-image))))
            :stream stream)))
      (generate-hint-response-page))))

(defun generate-logout-page ()
  (with-cookie-user (userinfo)
    (logout userinfo)
    (hunchentoot:redirect "/hint?v=31" :host *host-address* :protocol :http :code 303)))

(setq hunchentoot:*dispatch-table*
      (list 
        (hunchentoot:create-static-file-dispatcher-and-handler "/style.css" #P"resources/style.css")
        (hunchentoot:create-static-file-dispatcher-and-handler "/script.js" #P"resources/script.js")
        (hunchentoot:create-static-file-dispatcher-and-handler "/favicon.ico" #P"resources/favicon.ico")
        (hunchentoot:create-folder-dispatcher-and-handler "/images/" *image-path*)
        (hunchentoot:create-regex-dispatcher "^/navigator$" 'generate-navigator-page)
        (hunchentoot:create-regex-dispatcher "^/view$" 'generate-blog-view-page)
        (hunchentoot:create-regex-dispatcher "^/submit_message$" 'answer-submit-message)
        (hunchentoot:create-regex-dispatcher "^/register$" 'generate-register-page)
        (hunchentoot:create-regex-dispatcher "^/reg_response$" 'generate-register-response-page)
        (hunchentoot:create-regex-dispatcher "^/login$" 'generate-login-page)
        (hunchentoot:create-regex-dispatcher "^/login_response$" 'generate-login-response-page)
        (hunchentoot:create-regex-dispatcher "^/logout$" 'generate-logout-page)
        (hunchentoot:create-regex-dispatcher "^/hint$" 'generate-hint-response-page)
        (hunchentoot:create-regex-dispatcher *create-and-edit-label* 'generate-blog-editor-page)
        (hunchentoot:create-regex-dispatcher "^/index$" 'generate-blog-list-page)
        (hunchentoot:create-regex-dispatcher "^/$" 'generate-blog-list-page)
        (hunchentoot:create-regex-dispatcher "^/about$" 'generate-about-page)
        (hunchentoot:create-prefix-dispatcher "/" 'generate-hint-response-page)))
