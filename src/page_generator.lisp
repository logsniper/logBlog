(in-package :logsniper.logBlog)

(setq html-template:*default-template-pathname* *template-path*)

(defun merge-paragraphs (blog)
  (let ((body ""))
    (loop for para in (body blog)
          do (setq body (concatenate 'string body (content para))))
    body))

(defun generate-index-page ()
  "Generate the index page on which lists all the blog posts"
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./index.tmpl"
      (list :blog-posts
            (loop for blog-post in (pset-list *blog-posts*)
                  collect (list :host-address *host-address*
                                :title (title blog-post) :blogid (blogid blog-post)
                                :body (merge-paragraphs blog-post))))
      :stream stream)))

(defun generate-blog-view-page ()
  (let ((blog (get-blog (string-to-int (hunchentoot:get-parameter "blogid")))))
    (if blog
      (with-output-to-string (stream)
        (html-template:fill-and-print-template
          #P"./view_blog.tmpl"
          (list :blogid (blogid blog)
                :title (title blog)
                :timestamp (timestamp-to-string (timestamp blog))
                :body (merge-paragraphs blog))
          :stream stream)))))

(defun generate-message-page ()
  (add-message (hunchentoot:post-parameter "author")
               (hunchentoot:post-parameter "content")
               (hunchentoot:real-remote-addr))
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./message.tmpl"
      (list :message-posts
            (loop for message-post in (nreverse (get-instances-by-range 'message-post 'timestamp nil nil))
                  collect (list :author (author message-post)
                                :content (content message-post)
                                :timestamp (timestamp-to-string (timestamp message-post))
                                :ip-addr (ip-addr message-post))))
      :stream stream)))

(defun parse-blog-submitter (blog)
  (let ((blog-title (hunchentoot:post-parameter "title"))
        (blog-body (list)))
    (loop for idx from 0 to 20
          do (let ((ptext (hunchentoot:post-parameter (concatenate 'string "para_text_" (write-to-string idx))))
                   (ptype (hunchentoot:post-parameter (concatenate 'string "para_type_" (write-to-string idx)))))
                (if (and ptext (not (string= ptext "")))
                  (progn
                    (if (not ptype) (setq ptype "body"))
                    (push (make-instance 'blog-paragraph :content ptext :para-type ptype) blog-body)))))
    (if (or blog-title blog-body)
      (progn
        (format t "saving blogid : ~a~%." (blogid blog))
        (setf (title blog) blog-title)
        (setf (body blog) (nreverse blog-body))
        ;(setf (blogid blog) (get-blogid))
        (save-blog blog)))))

(defun generate-blog-editor-page ()
  (let ((blog (get-non-nil-blog (string-to-int (or (hunchentoot:get-parameter "blogid")
                                                   (hunchentoot:post-parameter "blogid"))))))
    (parse-blog-submitter blog)
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
        #P"./create_edit_blog.tmpl"
        (list :title (title blog)
              :blogid (blogid blog)
              :body-paragraph
              (loop for idx from 0 to 20
                    for para in (body blog)
                    collect (list :sequence idx :content (content para))))
        :stream stream))))

(defun generate-404-page ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
      #P"./404.tmpl"
      ()
      :stream stream)))

(setq hunchentoot:*dispatch-table*
      (list 
        (hunchentoot:create-regex-dispatcher "^/message$" 'generate-message-page)
        (hunchentoot:create-regex-dispatcher "^/view$" 'generate-blog-view-page)
        (hunchentoot:create-regex-dispatcher *create-and-edit-label* 'generate-blog-editor-page)
        (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
        (hunchentoot:create-prefix-dispatcher "/" 'generate-404-page)))
