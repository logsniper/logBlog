(in-package :logsniper.logBlog)

(defparameter *template-path* #P"./tmpl/")

(defparameter *image-path* #P"./images/")

(defparameter *create-and-edit-label* "^/Create_Edit$")

(defparameter *host-address* "127.0.0.1:8080")

(defparameter *max-paragraph-num* 20)

(defparameter *cookie-effective-period* (* 3600 24 7))
