(in-package :logsniper.logBlog)

(defparameter *template-path* #P"./tmpl/")

(defparameter *image-path* #P"./images/")

(defparameter *create-and-edit-label* "^/Create_Edit$")

(defparameter *host-address* "127.0.0.1:8080")

(defparameter *init-paragraph-num* 20)

(defparameter *max-paragraph-num* 100)

(defparameter *cookie-effective-period* (* 3600 24 7))

(defparameter *access-log-path* #P"./log/access.log")

(defparameter *message-log-path* #P"./log/message.log")

(defparameter *db-connection-refresh-frequency* 63)
