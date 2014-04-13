
(load "./src/package.lisp")
(load "./src/conf.lisp")
(load "./src/util.lisp")
(load "./src/db_def.lisp")
(load "./src/authenticate.lisp")
(load "./src/db_util.lisp")
(load "./src/page_generator.lisp")

(in-package :logsniper.logBlog)

(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 8080))
(setf (hunchentoot:acceptor-message-log-destination acceptor) *message-log-path*)
(setf (hunchentoot:acceptor-access-log-destination acceptor) *access-log-path*)

(with-open-store (*store-spec*)
                 (hunchentoot:start acceptor))
