
(load "./src/package.lisp")
(load "./conf/conf.lisp")
(load "./src/util.lisp")
(load "./src/db_def.lisp")
(load "./src/authenticate.lisp")
(load "./src/db_util.lisp")
(load "./src/page_generator.lisp")
(load "./src/ajax_process.lisp")
(load "./src/hunchentoot_extension.lisp")

(in-package :logsniper.logBlog)

(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor 
                                      :port 8080
                                      :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                                                                 :max-thread-count *max-thread-count*
                                                                 :max-accept-count *max-accept-count*)))
(setf (hunchentoot:acceptor-message-log-destination acceptor) *message-log-path*)
(setf (hunchentoot:acceptor-access-log-destination acceptor) *access-log-path*)
(setf hunchentoot:*acceptor* acceptor)

(load "./src/monitor.lisp")

;(open-store *store-spec* :thread t)
;(load "./src/update_pclass_tool.lisp")
(hunchentoot:start acceptor)
