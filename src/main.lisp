
(load "./src/package.lisp")
(load "./conf/conf.lisp")
(load "./src/util.lisp")
(load "./src/db_def.lisp")
(load "./src/authenticate.lisp")
(load "./src/db_util.lisp")
(load "./src/page_generator.lisp")
(load "./src/ajax_process.lisp")
(load "./src/hunchentoot_extension.lisp")

(load "./quux_hunchentoot/pkgdcl.lisp")
(load "./quux_hunchentoot/thread-pooling.lisp")

(in-package :logsniper.logBlog)

(defparameter multi-thread-taskmaster (make-instance 
                                        'quux-hunchentoot:thread-pooling-taskmaster 
                                        ;'hunchentoot:one-thread-per-connection-taskmaster
                                        :max-thread-count *max-thread-count* :max-accept-count *max-accept-count*))

(defparameter single-thread-taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster))

(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor 
                                      :port 8080 :taskmaster (if *single-thread-taskmaster*
                                                               single-thread-taskmaster
                                                               multi-thread-taskmaster)))

(setf (hunchentoot:acceptor-message-log-destination acceptor) *message-log-path*)
(setf (hunchentoot:acceptor-access-log-destination acceptor) *access-log-path*)
(setf hunchentoot:*acceptor* acceptor)

(load "./src/monitor.lisp")

(open-store *store-spec* :thread t)
;(load "./src/update_pclass_tool.lisp")
(hunchentoot:start acceptor)
