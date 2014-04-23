(in-package :logsniper.logBlog)

#|
(defmethod hunchentoot:initialize-connection-stream ((acceptor hunchentoot:easy-acceptor) stream)
  (log-info "initialize-connection-stream:~a" sb-thread:*current-thread*)
  (open-store *store-spec*)
  stream)

(defmethod hunchentoot:reset-connection-stream ((acceptor hunchentoot:easy-acceptor) stream)
  (log-info "reset-connection-stream:~a" sb-thread:*current-thread*)
  (close-store)
  (open-store *store-spec*)
  stream)
|#

; 解决"Error 5 / database is locked"以及数据库断开或打开过多的file description等问题
(defmethod hunchentoot:handle-request ((acceptor hunchentoot:easy-acceptor) request)
  (with-open-store (*store-spec*)
    ;(with-transaction (:store-controller *store-controller*)
      (call-next-method)))
