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
(defmethod hunchentoot:handle-request ((acceptor hunchentoot:easy-acceptor) request)
  (with-open-store (*store-spec*) ;; "with-open-store" will lead to opening too many unused controller.
    (with-transaction (:store-controller *store-controller*) ;; "with-transaction" wiil make the logout fail.
      (call-next-method))))
||#
