(asdf/package:define-package quux-hunchentoot
  (:use :common-lisp
   :hunchentoot
   :alexandria
   :lparallel ;; for exchanging messages
   :optima ;; for matching inter-thread messages
   :classy) ;; for its >simple-fifo-queue<
  (:import-from
   :hunchentoot
   #:*default-max-accept-count*
   #:*default-max-thread-count*
   #:*hunchentoot-stream*
   #:acceptor-process
   #:decrement-taskmaster-accept-count
   #:increment-taskmaster-accept-count
   #:decrement-taskmaster-thread-count
   #:increment-taskmaster-thread-count
   #:handler-case*
   #:send-service-unavailable-reply)
  (:export
   #:make-channel #:channel-send #:channel-recv
   #:start-thread
   #:thread-pooling-taskmaster))
