(asdf/package:define-package quux-hunchentoot
  (:use :common-lisp
   :hunchentoot
   :alexandria
   :chanl ;; for exchanging messages
   :optima ;; for matching inter-thread messages
   :classy) ;; for its >simple-fifo-queue<
  (:import-from
   :hunchentoot
   #:*default-max-accept-count*
   #:*default-max-thread-count*
   #:*hunchentoot-stream*
   #:acceptor-process
   #:handler-case*
   #:send-service-unavailable-reply)
  (:export
   #:start-thread
   #:thread-pooling-taskmaster))
