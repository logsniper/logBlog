(asdf/package:define-package quux-hunchentoot
  (:use :common-lisp
   :hunchentoot
   :alexandria :bordeaux-threads :optima
   :classy) ;; for its >simple-fifo-queue<
  (:import-from
   :hunchentoot
   #:*default-max-accept-count*
   #:*default-max-thread-count*
   #:*hunchentoot-stream*
   #:handler-case*
   #:send-service-unavailable-reply
   ))
