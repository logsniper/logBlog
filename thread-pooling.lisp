(in-package :quux-hunchentoot)

(defun make-channel ()
  "Create a communication channel between threads in our pool"
  (make-instance 'unbounded-channel))

;; This taskmaster takes threads out of a worker thread pool.
;; Workers have to register themselves whenever they are ready
;; to process one request, and register again when they are ready again.
;; Therefore, requests never have to pay for the cost of initializing
;; or cleaning up  whichever expensive state justifies having a thread pool
;; rather than just creating a new thread every time.
;;
(defclass thread-pooling-taskmaster (multi-threaded-taskmaster)
  ((master-lock
    ;; controls status, acceptor-process, and
    ;; all slots that are valid while there is or may be no dispatcher-process to process messages.
    :initform (bt:make-lock "taskmaster-master")
    :reader taskmaster-master-lock
    :documentation
    "Thread-unsafe operations without a clear owner use this lock")
   (status ;; must hold the master lock to change while stopped; owned by the dispatcher while running or stopping.
    :accessor taskmaster-status
    :initform :stopped
    :documentation
    "The status of the taskmaster: :STOPPED, :RUNNING, :STOPPING.")
   (dispatcher-process ;; must hold the master lock to change
    :accessor dispatcher-process
    :documentation
    "A process that dispatches connections to worker processes for handling,
     or withholds them when resources are missing.")
   (dispatcher-channel ;; receiving owned by the dispatcher. Anyone can send.
    :initarg :dispatcher-channel
    :initform (make-channel)
    :accessor dispatcher-channel)
   (available-workers ;; owned by the dispatcher.
    :documentation "A list of channels, each to speak to an available worker"
    :initarg :available-workers
    :initform (empty-fifo-queue)
    :accessor taskmaster-available-workers)
   (busy-workers ;; owned by the dispatcher.
    :documentation "A set of busy workers"
    :initarg :busy-workers
    :initform (make-hash-table :test 'equal)
    :accessor taskmaster-busy-workers)
   (pending-connections ;; owned by the dispatcher.
    :documentation "A list of pending connection socket"
    :initarg :pending-connections
    :initform (empty-fifo-queue)
    :accessor taskmaster-pending-connections)
   ;; Support for bounding the number of threads we'll create
   (max-thread-count ;; must only be modified while stopped.
    :type (or integer null)
    :initarg :max-thread-count
    :initform nil
    :reader taskmaster-max-thread-count
    :documentation 
    "The maximum number of request threads this taskmaster will simultaneously
     run before refusing or queueing new connections requests.  If the value
     is null, then there is no limit.")
   (thread-count ;; while running, owned by the dispatcher.
    :type integer
    :initform 0
    :accessor taskmaster-thread-count
    :documentation
    "The number of taskmaster processing threads currently running.")
   (max-accept-count ;; must only be modified while stopped.
    :type (or integer boolean)
    :initarg :max-accept-count
    :initform nil
    :reader taskmaster-max-accept-count
    :documentation
    "The maximum number of connections this taskmaster will accept
     before refusing new connections.  If supplied and an integer,
     this must be greater than MAX-THREAD-COUNT.
     The number of queued requests is the difference between MAX-ACCEPT-COUNT
     and MAX-THREAD-COUNT. If NIL, then behave as if it were MAX-THREAD-COUNT.
     If T, then keep accepting new connections until resources are exhausted (not recommended).")
   (accept-count ;; while running, owned by the dispatcher.
    :type integer
    :initform 0
    :accessor taskmaster-accept-count
    :documentation
    "The number of connection currently accepted by the taskmaster. These
    connections are not ensured to be processed, they may be waiting for an
    empty processing slot or rejected because the load is too heavy.")
   (worker-thread-name-format ;; must only be modified while stopped.
    :type (or string null)
    :initarg :worker-thread-name-format
    :initform "hunchentoot-worker-~A"
    :accessor taskmaster-worker-thread-name-format))
  (:default-initargs
   :max-thread-count *default-max-thread-count*
   :max-accept-count *default-max-accept-count*)
  (:documentation "A taskmaster that maintains a pool of worker threads
and a queue of accepted connections to be processed.

If MAX-THREAD-COUNT is null, a new thread will always be created
when all existing workers are busy.

If MAX-THREAD-COUNT is supplied, the number of worker threads is
limited to that.  Furthermore, if MAX-ACCEPT-COUNT is not supplied, an
HTTP 503 will be sent if the thread limit is exceeded.  Otherwise, if
MAX-ACCEPT-COUNT is supplied, it must be greater than MAX-THREAD-COUNT;
in this case, requests are accepted up to MAX-ACCEPT-COUNT, and only
then is HTTP 503 sent.

It is important to note that MAX-ACCEPT-COUNT and the HTTP 503 behavior
described above is racing with the acceptor listen backlog. If we are receiving
requests faster than threads can be spawned and 503 sent, the requests will be
silently rejected by the kernel.

In a load-balanced environment with multiple Hunchentoot servers, it's
reasonable to provide MAX-THREAD-COUNT but leave MAX-ACCEPT-COUNT null.
This will immediately result in HTTP 503 when one server is out of
resources, so the load balancer can try to find another server.

In an environment with a single Hunchentoot server, it's reasonable
to provide both MAX-THREAD-COUNT and a somewhat larger value for
MAX-ACCEPT-COUNT.  This will cause a server that's almost out of
resources to wait a bit; if the server is completely out of resources,
then the reply will be HTTP 503."))

(defmethod initialize-instance :after ((taskmaster thread-pooling-taskmaster) &rest init-args)
  "Ensure the if MAX-ACCEPT-COUNT is supplied, that it is greater than MAX-THREAD-COUNT."
  (declare (ignore init-args))
  (with-accessors ((max-accept-count taskmaster-max-accept-count)
                   (max-thread-count taskmaster-max-thread-count)) taskmaster
    (when max-accept-count
      (unless max-thread-count
        (parameter-error "MAX-THREAD-COUNT must be non-NIL if MAX-ACCEPT-COUNT is non-NIL (was ~D)"
                         max-accept-count))
      (unless (or (eql max-accept-count t) (> max-accept-count max-thread-count))
        (parameter-error "MAX-ACCEPT-COUNT must be greater than MAX-THREAD-COUNT, but ~D <= ~D"
                         max-accept-count max-thread-count)))))

(defmethod hunchentoot::decrement-taskmaster-accept-count ((taskmaster thread-pooling-taskmaster))
  ;; Compatibility function so we can reuse hunchentoot:send-service-unavailable-reply
  (values))

(defmethod shutdown ((taskmaster thread-pooling-taskmaster))
  ;; just wait until all workers are done, send them all a shutdown message, then die.
  (bt:with-lock-held ((taskmaster-master-lock taskmaster))
    (ecase (taskmaster-status taskmaster)
      ((:stopped :stopping)) ;; no active dispatcher to receive a message
      ((:running) (dispatcher-send taskmaster `(:shutdown) :blockp nil)))))

;; NB: by using the send and recv gf's, we provide a specialization point.
(defgeneric dispatcher-send (taskmaster message &key &allow-other-keys)
  (:method ((taskmaster thread-pooling-taskmaster) message &rest keys &key &allow-other-keys)
    (apply 'send (dispatcher-channel taskmaster) message keys)))

(defgeneric dispatcher-recv (taskmaster &key &allow-other-keys)
  (:method ((taskmaster thread-pooling-taskmaster) &rest keys &key &allow-other-keys)
    (apply 'recv (dispatcher-channel taskmaster) keys)))

(defmethod execute-acceptor ((taskmaster thread-pooling-taskmaster))
  (bt:with-lock-held ((taskmaster-master-lock taskmaster))
    (when (eq (taskmaster-status taskmaster) :stopped)
      (setf (taskmaster-status taskmaster) :running)
      (setf (acceptor-process taskmaster)
            (start-thread
             taskmaster
             (lambda () (accept-connections (taskmaster-acceptor taskmaster)))
             :name (format nil "hunchentoot-listener-~A:~A"
                           (or (acceptor-address (taskmaster-acceptor taskmaster)) "*")
                           (acceptor-port (taskmaster-acceptor taskmaster)))))
      (setf (dispatcher-process taskmaster)
            (start-thread
             taskmaster
             (lambda () (run-dispatcher-thread taskmaster))
             :name (format nil "hunchentoot-dispatcher-~A:~A"
                           (or (acceptor-address (taskmaster-acceptor taskmaster)) "*")
                           (acceptor-port (taskmaster-acceptor taskmaster))))))))

(defmethod handle-incoming-connection ((taskmaster thread-pooling-taskmaster) connection)
  (dispatcher-send taskmaster `(:process-connection ,connection) :blockp nil))

(defun mark-worker-ready (taskmaster worker-id chan)
  ;; POST: the worker has been removed from the busy-workers and pushed onto the available-workers
  (when (gethash worker-id (taskmaster-busy-workers taskmaster))
    (remhash worker-id (taskmaster-busy-workers taskmaster))
    (decf (taskmaster-accept-count taskmaster)))
  (enqueue (taskmaster-available-workers taskmaster) `(:worker ,worker-id ,chan))
  (values))

(defun mark-worker-busy (taskmaster worker-id connection &optional chan)
  ;; PRE: the worker has been removed from the available-workers
  ;; POST: the worker is added to the busy-workers
  (setf (gethash worker-id (taskmaster-busy-workers taskmaster)) (list connection chan))
  (values))

(defun get-worker-busy-on-connection (taskmaster worker-id channel connection)
  ;; POST: the worker is added to the busy-workers
  (send channel `(:process-connection ,connection) :blockp nil)
  (mark-worker-busy taskmaster worker-id connection channel))

(defmethod too-many-taskmaster-requests ((taskmaster thread-pooling-taskmaster) connection)
  (acceptor-log-message (taskmaster-acceptor taskmaster)
                        :warning "Can't handle a new request, too many request threads already"))

(defgeneric run-worker-thread (taskmaster worker-id channel))
(defmethod run-worker-thread ((taskmaster thread-pooling-taskmaster) worker-id channel)
  ;; NB: define :before methods to initialize any application-specific worker thread state
  (loop for request = (recv channel :blockp t)
        do (ematch request
             ((list :process-connection connection)
              (process-connection (taskmaster-acceptor taskmaster) connection))
             ((list :shutdown)
              (return)))
           (dispatcher-send taskmaster `(:worker-ready ,worker-id ,channel) :blockp nil)))

(defgeneric next-worker-id (taskmaster))
(defmethod next-worker-id ((taskmaster thread-pooling-taskmaster))
  (incf (taskmaster-thread-count taskmaster)))

(defgeneric create-worker-thread (taskmaster))

(defmethod create-worker-thread ((taskmaster thread-pooling-taskmaster))
  "Create a worker thread for handling requests"
  ;; we are handling all conditions here as we want to make sure that
  ;; the acceptor process never crashes while trying to create a
  ;; worker thread; one such problem exists in
  ;; GET-PEER-ADDRESS-AND-PORT which can signal socket conditions on
  ;; some platforms in certain situations.
  (let ((worker-id (next-worker-id taskmaster))
        (channel (make-channel)))
    (handler-case*
     (start-thread
      taskmaster
      (lambda () (run-worker-thread taskmaster worker-id channel))
      :name (format nil (taskmaster-worker-thread-name-format taskmaster) worker-id))
     (error (condition)
       (let ((*acceptor* (taskmaster-acceptor taskmaster)))
         (log-message* *lisp-errors-log-level*
                       "Error while creating worker thread: ~A" condition))))
    (values worker-id channel)))

(defgeneric run-dispatcher-thread (taskmaster))

(defmethod run-dispatcher-thread ((taskmaster thread-pooling-taskmaster))
  ;; Here's the idea, with the stipulations given in THREAD-POOLING-TASKMASTER
  ;;  - If MAX-THREAD-COUNT is null, just start a taskmaster
  ;;  - If the connection count will exceed MAX-ACCEPT-COUNT or if MAX-ACCEPT-COUNT
  ;;    is null and the connection count will exceed MAX-THREAD-COUNT,
  ;;    return an HTTP 503 error to the client
  ;;  - Otherwise if we're between MAX-THREAD-COUNT and MAX-ACCEPT-COUNT,
  ;;    wait until the connection count drops, then handle the request
  ;;  - Otherwise, increment THREAD-COUNT and start a taskmaster
  (with-accessors ((master-lock taskmaster-master-lock)
                   (dispatcher-process dispatcher-process)
                   (taskmaster-status taskmaster-status)
                   (pending-connections taskmaster-pending-connections)
                   (available-workers taskmaster-available-workers)
                   (busy-workers taskmaster-busy-workers)
                   (accept-count taskmaster-accept-count)
                   (thread-count taskmaster-thread-count)
                   (max-accept-count taskmaster-max-accept-count)
                   (max-thread-count taskmaster-max-thread-count))
      taskmaster
    (bt:with-lock-held (master-lock)
      (assert (eq taskmaster-status :running))
      (assert (eq dispatcher-process (bt:current-thread))))
    (loop
      (ematch (dispatcher-recv taskmaster :blockp t)
        ((list :worker-ready worker-id chan)
         (mark-worker-ready taskmaster worker-id chan))
        ((list :process-connection connection)
         (enqueue pending-connections connection)
         (incf accept-count))
        ((list :shutdown)
         ;; NB: hunchentoot is supposed to stop the acceptor before the taskmaster
         (setf taskmaster-status :stopping)))
      (ecase taskmaster-status
        (:stopping
         (dolist (worker (dequeue-all available-workers))
           (ematch worker
             ((list :worker worker-id channel)
              (declare (ignorable worker-id))
              (send channel '(:shutdown) :blockp nil))))
         (dolist (connection (dequeue-all pending-connections))
           (too-many-taskmaster-requests taskmaster connection)
           (send-service-unavailable-reply taskmaster connection))
         (when (empty-p busy-workers)
           (bt:with-lock-held ((taskmaster-master-lock taskmaster))
             (setf taskmaster-status :stopped)
             (setf (dispatcher-process taskmaster) nil))
           (return)))
        (:running
         (loop
           (cond
             ;; nothing to do? wait for more work!
             ((empty-p pending-connections)
              (return))
             ;; worker available? give him the job!
             ((not (empty-p available-workers))
              (ematch (dequeue available-workers)
                ((list :worker worker-id channel)
                 (get-worker-busy-on-connection
                  taskmaster worker-id channel (dequeue pending-connections)))))
             ;; positions available for more workers? hire a new one for the job!
             ((or (null max-thread-count) (< thread-count max-thread-count))
              (multiple-value-bind (worker-id channel)
                  (create-worker-thread taskmaster)
                (get-worker-busy-on-connection
                 taskmaster worker-id channel (dequeue pending-connections))))
             ;; Already trying to handle too many connections? Deny request with 503.
             ((etypecase max-accept-count
                (integer
                 (> accept-count max-accept-count))
                (null
                 (>= thread-count max-thread-count))
                ((eql t)
                  nil))
              (let ((connection (dequeue pending-connections)))
                (too-many-taskmaster-requests taskmaster connection)
                (send-service-unavailable-reply taskmaster connection)))
             ;; More connections than we are ready to process, but fewer than we are ready to accept?
             ;; Wait for some worker to become ready.
             ((and max-accept-count (>= thread-count max-thread-count))
              (return))
             (t
              (error "WTF?")))))))))
