(in-package :quux-hunchentoot)

;; This taskmaster takes threads out of a worker thread pool.
;; Workers have to register themselves whenever they are ready
;; to process one request, and register again when they are ready again.
;; Therefore, requests never have to pay for the cost of initializing
;; or cleaning up  whichever expensive state justifies having a thread pool
;; rather than just creating a new thread every time.
;;
(defclass thread-pooling-taskmaster (multi-threaded-taskmaster)
  ((master-lock
    ;; controls dispatcher-process, acceptor-process, and
    ;; all slots that are valid while there is or may be no dispatcher-process to process messages.
    :initform (bt:make-lock "taskmaster-master")
    :reader taskmaster-master-lock
    :documentation
    "Thread-unsafe operations without a clear owner use this lock")
   (dispatcher-process ;; must hold the master lock to change
    :accessor taskmaster-dispatcher-process
    :initform nil
    :documentation
    "A process that dispatches connections to worker processes for handling,
     or withholds them when resources are missing.")
   (dispatcher-channel ;; receiving owned by the dispatcher. Anyone can send.
    :initarg :dispatcher-channel
    :initform nil
    :accessor taskmaster-dispatcher-channel)
   (context ;; must only change before the kernel is started
    :accessor taskmaster-context
    :initarg :context
    :initform 'funcall
    :documentation
    "A context function, taking a thunk as argument, and calling it within proper context,
    for workers in the thread pool.")
   (bindings ;; must only change before the kernel is started
    :accessor taskmaster-bindings
    :initform nil
    :documentation
    "bindings (as an alist) to wrap around workers in the thread pool.")
   (thread-pool ;; must hold the master lock to change
    :initform nil
    :accessor taskmaster-thread-pool
    :documentation
    "A kernel to which to bind lparallel:*kernel* to handle the thread pool.")
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
    empty processing slot or rejected because the load is too heavy."))
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

(defun call-with-thread-pool (taskmaster thunk)
  (let ((*kernel* (taskmaster-thread-pool taskmaster)))
    (unless *kernel*
      (error "Thread pool not active"))
    (funcall thunk)))

(defmacro with-thread-pool ((taskmaster) &body body)
  `(call-with-thread-pool ,taskmaster #'(lambda () ,@body)))

(defmacro with-taskmaster-accessors (slots taskmaster &body body)
  `(with-accessors
         ,(loop for slot in slots
                 collect `(,slot ,(format-symbol :quux-hunchentoot "~A-~A" 'taskmaster slot)))
       ,taskmaster ,@body))

(defmethod initialize-instance :after ((taskmaster thread-pooling-taskmaster) &rest init-args)
  "Ensure the if MAX-ACCEPT-COUNT is supplied, that it is greater than MAX-THREAD-COUNT."
  (declare (ignore init-args))
  (with-taskmaster-accessors (max-accept-count max-thread-count) taskmaster
    (when max-accept-count
      (unless max-thread-count
        (parameter-error "MAX-THREAD-COUNT must be non-NIL if MAX-ACCEPT-COUNT is non-NIL (was ~D)"
                         max-accept-count))
      (unless (or (eql max-accept-count t) (> max-accept-count max-thread-count))
        (parameter-error "MAX-ACCEPT-COUNT must be greater than MAX-THREAD-COUNT, but ~D <= ~D"
                         max-accept-count max-thread-count)))))

(defgeneric ensure-dispatcher-process (taskmaster))

(defmethod ensure-dispatcher-process ((taskmaster thread-pooling-taskmaster))
  (with-taskmaster-accessors (dispatcher-process) taskmaster
    (assert (eq dispatcher-process (bt:current-thread)))))

(defmethod decrement-taskmaster-accept-count ((taskmaster thread-pooling-taskmaster))
  (ensure-dispatcher-process taskmaster)
  (decf (taskmaster-accept-count taskmaster)))
(defmethod increment-taskmaster-accept-count ((taskmaster thread-pooling-taskmaster))
  (ensure-dispatcher-process taskmaster)
  (incf (taskmaster-accept-count taskmaster)))
(defmethod decrement-taskmaster-thread-count ((taskmaster thread-pooling-taskmaster))
  (ensure-dispatcher-process taskmaster)
  (decf (taskmaster-thread-count taskmaster)))
(defmethod increment-taskmaster-thread-count ((taskmaster thread-pooling-taskmaster))
  (ensure-dispatcher-process taskmaster)
  (incf (taskmaster-thread-count taskmaster)))


(defmethod shutdown ((taskmaster thread-pooling-taskmaster))
  (with-taskmaster-accessors (master-lock dispatcher-process) taskmaster
    (bt:with-lock-held (master-lock)
      (when dispatcher-process
        (dispatcher-send taskmaster '(:shutdown))))))

(defgeneric dispatcher-send (taskmaster message &key &allow-other-keys)
  (:method ((taskmaster thread-pooling-taskmaster) message &key &allow-other-keys)
    (lparallel.queue:push-queue
     message
     (lparallel.kernel::channel-queue (taskmaster-dispatcher-channel taskmaster)))))

(defgeneric dispatcher-recv (taskmaster &key &allow-other-keys)
  (:method ((taskmaster thread-pooling-taskmaster) &key &allow-other-keys)
    (receive-result (taskmaster-dispatcher-channel taskmaster))))

(defmethod execute-acceptor ((taskmaster thread-pooling-taskmaster))
  (with-taskmaster-accessors
      (master-lock
       max-accept-count max-thread-count
       acceptor
       dispatcher-process dispatcher-channel
       context bindings thread-pool)
      taskmaster
    (bt:with-lock-held (master-lock)
      (let ((address (or (acceptor-address acceptor) "*"))
            (port (acceptor-port acceptor)))
        (assert (null thread-pool))
        (assert (null dispatcher-process))
        (assert (null dispatcher-channel))
        (setf thread-pool
              (make-kernel (or max-thread-count most-positive-fixnum)
                           :name (format nil "quux-hunchentoot-thread-pool-~A:~A" address port)
                           :context context :bindings bindings))
        (with-thread-pool (taskmaster)
          (setf dispatcher-channel (make-channel))
          (setf (acceptor-process taskmaster)
                (start-thread
                 taskmaster
                 (lambda () (accept-connections (taskmaster-acceptor taskmaster)))
                 :name (format nil "quux-hunchentoot-listener-~A:~A" address port)))
          (setf dispatcher-process
                (start-thread
                 taskmaster
                 (lambda () (run-dispatcher-thread taskmaster))
                 :name (format nil "quux-hunchentoot-dispatcher-~A:~A" address port))))))))


(defmethod handle-incoming-connection ((taskmaster thread-pooling-taskmaster) connection)
  (dispatcher-send taskmaster `(:process-connection ,connection) :blockp nil))

(defmethod too-many-taskmaster-requests ((taskmaster thread-pooling-taskmaster) connection)
  (declare (ignore connection))
  (acceptor-log-message (taskmaster-acceptor taskmaster)
                        :warning "Can't handle a new request, too many request threads already"))

(defun work-on-connection (taskmaster connection)
  `(:worker-done ,(process-connection (taskmaster-acceptor taskmaster) connection)))

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
  (with-taskmaster-accessors
      (master-lock thread-pool pending-connections
       dispatcher-channel dispatcher-process
       accept-count max-accept-count
       thread-count max-thread-count) taskmaster
    (ensure-dispatcher-process taskmaster)
    (with-thread-pool (taskmaster)
      (loop
        ;;; Process one message
        (ematch (dispatcher-recv taskmaster)
          ((list :worker-done _)
           (decrement-taskmaster-accept-count taskmaster)
           (decrement-taskmaster-thread-count taskmaster))
          ((list :process-connection connection)
           (enqueue pending-connections connection)
           (increment-taskmaster-accept-count taskmaster))
          ((list :shutdown)
           ;; NB: hunchentoot is supposed to stop the acceptor before the taskmaster
           (dolist (connection (dequeue-all pending-connections))
             (too-many-taskmaster-requests taskmaster connection)
             (send-service-unavailable-reply taskmaster connection))
           (end-kernel :wait t)
           (setf thread-pool nil
                 dispatcher-channel nil
                 dispatcher-process nil
                 (acceptor-process taskmaster) nil)))
        ;;; Do whatever work we can, while we can
        (loop
          (cond
            ;; nothing to do? wait for more work!
            ((empty-p pending-connections)
             (return))
            ;; thread available? put it on the job!
            ((or (null max-thread-count) (< thread-count max-thread-count))
             (increment-taskmaster-thread-count taskmaster)
             (submit-task dispatcher-channel
                          #'work-on-connection taskmaster (dequeue pending-connections)))
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
               ;; NB: the following decrements the accept-count
               (send-service-unavailable-reply taskmaster connection)))
            ;; More connections than we are ready to process, but fewer than we are ready to accept?
            ;; Wait for some worker to become ready.
            ((and max-accept-count (>= thread-count max-thread-count))
             (return))
            (t
             (error "WTF?"))))))))
