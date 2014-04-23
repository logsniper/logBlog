(in-package :logsniper.logBlog)

(defmacro log-monitor (format-string &rest format-arguments)
  `(with-open-file (stream #P"./log/monitor.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream (concatenate 'string "[~a]" ,format-string "~%") (timestamp-to-string (get-universal-time)) ,@format-arguments)))

(defun refresh-user-hash ()
  (let ((cur-time (get-universal-time))
        (cur-num 0))
    (sb-thread:with-mutex (*active-user-hash-mutex*)
      (loop for k being the hash-keys in *active-user-hash* using (hash-value then-time)
              do (if (> (- cur-time then-time) 60) ; no hearbeat for over 1 minute
                   (progn
                     (let ((userinfo (query-userinfo-by-email k)))
                       (if userinfo (setf (last-time userinfo) then-time)))
                     (remhash k *active-user-hash*))
                   (incf cur-num))))
    (unless (= cur-num *active-user-num*)
      (log-monitor "active-user-num=~a" cur-num))
    (setf *active-user-num* cur-num)))

(defun output-active-user ()
  (sb-thread:with-mutex (*active-user-hash-mutex*)
    (loop for k being the hash-keys in *active-user-hash* using (hash-value then-time)
          do (log-monitor "active-user:~a" k))))

(defun terminate-zombie-thread ()
  (loop for thread in (sb-thread:list-all-threads)
        do (with-output-to-string (stream)
               (format stream "~a" thread)
               (let ((str (get-output-stream-string stream)))
                 (if (or
                       (and (search "hunchentoot-worker-" str)
                            (search "waiting on" str))
                       (and (search "quux-hunchentoot-thread-pool-" str)
                            (search "waiting on" str)
                            (not (search "Anonymous condition variable" str))))
                   (progn (log-monitor "terminating thread:~a" thread)
                          (sb-thread:terminate-thread thread)))))))

(defun monitor-thread-function ()
  (sleep 10)
  (loop
    (refresh-user-hash)
    (terminate-zombie-thread)
    (sleep 60)))

(defparameter *monitor-thread* (make-instance 'sb-thread:thread))

(if (sb-thread:thread-alive-p *monitor-thread*)
  (sb-thread:terminate-thread *monitor-thread*))
(setf *monitor-thread* (sb-thread:make-thread 'monitor-thread-function :name "monitor"))
