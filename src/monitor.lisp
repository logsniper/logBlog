(in-package :logsniper.logBlog)

(defmacro log-monitor (format-string &rest format-arguments)
  `(with-open-file (stream #P"./log/monitor.log" :direction :output :if-exists :append :if-does-not-exist :create)
    (format stream (concatenate 'string "[~a]" ,format-string "~%") (timestamp-to-string (get-universal-time)) ,@format-arguments)))

(defun refresh-user-hash ()
  (let ((cur-time (get-universal-time))
        (cur-num 0)
        (inactive-user (make-hash-table)))
    (sb-thread:with-mutex (*active-user-hash-mutex*)
      (loop for k being the hash-keys in *active-user-hash* using (hash-value user-status)
              do (if (> (- cur-time (last-time user-status)) 60) ; no hearbeat for over 1 minute
                   (progn
                     (setf (gethash k inactive-user) (last-time user-status))
                     (remhash k *active-user-hash*))
                   (progn
                     (incf cur-num)
                     (if (> (request-in-this-minute user-status) *max-request-per-user-per-minute*)
                       (log-monitor "too many request last minute.[user:~a] [reqnum:~a]"
                                    k (request-in-this-minute user-status)))
                     (setf (request-in-this-minute user-status) 0)))))
    (loop for k being the hash-keys in inactive-user using (hash-value then-time)
          do (let ((userinfo (query-userinfo-by-email k)))
               (log-monitor "inactive-user:~a, last active time:~a" k (timestamp-to-string then-time))
               (if userinfo (setf (last-time userinfo) then-time))))
    (unless (= cur-num *active-user-num*)
      (log-monitor "active-user-num=~a" cur-num))
    (setf *active-user-num* cur-num)))

(defun output-active-user ()
  (sb-thread:with-mutex (*active-user-hash-mutex*)
    (loop for k being the hash-keys in *active-user-hash* using (hash-value user-status)
          do (log-monitor "active-user:~a, request-in-this-minute:~a" k (request-in-this-minute user-status)))))

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
    ;(terminate-zombie-thread) ;; not necessary
    (sleep 60)))
