(in-package :logsniper.logBlog)

(defun timestamp-to-string (timestamp)
  (with-output-to-string (stream)
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (format stream "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (GMT~@d)"
              hour minute second month date year tz)
      stream)))

(defun string-to-int (str)
  (if str
    (handler-case
      (parse-integer str)
      (SB-INT:SIMPLE-PARSE-ERROR (spe) -1))
    -1))

(defun string-to-symbol (str)
  (handler-case
    (multiple-value-bind (ret-val other)
      (intern (string-upcase str))
      ret-val)
    (TYPE-ERROR (te) 'XXX)))
