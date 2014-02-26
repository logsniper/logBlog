(in-package :logsniper.logBlog)

(defun ignore-unused-var (&rest rest)
  (concatenate 'list rest))

(defun timestamp-to-string (timestamp)
  (with-output-to-string (stream)
    (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time timestamp)
      (format stream "~2,'0d:~2,'0d:~2,'0d ~d/~2,'0d/~d (GMT~@d)"
              hour minute second month date year tz)
      (ignore-unused-var day-of-week dst-p)
      stream)))

(defun string-to-int (str)
  (if str
    (handler-case
      (parse-integer str)
      (SB-INT:SIMPLE-PARSE-ERROR () -1))
    -1))

(defun string-to-symbol (str)
  (handler-case
    (find-symbol (string-upcase str) :logsniper.logBlog)
    (TYPE-ERROR () 'XXX)))

(defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names collect `(,n (gensym)))
            ,@body))

(defun generate-salt ()
  (random 10000000))

(defun none-of-them-is-empty (&rest variables)
  (let ((flag t))
    (loop for var in variables
          do (if (or (not var) (and (typep var 'string) (string= var "")))
               (setf flag nil)))
    flag))

; functions about md5 below comes from "http://www.cnblogs.com/eyeit/archive/2011/08/12/2142044.html"
(defvar +letters+ "0123456789abcdef")
 
(defun octets->letters (octet-vector)
  (with-output-to-string (stream)
    (loop for i across octet-vector
          do (flet ((foo (x) (aref +letters+ (ldb (byte x (- x 4)) i))))
               (princ (foo 8) stream)
               (princ (foo 4) stream)))))

(defun md5sum (string)
  (and string
    (octets->letters #+sbcl (sb-md5:md5sum-string string)
                     #-sbcl (with-input-from-string (stream string)
                              (md5:md5sum-stream stream)))))
; md5 end

(defmacro def-log-macro (macro-name log-level)
  `(defmacro ,macro-name (format-string &rest format-arguments)
    `(hunchentoot:log-message* ,,log-level ,format-string ,@format-arguments)))
(def-log-macro log-error :error)
(def-log-macro log-warning :warning)
(def-log-macro log-info :info)

(defun split-string-by-char (str splitter)
  (let ((first-pos -1) (len (1- (length str))))
    (loop named str-visitor for i from 0 to len
          do (if (char= (char str i) splitter)
               (progn (setq first-pos i)
                      (return-from str-visitor i))))
    (if (= first-pos -1)
      (list str)
      (let ((left (subseq str 0 first-pos))
            (right (split-string-by-char (subseq str (1+ first-pos)) splitter)))
        (push left right)
        right))))

(defun trim-and-split (str)
  (let ((words (split-string-by-char str #\,)))
    (loop for word in words
          collect (string-trim " " word))))

(defun join-string-with-comma (str-list)
  (with-output-to-string (stream)
    (format stream "~{~a~^, ~}" str-list)
    stream))
