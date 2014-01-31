
(load "./src/package.lisp")
(load "./src/conf.lisp")
(load "./src/util.lisp")
(load "./src/db_def.lisp")
(load "./src/authenticate.lisp")
(load "./src/db_util.lisp")
(load "./src/page_generator.lisp")

(in-package :logsniper.logBlog)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
