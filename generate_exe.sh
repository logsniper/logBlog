echo "
(defun main () (load \"src/main.lisp\"))
(sb-ext:save-lisp-and-die \"logBlog\" :toplevel #'main :executable t)
" | sbcl
