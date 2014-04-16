(in-package :logsniper.logblog)

(defmacro add-slot-to-pclass ((slot-name pclass-name))
  (handler-bind ((UNBOUND-SLOT #'(lambda () (invoke-restart 'STORE-VALUE ()))))
    `(loop for elem in (get-instances-by-class ',pclass-name)
           do (,slot-name elem))))

(add-slot-to-pclass (new-reply userinfo))
