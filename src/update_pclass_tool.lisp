(in-package :logsniper.logblog)

(defmacro add-slot-to-pclass ((slot-name pclass-name default-value))
  (handler-bind ((UNBOUND-SLOT #'(lambda () (invoke-restart 'STORE-VALUE ,default-value))))
    `(loop for elem in (get-instances-by-class ',pclass-name)
           do (,slot-name elem))))

(with-open-store (*store-spec*)
  (add-slot-to-pclass (new-reply userinfo)))
