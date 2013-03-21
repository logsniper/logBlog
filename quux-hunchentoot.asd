(defsystem quux-hunchentoot
  :depends-on (:hunchentoot ;; we extend it -- we depend on hunchentoot 1.2.15 + patch
               ;; see https://github.com/edicl/hunchentoot/pull/52
               :alexandria ;; for various utilities
               #-asdf3 :asdf-driver ;; for various utilities
               :bordeaux-threads ;;
               :lil ;; for FIFO queues
               :chanl ;; for communication channels
               :optima) ;; for parsing messages sent over channels
  :components
  ((:file "pkgdcl")
   (:file "thread-pooling" :depends-on ("pkgdcl"))))
