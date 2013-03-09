(defsystem quux-hunchentoot
  :depends-on (:hunchentoot ;; we extend it
               :alexandria ;; for various utilities
               #-asdf3 :asdf-driver ;; for various utilities
               :bordeaux-threads ;;
               :lil ;; for FIFO queues
               :chanl ;; for communication channels
               :optima) ;; for parsing messages sent over channels
  :components
  ((:file "pkgdcl")
   (:file "thread-pooling" :depends-on ("pkgdcl"))))
