(defsystem quux-hunchentoot
  :depends-on ((:version :hunchentoot "1.2.17")
               :alexandria ;; for various utilities
               #-asdf3 :uiop ;; for various utilities
               :bordeaux-threads ;; for threads
               :lil ;; for FIFO queues
               :lparallel ;; for communication channels
               :optima) ;; for parsing messages sent over channels
  :components
  ((:file "pkgdcl")
   (:file "thread-pooling" :depends-on ("pkgdcl"))))
