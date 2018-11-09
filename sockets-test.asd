(asdf:defsystem #:sockets-test
  :depends-on (#:usocket
	       #:flexi-streams
	       #:lparallel
	       #:bordeaux-threads
	       #:trivial-utf-8)
  :serial t
  :components 
  ((:file "sockets-test")
))
