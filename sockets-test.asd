(asdf:defsystem #:sockets-test
  :depends-on (#:usocket
	       #:flexi-streams
	       #:lparallel)
  :serial t
  :components 
  ((:file "sockets-test")
))
