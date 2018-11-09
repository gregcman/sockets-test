(asdf:defsystem #:sockets-test
  :depends-on (#:usocket)
  :serial t
  :components 
  ((:file "sockets-test")
))
