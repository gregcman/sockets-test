(defpackage #:sockets-test
  (:use #:cl)
  (:export
   #:logger))
(in-package :sockets-test)

#+nil
(defun test ()
  (usocket:with))

; This is working version of a simple TCP echo server, inspired by
; https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
; https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc
; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
;
; TCP server example - https://gist.github.com/traut/6bf71d0da54493e6f22eb3d00671f2a
; UDP server example https://gist.github.com/traut/648dc0d7b22fdfeae6771a5a4a19f877


(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging"
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))

(defun run-server (&optional
		     (type (or 'udp
			       'tcp))
		     (host "0.0.0.0")
		     (port (or 8882
			       8881)))
  (case type
    ((udp) (udp-server::run-udp-server host port))
    ((tcp) (tcp-server::run-tcp-server host port))))

(defpackage :tcp-server
  (:use :cl :sockets-test))
(in-package :tcp-server)

; To connect to a running server, run
;
;   $ telnet 127.0.0.1 8881
(defun send-text-to-socket (text socket)
  (let ((socket-stream (usocket:socket-stream socket)))
    (format
     socket-stream
     (format nil "~a~%" text))  ; adding a line break at the end for prettiness
    (force-output socket-stream)))


(defun close-socket (socket)
  "Close a socket without raising an error if something goes wrong"
  (handler-case
      (usocket:socket-close socket)
    (error (e)
      (logger "ignoring the error that happened while trying to close socket: ~a" e)))
  (logger "socket closed"))


(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  ;; NOTE: read-line blocks until end-of-line character is received
  ;; see http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
  ;; for read-byte-at-a-time solution
  (let ((message (read-line (usocket:socket-stream client-socket))))
    (logger "got a message: ~a" message)
    (send-text-to-socket message client-socket)))


(defun run-tcp-server (host port)
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket.
  All client sockets are kept in all-sockets list."
  (let* ((master-socket (usocket:socket-listen host port :backlog 256))
         (all-sockets `(,master-socket)))
    (loop
       (loop for sock in (usocket:wait-for-input all-sockets :ready-only t)
	  do (if (eq sock master-socket)
					; new connection initiated
                 (let ((client-socket
			(usocket:socket-accept master-socket :element-type 'character)))
                   (push client-socket all-sockets)
                   (logger "new socket initiated: ~a" client-socket))
					; client socket activity
                 (handler-case
		     (process-client-socket sock)
                   (t (e)
		     (logger "error during processing ~a" e)
		     (setf all-sockets (delete sock all-sockets))
		     (close-socket sock))))))))

(defpackage :udp-server
  (:use :cl :sockets-test))
(in-package :udp-server)

; To connect to a running server, run
;
;   $ nc -u localhost 8882
(defvar +max-buffer-size+ 32768)


(defun send-text-to-socket (text socket remote-host remote-port)
  (let* ((message (format nil "~a~%" text))  ; adding a line break at the end for prettiness
         (buffer (flexi-streams:string-to-octets message :external-format :utf-8)))
    (usocket:socket-send
     socket
     buffer
     (length message)
     :host remote-host
     :port remote-port)))


(defun trim (str)
  (string-trim '(#\return #\space #\linefeed) str))


(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  (let ((buffer (make-array +max-buffer-size+
                            :element-type '(unsigned-byte 8)
                            :fill-pointer t)))
    (multiple-value-bind (recv size remote-host remote-port)
	(usocket:socket-receive client-socket buffer nil)
      (declare (ignore recv)) ; it's the same buffer val, so we can ignore it
      (logger "new data from ~a:~a / ~a on socket ~a" remote-host remote-port size client-socket)
      (if (plusp size)
					; converting buffer to string
	  (let* ((message (flexi-streams:octets-to-string buffer :external-format :utf-8 :end size))
		 (trimmed-message (trim message)))
	    (logger "got a message: ~a" trimmed-message)
	    (send-text-to-socket trimmed-message client-socket remote-host remote-port))
	  (logger "no data received on udp socket: ~d" size)))))


(defun run-udp-server (host port)
  "Run UDP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket"
  (let ((socket (usocket:socket-connect nil nil
                                        :protocol :datagram
                                        :local-host host
                                        :local-port port
                                        :element-type '(unsigned-byte 8))))
    (loop
       (loop for sock in (usocket:wait-for-input `(,socket) :ready-only t)
	  do (process-client-socket sock)))))
