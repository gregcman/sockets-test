(defpackage #:sockets-test
  (:use #:cl)
  (:export
   #:logger)
  (:export
   #:*host*
   #:*port*))
(in-package :sockets-test)

;; This is working version of a simple TCP echo server, inspired by
;; https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
;; https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc
;; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
;;
;; TCP server example - https://gist.github.com/traut/6bf71d0da54493e6f22eb3d00671f2a
;; UDP server example https://gist.github.com/traut/648dc0d7b22fdfeae6771a5a4a19f877

;;connect to tcp server:
;;   $ telnet 0.0.0.0 8882
;;connect to udp server:
;;   $ nc -u localhost 8882

(defparameter *host* "0.0.0.0")
(defparameter *port* 8882)

(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging"
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))

(defpackage :tcp-server
  (:use :cl :sockets-test))
(in-package :tcp-server)
(defun send-text-to-socket (text socket)
  (let ((socket-stream (usocket:socket-stream socket)))
    (dotimes (i (length text))
      (write-byte (char-code (aref text i))
		  socket-stream)) ; adding a line break at the end for prettiness
    (write-byte 10 socket-stream)
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
  (let ((data? (client-read client-socket)))
    (when data?
      (logger "finished getting a message: ~a" data?)
      (send-text-to-socket data? client-socket))))

(defun collect-input (socket buffer &optional (end-char 10 ;0
							))
  (let ((stream (usocket:socket-stream socket))
	byte)
    (block what
      (loop
	 (unless (listen stream)
	   (return-from what nil))
	 (setq byte (read-byte stream))
	 (when (= byte end-char)
	   (return-from what t))
	 (vector-push-extend byte buffer)))))

(defvar *socket-socket-object* (make-hash-table))
(defun what-socket-client-object (client-socket)
  (gethash client-socket *socket-socket-object*))
(defun (setf what-socket-client-object) (new client-socket)
  (setf (gethash client-socket *socket-socket-object*) new))
(defun remove-what-socket-client-object (client-socket)
  (remhash client-socket *socket-socket-object*))

(defun reset-buffer (buffer)
  (setf (fill-pointer buffer) 0))
(defun client-read (client-socket)
  (let ((buffer (what-socket-client-object client-socket)))
    (when (collect-input client-socket buffer)
      (prog1
	  (trivial-utf-8:utf-8-bytes-to-string buffer)
	(reset-buffer buffer)))))
		       
(defun run-tcp-server (&optional (host *host*) (port *port*))
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket.
  All client sockets are kept in all-sockets list."
  (clrhash *socket-socket-object*)
  (usocket:with-socket-listener (master-socket host port :backlog 256)
    (let ((all-sockets `(,master-socket)))
      (unwind-protect
	   (loop
	      (loop for sock in (usocket:wait-for-input all-sockets :ready-only t)
		 do (if (eq sock master-socket)
			;; new connection initiated
			(let ((client-socket
			       (usocket:socket-accept master-socket :element-type 'unsigned-byte)))
			  (push client-socket all-sockets)
			  (setf (what-socket-client-object client-socket)
				(make-array 0 :adjustable t
					    :fill-pointer 0
					    :element-type 'unsigned-byte))
			  (logger "new socket initiated: ~a" client-socket))
			;; client socket activity
			(handler-case
			    (process-client-socket sock)
			  (t (e)
			    (logger "error during processing ~a" e)
			    (setf all-sockets (delete sock all-sockets))
			    (remove-what-socket-client-object sock)
			    (close-socket sock))))))
	(clrhash *socket-socket-object*)
	(dolist (socket all-sockets)
	  (unless (eq socket master-socket)
	    (close-socket socket)))))))

(defpackage :udp-server
  (:use :cl :sockets-test))
(in-package :udp-server)

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
	  ;; converting buffer to string
	  (let* ((message (flexi-streams:octets-to-string buffer :external-format :utf-8 :end size))
		 (trimmed-message (trim message)))
	    (logger "got a message: ~a" trimmed-message)
	    (send-text-to-socket trimmed-message client-socket remote-host remote-port))
	  
	  (logger "no data received on udp socket: ~d" size)))))

(defun run-udp-server (&optional (host *host*) (port *port*))
  "Run UDP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket"
  (usocket:with-client-socket (socket nil nil nil
				      :protocol :datagram
				      :local-host host
				      :local-port port
				      :element-type '(unsigned-byte 8))
    (loop
       (loop for sock in (usocket:wait-for-input `(,socket) :ready-only t)
	  do (process-client-socket sock)))))
