(defun hello ()
  (print "Hello echo server."))


(defun start-echo-server (port)
  "Listening on a port for a message, and print the received message."
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (usocket:wait-for-input socket)
    (usocket:with-connected-socket (connection (usocket:socket-accept socket))
      (setf server-stream (usocket:socket-stream connection))
      (loop for line = (read-line server-stream)
        do
        (progn
          (format server-stream "~a~%" line)
          (force-output server-stream))
        (if (equal (string-right-trim '(#\Return #\Newline) line) "quit") (return NIL))))))

