(defun start-simple-server (port)
  "Listening on a port for a message, and print the received message."
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (usocket:wait-for-input socket)
    (usocket:with-connected-socket (connection (usocket:socket-accept socket))
      (loop for line = (read-line (usocket:socket-stream connection))
        do (format t "~a~%" (string-right-trim '(#\Return #\Newline) line))
        (if (equal (string-right-trim '(#\Return #\Newline) line) "quit") (return NIL))))))

