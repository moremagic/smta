(asdf:load-system :usocket)

(in-package :cl-user)

(defpackage :smta
  (:use :common-lisp :asdf  #+sbcl :sb-bsd-sockets)
  (:export :run-program
           :start-smtp-server
           :start-spooler
           :test
           :smtp-repl
           :print-spool-status
           :start-simple-server))

(in-package :smta)

