(in-package :cl-user)

(defpackage :smta
  (:use :common-lisp #+sbcl :sb-bsd-sockets)
  (:export :run-program
           :start-smtp-server
           :start-spooler
           :test
           :smtp-repl
           :print-spool-status))

