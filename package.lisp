;;(asdf:load-system :usocket)
(require :usocket)

(in-package :cl-user)

(defpackage :smta
  (:use :common-lisp :asdf  #+sbcl :sb-bsd-sockets)
  (:export :run-program
           :start-smtp-server
           :start-spooler
           :test
           :smtp-repl
           :print-spool-status
           :start-echo-server))

(in-package :smta)

