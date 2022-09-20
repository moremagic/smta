(in-package :cl-user)

(defpackage :smta
  (:use :common-lisp :asdf :sb-sys  #+sbcl :sb-bsd-sockets))

(in-package :smta)

(defsystem smta
  :description "simple smtp server library"
  :version "0.1"
  :author "wcp.sdf-eu.org"
  :license "GPL"
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "smta")))))
