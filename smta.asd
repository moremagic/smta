(defsystem smta
  :description "simple smtp server library"
  :version "0.1.0"
  :author "moremagic"
  :license "GPL"
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "smta")
                 (:file "echo")))))
