(in-package #:asdf-user)

(defsystem "uni"
  :depends-on ("term")
  :pathname "../keim/prog/uni"
  :components
  ((:file "unif")))
