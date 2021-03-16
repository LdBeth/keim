(in-package #:asdf-user)

(defsystem "resolution"
  :depends-on ("problem" "uni")
  :pathname "../keim/prog/resolution"
  :serial t
  :components
  ((:file "literal")
   (:file "clause")
   (:file "set")
   (:file "delta")
   (:file "resolution")
   (:file "simpl")
   (:file "cnf")))
