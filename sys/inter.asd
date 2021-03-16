(in-package #:asdf-user)

(defsystem "inter"
  :depends-on ("base-keim")
  :pathname "../keim/prog/inter"
  :components
  ((:file "argtypes")
   (:file "argdefs" :depends-on ("argtypes"))
   (:file "interface" :depends-on ("argtypes"))
   (:file "ascii-interface" :depends-on ("interface" "argtypes"))
   (:file "command" :depends-on ("interface" "argtypes"))))
