(in-package #:asdf-user)

(defsystem "nd"
  :depends-on ("problem")
  :pathname "../keim/prog/nd"
  :serial t
  :components
  ((:file "nd")
   (:file "rule")))
