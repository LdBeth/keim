(in-package #:asdf-user)

(defsystem "base-keim"
  :depends-on ("ags")
  :pathname "../keim/prog/base"
  :serial t
  :components
  ((:file "package")
   (:file "keim")
   (:file "help")
   (:file "env")
   (:file "post")
   (:file "position")))
