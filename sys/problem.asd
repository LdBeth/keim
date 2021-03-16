(in-package #:asdf-user)

(defsystem "problem"
  :depends-on ("term")
  :pathname "../keim/prog/problem"
  :components
  ((:file "justification")
   (:file "node" :depends-on ("justification"))
   (:file "assum")
   (:file "conc")
   (:file "problem" :depends-on ("node" "conc" "assum"))
   (:file "proof" :depends-on ("problem"))))
