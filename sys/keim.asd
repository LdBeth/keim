(in-package #:asdf-user)

(defsystem "keim"
  :depends-on ("base-keim")
  :pathname "../keim/prog"
  :components
  ((:module "term"
    :components
    ((:file "type")
     (:file "position")
     (:file "symbol" :depends-on ("type"))
     (:file "term-basic" :depends-on ("position" "symbol" "type"))
     (:file "sym" :depends-on ("symbol" "term-basic" "type"))
     (:file "appl" :depends-on ("term-basic" "type"))
     (:file "abstr" :depends-on ("symbol" "term-basic" "type"))
     (:file "sksym" :depends-on ("symbol" "term-basic" "type"))
     (:file "term-comp"
      :depends-on ("abstr" "appl" "symbol" "sym" "term-basic" "type"))
     (:file "term-mixin" :depends-on ("term-basic"))
     (:file "poly" :depends-on ("term-basic" "symbol" "type"))
     (:file "meta" :depends-on ("type" "term-basic" "poly" "symbol"))
     (:file "fo" :depends-on ("term-basic" "type"))
     (:file "top" :depends-on ("term-basic" "type" "position"))
     (:file "ho-op" :depends-on ("term-basic" "type" "top" "position"))
     (:file "subst" :depends-on ("type" "term-basic" "top"))))

   (:module "problem"
    :depends-on ("term")
    :components
    ((:file "justification")
     (:file "node" :depends-on ("justification"))
     (:file "assum")
     (:file "conc")
     (:file "problem" :depends-on ("node" "conc" "assum"))
     (:file "proof" :depends-on ("problem"))))
   (:module "uni"
    :depends-on ("term")
    :components
    ((:file "unif")))

   (:module "resolution"
    :depends-on ("problem" "uni")
    :serial t
    :components
    ((:file "literal")
     (:file "clause")
     (:file "set")
     (:file "delta")
     (:file "resolution")
     (:file "simpl")
     (:file "cnf")))
   (:module "inter"
    :components
    ((:file "argtypes")
     (:file "argdefs" :depends-on ("argtypes"))
     (:file "interface" :depends-on ("argtypes"))
     (:file "ascii-interface" :depends-on ("interface" "argtypes"))
     (:file "command" :depends-on ("interface" "argtypes"))))
   (:module "nd"
    :depends-on ("problem")
    :serial t
    :components
    ((:file "nd")
     (:file "rule")))))
