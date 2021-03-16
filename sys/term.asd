(in-package #:asdf-user)

(defsystem "term"
  :depends-on ("base-keim")
  :pathname "../keim/prog/term"
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
