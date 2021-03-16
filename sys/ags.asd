(in-package #:asdf-user)

(defsystem "ags"
  :serial t
  :pathname "../ags/prog"
  :components
  ((:module "mod"
    :pathname ""
    :components
    ((:file "modules")))
   (:module "doc"
    :pathname ""
    :components ((:file "doc")
                 (:file "docl")
                 (:file "doca")
                 ; (:file "man")
                 ))
   (:module "sys"
    :pathname ""
    :components ((:file "sys")))
   (:module "pp"
    :pathname ""
    :components
    ((:file "pprint")))))
