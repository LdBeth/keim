;; changed by EV: Add the USER package as a nickname for CL-USER to MCL:
;; we need this here, since test-keim.system
;; assumes the existence of USER.  

#+CCL-2 (defpackage "COMMON-LISP-USER" (:nicknames  cl-user user))


(in-package :user)
#+:lcl4.1 (pushnew "sbin41" lcl:*load-binary-pathname-types*
		   :test #'string=)
#+:allegro-v4.2 (setq excl:*fasl-default-type* "fasl42")
#+:allegro-v4.2 (setq system:*load-search-list*
		      (cons *default-pathname-defaults*
			    (cons #p(:type "fasl42")
				  (cdr system:*load-search-list*))))
#-:mk-defsystem
(load "defsystem")
;;; CHANGE THIS VARIABLE !!!
(setq mk::*central-registry* (list "/home/omega/distrib/sys/"))
(defun load-sys (system-name)
  (mk:operate-on-system system-name :load :force :new-source-and-dependents))
(defun compile-sys (system-name)
  (mk:operate-on-system system-name :compile :force :all))
(format *terminal-io* "Use the functions LOAD-SYS and COMPILE-SYS to load
a system.  Example:
(load-sys 'keim)
(load-sys 'nd)
Use the functions EXDOCU-SYS and CHECK-SYS to extract LaTeX Documentation or
check the programming conventions in the system.")

