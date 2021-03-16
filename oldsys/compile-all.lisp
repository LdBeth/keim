;; changed by EV: Add the USER package as nickname to CL-USER to MCL:
;; we need this here, since test-keim.system
;; assumes the existence of USER.  

#+CCL-2 (defpackage "COMMON-LISP-USER" (:nicknames  cl-user user))
#+:lcl4.1 (pushnew "sbin41" lcl:*load-binary-pathname-types*
		   :test #'string=)
#+:allegro-v4.2 (setq excl:*fasl-default-type* "fasl42")
#+:allegro-v4.2 (setq system:*load-search-list*
		      (cons *default-pathname-defaults*
			    (cons #p(:type "fasl42")
				  (cdr system:*load-search-list*))))

(unless  (find-package "MK")
  (load "defsystem"))

(mk::oos 'test-keim :compile :force :new-source :verbose t)

