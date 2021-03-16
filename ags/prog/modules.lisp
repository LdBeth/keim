;;; -*- syntax: common-lisp; package: ags; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :ags)
#+comment
(mod~defmod mod :uses nil 
	    :documentation "Things that deal with the module structure."
	    :exports 
	    (mod~defmod 
	     mod~load-module 
	     mod~find-module
	     mod~compile-module
	     mod+module))

(export '(mod~defmod mod~load-module mod~find-module mod~compile-module mod+module
		     ))

#{
\section{The \keim\ module facility}
\label{mod:mod}
 The functions in this collection provide a means for creating and maintaining 
 modules.
 This file only requires that the AGS package has been defined, including
the defsystem code.  Each module stores as a slot the defsystem component
to which it corresponds, so that we can operate directly on the modules
without going over the systems.
\subsection{Class definition}
#}

(eval-when (load eval compile)
(defclass mod+module ()
  ((name :reader mod=module-name :initarg :name
	 :documentation "A symbol which names this module.")
   (uses :accessor mod=module-uses :initarg :uses
	 :documentation "Names of modules that this module uses.")
   (documentation :accessor mod=module-documentation
		  :initarg :documentation
		  :initform ""
		  :documentation "What the purpose of this module is.")
   (exports :accessor mod=module-exports
	    :initarg :exports
	    :initform nil
	    :documentation "Symbols defined in this module that should be exported from this package.")
   (component :accessor mod=component 
	      :documentation "This is the defsystem component that corresponds
to this module."))
  (:documentation "A module is a chunk of lisp code, which may use the functions defined by the other modules listed in its USES property, and exports the 
 indicated symbols from its Lisp package."))

(defmethod print-object ((mod mod+module) stream)
  (format stream "#<MOD+MODULE ~S>" (mod=module-name mod))
)

(defvar mod*loaded-modules nil "A list of all current-loaded modules")
)

#{
\subsection{Defining a module}
A {\vb MOD~DEFMOD} form should be placed at the beginning of each module file.
#}

(eval-when (load eval compile)
(defmacro mod~defmod (name &key (uses nil) (documentation "") (exports nil))
  (declare (edited  "07-AUG-1992 14:41")
	   (authors NESMITH)
	   (input   "A module name (symbol).  Following keyword arguments are"
                    "recognized: :uses, a list of modules which this module uses; :documentation,"
                    "a string which describes this module; :exports, symbols defined in this module"
                    "that should be exported from the current lisp package.  A warning is issued"
                    "if a module in the :uses list has not been loaded, and whenever a module is"
                    "redefined."
                    "Note: Arguments are not evaluated, so quoting is not necessary."
                    "Usage:"
                    "(mod~defmod foo :uses (bar baz) :documentation \"The foo module.\""
                    "                                :exports (foo~bim foo~bam))")
	   (effect  "Defines a new module, and exports the indicated symbols.")
	   (value   "The newly defined module."))
 `(prog1
    (make-instance 'mod+module :name ',name :uses ',uses
                 :documentation ',documentation
                 :exports ',exports)
    (export ',exports *package*)))
)





(defmethod initialize-instance :after ((new-mod mod+module) &key)
  ; (declare (special mk:*current-component*))
  (let* ((new-mod
	  (let* ((pre-mod
		  (mod~find-module (mod=module-name new-mod))))
	    (when pre-mod
	      (warn "MOD~~DEFMOD: Redefining module ~S." (mod=module-name new-mod))
	      (setf (mod=module-uses pre-mod) (mod=module-uses new-mod))
	      (setf (mod=module-documentation pre-mod) 
		(mod=module-documentation new-mod))
	      (setf (mod=module-exports pre-mod) 
		(mod=module-exports new-mod))
	      (setq new-mod pre-mod))
	    new-mod)))
    ; (setf (mod=component new-mod) mk:*current-component*)
    (pushnew new-mod mod*loaded-modules)
    (dolist (mod (mod=module-uses new-mod))
      (unless (mod~find-module mod)
	(warn "MOD~~DEFMOD: Module ~S wants to use ~S, but ~
               it hasn't been loaded." 
	      (mod=module-name new-mod) 
	      (symbol-name mod))))))


#| These can be used for any file, but of course if each module is a file,
then they are usable for modules as well. |#

(defvar mod*compile-if-source-newer nil)

#{
\subsection{Accessing and using modules}
#}

(eval-when (load eval compile)
(defun mod~find-module (module-name)
  (declare (edited  "7-JUL-1993 03:06")
	   (authors NESMITH)
	   (input   "A string or symbol with the name of an already-loaded 
module.")
	   (effect  "none")
	   (value   "The module which has this name, or NIL if none exists."))
  (find module-name mod*loaded-modules
	:test #'string-equal 
	:key #'mod=module-name))


(mod~defmod mod :uses nil 
	    :documentation "Things that deal with the module structure."
	    :exports 
	    (mod~defmod 
	     mod~load-module 
	     mod~find-module
	     mod~compile-module
	     mod+module))
)

#+comment
(defun mod~load-module (module-name 
			&key (compile-if-source-newer 
				 mod*compile-if-source-newer)
			     (always-compile nil)
			     (source-only nil)
			     (verbose *load-verbose*))
  (declare (edited  "10-JUN-1992 03:06")
	   (authors NESMITH)
	   (input   "A string or symbol with the name of an already-loaded 
module.  If SOURCE-ONLY is non-nil, then will only load the
source file; if COMPILE-IF-SOURCE-NEWER is non-nil (defaults to value of
mod*compile-if-source-newer), will first compile the source file if it is
newer than the corresponding compiled file; 
if ALWAYS-COMPILE is non-nil, will always compile the source file before
loading.  These flags are used in the order: SOURCE-ONLY, ALWAYS-COMPILE and
COMPILE-IF-SOURCE-NEWER.  If all of these are nil, then the newest of the
source and compiled file will be loaded.  VERBOSE will be used as the value 
of *load-verbose* during the load process.")
	   (effect  "Finds file where MODULE-NAME module was defined, loads it, perhaps compiling it first, depending on keyword arguments.")
	   (value   "Whatever the value of load returns, or signals error if
the file cannot be found."))
  (let ((mod (mod~find-module module-name)))
    (cond ((null mod)
	   (error "Module ~S not defined." module-name))
	  ((null (mod=component mod))
	   (error "Module ~S loaded from unknown location." module-name))
	  (t
	   (let ((mk::*load-source-instead-of-binary* source-only)
		 (mk::*minimal-load* nil)
		 (mk::*oos-verbose* verbose))
	   (mk::operate-on-component 
	    (mod=component mod) 
	    :load 
	    (cond (always-compile :all)
		  (t :new-source))
	    ))))))

#+comment
(defun mod~compile-module (module-name &key (verbose *load-verbose*))
  (declare (edited  "10-JUN-1992 03:06")
	   (authors NESMITH)
	   (input   "A string or symbol which is the name of an already-loaded
module.  Keyword argument VERBOSE will be used as the value 
of *load-verbose* during the load process.")
	   (effect  "Finds the file where MODULE-NAME was defined, compiles and loads it.")
	   (value   "Whatever the value of load returns, or signals error if
the file cannot be found."))
  (let ((mod (mod~find-module module-name)))
    (cond ((null mod)
	   (error "Module ~S not defined." module-name))
	  ((null (mod=component mod))
	   (error "Module ~S loaded from unknown location." module-name))
	  (t
	   (let ((mk::*oos-verbose* verbose))
	     (mk::operate-on-component (mod=component mod) :compile :all))
	   ))))




