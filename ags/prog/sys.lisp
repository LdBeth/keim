;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: AGS -*-
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
;; Changed by Espen Vestre 23/9-93 to MCL2.0 compatibility:
(in-package "AGS")

(mod~defmod sys :uses (mod)
	         :documentation "System- and Lisp-dependent definitions."
		 :exports (
			   sys~generic-function-methods
			   sys~generic-function-name
			   sys~generic-function-lambda-list
			   sys~method-specializers
			   sys~exit-from-lisp
			   sys+error
			   sys~define-condition
			   sys~make-condition
			   sys~signal
			   sys~handler-bind
			   sys~handler-case
			   sys+error
			   sys+warning
			   sys+abort
	                   sys~call-system
			   sys~print-specializers
			   sys~class-precedence-list
			   sys~dump-system
			   ))

#{
\section{System-dependent functions and error handling}
\label{mod:sys}
Some things we want to do are not yet standardized, or are not in
every Lisp.  This module is where such things belong, so that we
can keep the rest of \keim\ free from \#+ and \#-.  Most of the functions
in this manual are defined only for LCL4.0 (Lucid Common Lisp 4.0),
LCL4.1 (Lucid Common Lisp 4.1), ALLEGRO-V4.1 (Allegro Common Lisp 4.1),
ALLEGRO-V4.2 (Allegro Common Lisp 4.2), CCL-2 (Macintosh Common Lisp 2.0) and 
GENERA (actually Genera 8.0.2
running on a UX1200 board in a Sun4). 	To port \keim\ to other
setups, you'll have to add stuff below.
#}

#+cmu
(eval-when (load compile eval)
(when (string= "16e" (lisp-implementation-version))
  (pushnew :cmu16e *features*)))

#{\subsection{CLOS functions}				      
A few things are nice to have when working with CLOS.  We'd like to be 
able to inspect the methods on a generic function, to extract its name and
lambda list, and to get the specializers from a method.
#}
(defgeneric sys~generic-function-methods (fun)
  (declare (authors nesmith)
	   (input "A symbol or generic function.")
	   (value "If the input refers to a generic function, then a 
list of the function's methods will be returned.")
	   (effect "none"))
  (:method ((fun symbol))
      (if (fboundp fun)
	  (sys~generic-function-methods (ensure-generic-function fun))
	(error "~S does not represent a generic function." fun))))

#+(or lcl4.0 :lcl4.1)
(defmethod sys~generic-function-methods ((fun generic-function))
  (slot-value fun 'clos::methods))

#+genera
(defmethod sys~generic-function-methods ((fun generic-function))
  (slot-value fun 'clos-internals::methods))

#+(or allegro-v4.1 allegro-v4.2)
(defmethod sys~generic-function-methods ((fun generic-function))
  (clos:generic-function-methods fun))

#+ccl-2
(defmethod sys~generic-function-methods ((fun generic-function))
  (ccl:generic-function-methods fun))

#+:cmu17
(defmethod sys~generic-function-methods ((fun generic-function))
  (pcl:generic-function-methods fun))

#+:cmu16e
(defmethod sys~generic-function-methods ((fun pcl::generic-function))
  (pcl::generic-function-methods fun))

(defgeneric sys~generic-function-name (fun)
  (declare (authors nesmith)
	   (input "A symbol or generic function.")
	   (value "If the input refers to a generic function, then the
function's name will be returned.")
	   (effect "none"))
  (:method ((fun symbol))
      (if (fboundp fun)
	  (sys~generic-function-name (ensure-generic-function fun))
	(error "~S does not represent a generic function." fun))))

#+(or lcl4.0 :lcl4.1)
(defmethod sys~generic-function-name ((fun generic-function))
  (slot-value fun 'clos::name))

#+genera
(defmethod sys~generic-function-name ((fun generic-function))
  (slot-value fun 'clos-internals::name))

#+(or allegro-v4.1 allegro-v4.2)
(defmethod sys~generic-function-name ((fun generic-function))
  (clos:generic-function-name fun))

#+ccl-2
(defmethod sys~generic-function-name ((fun generic-function))
  (ccl:function-name fun))

#+:cmu17
(defmethod sys~generic-function-name ((fun generic-function))
  (pcl:generic-function-name fun))

#+:cmu16e
(defmethod sys~generic-function-name ((fun pcl::generic-function))
  (pcl::generic-function-name fun))

(defgeneric sys~generic-function-lambda-list (fun)
  (declare (authors nesmith)
	   (input "A symbol or generic function.")
	   (value "If the input refers to a generic function, then the
function's lambda-list will be returned.")
	   (effect "none"))
  (:method ((fun symbol))
      (if (fboundp fun)
	  (sys~generic-function-lambda-list (ensure-generic-function fun))
	(error "~S does not represent a generic function." fun))))

#+(or lcl4.0 :lcl4.1)
(defmethod sys~generic-function-lambda-list ((fun generic-function))
  (slot-value fun 'clos::lambda-list))

#+genera
(defmethod sys~generic-function-lambda-list ((fun generic-function))
  (slot-value fun 'clos-internals::lambda-list))

#+(or allegro-v4.1 allegro-v4.2)
(defmethod sys~generic-function-lambda-list ((fun generic-function))
  (clos:generic-function-lambda-list fun))

#+ccl-2
(defmethod sys~generic-function-lambda-list ((fun generic-function))
  (ccl:arglist fun))

#+:cmu17
(defmethod sys~generic-function-lambda-list ((fun generic-function))
  (pcl:generic-function-lambda-list fun))

#+:cmu16e
(defmethod sys~generic-function-lambda-list ((fun pcl::generic-function))
  (pcl::generic-function-lambda-list fun))

(defgeneric sys~method-specializers (method)
  (declare (authors nesmith)
	   (input "A method.")
	   (value "A list of the method's specializers will be returned.")
	   (effect "none")))
#+(or lcl4.0 :lcl4.1)
(defmethod sys~method-specializers ((method standard-method))
  (slot-value method 'clos::specializers))

#+genera
(defmethod sys~method-specializers ((method standard-method))
  (slot-value method 'clos-internals::specializers))

#+(or allegro-v4.1 allegro-v4.2)
(defmethod sys~method-specializers ((method standard-method))
  (clos:method-specializers method))

#+ccl-2
(defmethod sys~method-specializers ((method standard-method))
  (ccl:method-specializers method))

#+:cmu17
(defmethod sys~method-specializers ((method standard-method))
  (pcl:method-specializers method))

#+:cmu16e
(defmethod sys~method-specializers ((method pcl::standard-method))
  (pcl::method-specializers method))

(defgeneric sys~class-precedence-list (class)
  (declare (authors nesmith)
	   (input "A standard CLOS class.")
	   (value "The class's precedence list.")
	   (effect "none"))
  (:method (class)
    (declare (ignore class))
    nil))

#+(or lcl4.0 :lcl4.1)
(defmethod sys~class-precedence-list ((class class))
  (clos:class-precedence-list class))

#+(or allegro-v4.1 allegro-v4.2)
(defmethod sys~class-precedence-list ((class class))
  (clos:class-precedence-list class))

#+ccl-2
(defmethod sys~class-precedence-list ((class class))
  (ccl:class-precedence-list class))

#+genera
(defmethod sys~class-precedence-list ((class class))
  (clos:class-precedence-list class))

#+:cmu17
(defmethod sys~class-precedence-list ((class class))
  (pcl:class-precedence-list class))

#+:cmu16e
(defmethod sys~class-precedence-list ((class pcl::class))
  (pcl::class-precedence-list class))


(defun sys~print-specializers (function &optional (stream t))
  (declare (edited  "05-MAR-1993 14:47")
	   (authors RICHTS)
	   (input   "A generic function and a stream.")
	   (effect  "A table with the specializers of all methods defined for this generic function are printed on the STREAM.")
	   (value   "Undefined."))
  (let ((methods (sys~generic-function-methods function)))
    (format stream "~%The generic function ~A has ~A defined methods with the following specializers:"
	    (sys~generic-function-name function)
	    (length methods))
    (format stream "~%~{~1,22T~S ~}~%" 
	    (sys~generic-function-lambda-list function))
    (mapc #'(lambda (method)
	      (format stream "~%~{~3,22T~S ~}" 
		      (mapcar #'(lambda (specializer)
				  (if (typep specializer 'class)
				      (class-name specializer)
				    specializer))
			      (sys~method-specializers method))))
		  methods))
  nil)

#{\subsection{Operating system interface}				      
When we need to somehow do things outside of this lisp, here's how.
#}
(defun sys~exit-from-lisp ()
   (declare (edited  "9-Feb-1993")
	   (authors nesmith)
	   (input   "None")
	   (effect  "Exits from Lisp.")
	   (value "None."))
  #+(or lcl4.0 :lcl4.1) (lucid-common-lisp:quit)
  #+(or allegro-v4.1 allegro-v4.2) (excl:exit)
  #+:cmu17 (ext:quit)
  #+:cmu16e (ext:quit)
  #+ccl-2 (ccl:quit)
  #-(or lcl4.0 :lcl4.1 allegro-v4.1 allegro-v4.2 :cmu17 :cmu16e ccl-2)
  (error "Don't know how to exit from lisp.  Sorry.")
  )

(defun sys~call-system (string)
  (declare (authors nesmith)
	   (input "A string with a system command. On Genera system, a password
for the Unix host will be required.")
	   (effect "Runs the given command in a subordinate shell, whose
output will go to *standard-output*.")
	   (value "In Allegro or Lucid, an integer containing the exit status
of the subordinate shell (0 if no error occurred). Otherwise undefined."))
  #+(or allegro-v4.1 allegro-v4.2)
  (excl:run-shell-command string :wait t)
  #+genera
  (tcp::com-execute-command net:*emb-host* string)
  #+(or lcl4.0 :lcl4.1)
  (lucid-common-lisp:shell string)
  ;; noop version for MCL.  Will still do error below.
  #+ccl-2
  (ccl:message-dialog (format nil "KEIM wants to do this:~%~A" string))
  #+(or :cmu17 :cmu16e)
  (multiple-value-bind
    (pid csh-stream out-stream error-stream)
    (ext:run-program "/bin/csh" '("-f") 
		     :input :stream :wait nil :output t
		     :error :stream
		     )
    (declare (ignore pid out-stream))
    (format csh-stream "~A~%" string)
    (format csh-stream "exit~%")
    (close csh-stream)
    (close error-stream)
      )

  #-(or allegro-v4.1 allegro-v4.2 genera lcl4.0 :lcl4.1 :cmu17 :cmu16e)
  (error "Function SYS~~CALL-SYSTEM in SYS module undefined for this lisp.")
)

(defun sys~dump-system (system filename startup-function)
;;; The function DUMP-SYSTEM should be used to save a dump on disk.
  (declare (edited  "20-JUL-1993 13:53" )
	 (authors NESMITH )
	 (input   "A system name, a filename for the dump and a 
funcallable startup-function")
	 (effect  "The system is loaded, and a LISP image for KEIM is dumped.")
	 (value   "Undefined."))
  #+symbolics
  (error "Don't know how to dump Symbolics.")
  #-symbolics
  (progn
    (mk:operate-on-system system :load :verbose t)
  #+(or lcl4.0 :lcl4.1)
  (lcl:disksave filename
		  :restart-function startup-function
		  :full-gc t)
  #+(or allegro-v4.1 allegro-v4.2)
  (progn
    (setq excl:*cl-default-special-bindings*
	  (remove '*package*
		  (remove '*readtable* excl:*cl-default-special-bindings*
			  :key #'car)
		  :key #'car))
    (excl:gc t)
    (excl:dumplisp :name filename :checkpoint nil
		   :read-init-file nil
		   :restart-function startup-function))
  #+:cmu
  (progn (ext:gc)
	 (ext:save-lisp filename
			:init-function startup-function :print-herald nil)
	 )
  #+ccl-2
  (ccl:save-application filename :toplevel-function startup-function)
  ))



#{\section{Error handling}				      

 \commonlisp\ as it will be defined by the upcoming ANSI standard
 includes a comprehensive condition system, integrated in \clos.  We
 want to take advantage of those capabilities here.  At the same
 time, many older Lisp implementations do not have these features.
 Therefore, we'll try to define our own versions that, while
 limiting the full flexibility of the new \commonlisp, are backward
 compatible to older Lisp versions.  This will probably be replaced
 by full ANSI-compatible versions later, allowing all the features
 of the ANSI Lisps even in older pre-ANSI Lisps.

 NOTE: the implementation for Lisps which do not have a condition system
 based on \clos\ or on {\vb DEFSTRUCT} is incomplete.  {\vb SYS~HANDLER-CASE}
 does
 not handle :no-error clauses correctly and various restart
 functions do not yet exist.

 Assume that we were to use the \clos\ version of the syntax. 
 That means we have
 throw away some stuff, or the older Lisp will not work. 
 For slot specifiers, we always use as an accessor the same symbol
 that would be generated by a defstruct. With-slots in the :report
 function should not be used, rather these accessors.
#}
;; first we set up features for certain Lisps
(eval-when (load compile eval)
  ;; :keim-clos-conditions version should be good for ANSI-compatible Lisps,
  ;; that is, those that have the condition system integrated with CLOS
  #+(or allegro-v4.1 allegro-v4.2 ccl-2)
  (pushnew :keim-clos-conditions *features*)
  ;; :keim-defstruct-conditions is for Lisps that conform to the Pitman 
  ;; specification, before it was decided to merge condition system into 
  ;; CLOS and uses defstruct to define conditions.
  #+(or lcl4.0 :lcl4.1 :cmu17 :cmu16e)
    (pushnew :keim-defstruct-conditions *features*)
  ;; the version of conditions that is in Genera 8.0.2 is almost unsalvageable
  #+genera (pushnew :keim-give-up-conditions *features*)
  )

#-(or :keim-clos-conditions :keim-defstruct-conditions) ; needed for defining condition system
(progn 
(defclass sys+error ()
  ((:report :initarg :report :allocation :class :accessor sys+error-report)))
(defclass sys+warning ()
  ((:report :initarg :report :allocation :class :accessor sys+warning-report)))
(defclass sys+abort ()
  ((:report :initarg :report :allocation :class :accessor sys+abort-report)))
)

(eval-when (load compile eval)
(defmacro sys~define-condition (name (&rest parents) 
				     slot-specifiers
				     report-function
				     &rest options)
  (declare 
   (authors nesmith)
   (value "The new condition class.")
   (input "A symbol NAME, a list of PARENTS (each a symbol),
SLOT-SPECIFIERS, a REPORT-FUNCTION and optional OPTIONS.
In pre-ANSI compatible Lisps, only the first PARENT will be used. 
Each PARENT should be an already-defined condition.
SLOT-SPECIFIERS should have the syntax:
( {slot-name | (slot-name initform)}* )
where slot-name is a symbol and initform is the default form to use when
initializing an instance.
REPORT-FUNCTION should be a lambda expression taking two arguments,
the first being a condition of this type and the second being a
stream; this function should be used to report the condition to the stream.")
   (effect "Define a new condition class with the specified slots.
It is assured that for each slot, an accessor with the name
NAME-SLOT-NAME (as with a defstruct) will be defined.  This accessor
can be used in the REPORT-FUNCTION.  OPTIONS that cannot be handled by
the Lisp's condition system will be ignored."))
  ;;
  #+:keim-clos-conditions
  (let ((new-slot-specifiers
	 (mapcar #'(lambda (slot)
		     (if (symbolp slot)
			 (list slot :accessor 
			       (intern (format nil "~A-~A" name slot))
			       :initarg (intern (symbol-name slot)
						(find-package "KEYWORD")))
			 (list (car slot) :accessor 
			       (intern (format nil "~A-~A" name (car slot)))
			       :initform (cadr slot)
			       :initarg (intern (symbol-name (car slot))
						(find-package "KEYWORD")))))
		 slot-specifiers)))
    `(define-condition ,name ,parents ,new-slot-specifiers 
      (:report ,report-function) ,@options)
    )
  ;;
  #+:keim-defstruct-conditions
  (let ((parent
	 (if (cdr parents)
	     (error "Can only inherit from one parent to preserve
backward compatibility to old condition system.")
	     (car parents))))
    `(define-condition ,name (,parent) ,slot-specifiers
      (:report ,report-function) ))
  ;;
  ;; if we have neither of the above, we define our own version.
  #-(or  :keim-clos-conditions :keim-defstruct-conditions)
  (let ((new-slot-specifiers
	 (mapcar #'(lambda (slot)
		     (if (symbolp slot)
			 (list slot :accessor 
			       (intern (format nil "~A-~A" name slot)))
			 (list (car slot) :accessor 
			       (intern (format nil "~A-~A" name (car slot)))
			       :initform (cadr slot))))
		 slot-specifiers))
	(new-name (gensym)))
    `(progn
      (let ((,new-name (defclass ,name (,@parents sys+error) ,slot-specifiers ,@options)))
	(defmethod print-object ((condition ,name) stream)
	  (if *print-escape*
	      (call-next-method)
	      (funcall #',report-function condition stream)))
	,new-name)))
  )
)

(defmacro sys~make-condition (type &rest slot-initializations)
  (declare 
   (authors nesmith)
   (input "A condition TYPE, and slot initializations, which are
keyword pairs.")
   (value "Analogous to make-condition.  An instance of the condition type with the given slots initialized."))
  #+(or :keim-clos-conditions :keim-defstruct-conditions)
  `(make-condition ,type ,@slot-initializations)
  #-(or  :keim-clos-conditions :keim-defstruct-conditions)
  `(make-instance ,type ,@slot-initializations)
  )


(defmacro sys~signal (condition)
  (declare 
   (authors nesmith)
   (input "A condition.")
   (effect "Analogous to signal.  Signals the condition argument."))
   #+(or :keim-clos-conditions :keim-defstruct-conditions)
  `(signal ,condition)
   #-(or  :keim-clos-conditions :keim-defstruct-conditions)
    (let ((foo (gensym)))
    `(let ((,foo ,condition))
      (throw (if (symbolp ,foo) ,foo (class-of ,foo)) ,foo)))
    )

#-(or  :keim-clos-conditions :keim-defstruct-conditions)
(eval-when (load compile eval)
  (defmacro sys=expand-handler-bind ((tag fun) &body body)
    (let ((result-var (gensym))
	  (result-var2 (gensym))
	  (special-value (gensym)))
      `(let* ((,result-var ',special-value)
	      (,result-var2 
	       (catch ',tag
		 (setq ,result-var 
		       (multiple-value-list (progn ,@body))))))
	(if (eq ,result-var ',special-value)
	    (funcall ,fun ,result-var2)
	    (values-list ,result-var))))))

(eval-when (load compile eval)
(defmacro sys~handler-bind ((&rest defs) &body body)
  (declare 
   (authors nesmith)
   (input "Same as ANSI {\\vb HANDLER-BIND}.")
   (effect "Same as ANSI {\\vb HANDLER-BIND}.")
   (value "Same as ANSI {\\vb HANDLER-BIND}."))
  #+(or :keim-clos-conditions :keim-defstruct-conditions)
  `(handler-bind ,defs ,@body)
  #-(or  :keim-clos-conditions :keim-defstruct-conditions)
   (let ((first-def (car defs)))
     (if (null first-def)
	 `(progn ,@body)
	 (let ((xxx `(sys~handler-bind ,(cdr defs) ,@body)))
	   `(sys=expand-handler-bind ,first-def ,xxx))))
  )
)

(defmacro sys~handler-case (form &body body)
  (declare 
   (authors nesmith)
   (input "Same as ANSI {\\vb HANDLER-CASE}.")
   (effect "Same as ANSI {\\vb HANDLER-CASE}.")
   (value "Same as ANSI {\\vb HANDLER-CASE}."))
  #+(or :keim-clos-conditions :keim-defstruct-conditions)
  `(handler-case ,form ,@body)
  #-(or  :keim-clos-conditions :keim-defstruct-conditions)
  (let ((funs 
	 (mapcar #'(lambda (fdef)
		     (list (car fdef)
			   `(function 
			     (lambda ,(if (null (cadr fdef))
					  (list (gensym))
					  (cadr fdef))
			      ,@(cddr fdef))))) 
		 body)))
    `(sys~handler-bind ,funs
      ,form))
  )

#{\subsection{Basic condition types}

 {\vb SYS+ERROR} is meant to be the highest type of \keim\ error. It
 inherits from the {\vb ERROR} condition type.  It is not intended to have
 any instances.
#}

#+(or :keim-clos-conditions :keim-defstruct-conditions)
(sys~define-condition sys+error (error)
     ()
     (lambda (cond stream)
       (declare (ignore cond stream))
       nil)
     (:documentation "A basic error class for KEIM."))

#{ {\vb SYS+WARNING} is meant to be the highest type of \keim\ warning. It
 inherits from the {\vb WARNING} condition type. It is not intended to have
 any instances.
#}

#+(or :keim-clos-conditions :keim-defstruct-conditions)
(sys~define-condition sys+warning (warning)
     ()
     (lambda (cond stream)
       (declare (ignore cond stream))
       nil)
     (:documentation "A basic warning class for KEIM."))

#{ {\vb SYS+ABORT} is meant to be the highest type of \keim\ abort. Here
 abort is meant as a condition that is signaled when we want to
 abort the current action. {\vb SYS+ABORT} 
 inherits from the {\vb SYS+ERROR} condition type. It is not intended to have
 any instances.
#}

#+(or :keim-clos-conditions :keim-defstruct-conditions)
(sys~define-condition sys+abort (sys+error)
     ()
     (lambda (cond stream)
       (declare (ignore cond stream))
       nil)
     (:documentation "A basic abort class for KEIM."))

