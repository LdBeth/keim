;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)

(mod~defmod inter :uses (mod sys arg help keim)
	    :exports (
		      inter+face
		      inter+warning
		      inter+error
		      inter~input-object
		      inter~prompt-for-input
		      inter~prompt-for-input-with-default
		      inter~prompt-from-choices
		      inter~output-object
		      inter~print-error
		      inter~print-warning
		      inter~terpri
		      inter~fresh-line
		      ))

#{\section{Interfaces}
\label{mod:inter}
 An interface is an abstract object which serves as a conduit
 between a ``user'' and a program.  Actually, the program could
 also be acting as a user, and the user could be a program.
 The only requirement is that the interface be able to carry out
 certain commands relating to output and input.  We define those
 commands in this file.   The program and user need not know ``how''
 those commands are carried out, and each interface may interpret
 them in its own way, appropriate to its purpose.

 Each type of interface is a \clos\ class. We define the most
 primitive here.
#}

(eval-when (load compile eval)
(defclass inter+face (help+help keim+name) 
  ()
  (:documentation "The class of all interfaces")))

#{
 In working with interfaces, we will need to signal some
 conditions.  We define some basic conditions relating to
 conditions here, so that we can use them later. 

 {\vb inter+warning} is a warning to be used.  It has slots for the
 INTERFACE which caused the warning, a FORMAT-STRING, and a list of
 ARGS for the FORMAT-STRING.
#}

(sys~define-condition inter+warning (sys+warning)
     (interface format-string args)
     (lambda (cond stream) (apply #'format stream 
				  (inter+warning-format-string cond)
				  (inter+warning-args cond)))
     (:documentation "A basic warning class for interfaces."))

#{
 {\vb inter+error} is a error to be used.  It has slots for the
 INTERFACE which caused the error, a FORMAT-STRING, and a list of
 ARGS for the FORMAT-STRING.
#}

(sys~define-condition inter+error (sys+error)
     (interface format-string args)
     (lambda (cond stream) 
       (apply #'format stream (inter+error-format-string cond)
	      (inter+error-args cond)))
     (:documentation "A basic error class for interfaces."))

#{\subsection{Input functions}
Here we define some basic input functions that we will expect every
interface to be able to handle in some way.
#}

(defgeneric inter~input-object (interface argtype)
  (declare 
   (authors nesmith)
   (input "An INTERFACE and an ARGTYPE.")
   (effect "The interface is requested to return an object of the
proper argtype.  If the interface doesn't know how to get something of
this type, a condition of type {\\vb inter+error} is signaled.")
   (value "The object if one is returned."))
  (:method (interface argtype)
	   (sys~signal
	    (sys~make-condition 
	     'inter+error 
	     :format-string
	     "Don't know how to input argtype ~S"
	     :interface interface
	     :args (list argtype))))
  (:method ((interface inter+face) (argtype symbol))
	   (let ((argdef (arg~find-argtype argtype)))
	     (if argdef
		 (inter~input-object interface argdef)
		 (sys~signal
		  (sys~make-condition 
		   'inter+error 
		   :format-string
		   "Don't know how to input argtype ~S"
		   :interface interface
		   :args (list argtype))))))
  (:documentation "Get an object from INTERFACE."))

(defgeneric inter~prompt-for-input (interface prompt argtype)
  (declare 
   (authors nesmith)
   (input "An INTERFACE, a PROMPT and an ARGTYPE.")
   (effect "The interface is requested to prompt for and input an object of the
proper argtype.  If the interface doesn't know how to get something of
this type, a condition of type {\\vb inter+error} is signaled.")
   (value "The object if one is returned."))
  (:method (interface prompt argtype)
   (sys~signal 
    (sys~make-condition 'inter+error :format-string
     "Don't know how to prompt with args: ~S, ~S"
     :interface interface
     :args (list prompt argtype))))
  (:method ((interface inter+face) prompt (argtype symbol))
   (let ((argdef (arg~find-argtype argtype)))
     (if argdef
	 (inter~prompt-for-input interface prompt argdef)
	 (sys~signal 
	  (sys~make-condition 'inter+error :format-string
			      "Don't know how to prompt with args: ~S, ~S"
			      :interface interface
			      :args (list prompt argtype))))))
  (:method ((interface inter+face) prompt (type arg+type))
    (sys~handler-case
       (progn
	(inter~output-object interface prompt)
	(inter~input-object interface type))
       (inter+error (condition)
		    (inter~print-error interface condition))
       (inter+warning (condition)
		      (inter~print-warning interface condition))))
 (:documentation "Prompt with PROMPT for an object of type TYPE from 
INTERFACE."))

(defgeneric inter~prompt-for-input-with-default (interface prompt argtype default)
  (declare 
   (authors nesmith)
   (input "An INTERFACE, a PROMPT, an ARGTYPE and a DEFAULT.")
   (effect "The interface is requested to prompt for and input an object of the
proper argtype.  If the interface doesn't know how to get something of
this type, a condition of type {\\vb inter+error} is signaled.  The DEFAULT is used if the user indicates it.")
   (value "The object if one is returned."))
  (:method (interface prompt argtype default)
   (sys~signal 
    (sys~make-condition 'inter+error :format-string
     "Don't know how to prompt with args: ~S, ~S, ~S"
     :interface interface
     :args (list prompt argtype default))))
  (:method ((interface inter+face) prompt (argtype symbol) default)
   (let ((argdef (arg~find-argtype argtype)))
     (if argdef
	 (inter~prompt-for-input-with-default interface prompt argdef default)
	 (sys~signal 
	  (sys~make-condition 'inter+error :format-string
			      "Don't know how to prompt with args: ~S, ~S, ~S"
			      :interface interface
			      :args (list prompt argtype default))))))
  (:method ((interface inter+face) prompt (type arg+type) default)
    (sys~handler-case
       (progn
	 (inter~output-object interface prompt)
	 (inter~output-object interface "[")
	 (inter~output-object interface default)
	 (inter~output-object interface "]")
	 (inter~input-object interface type))
       (inter+error (condition)
		    (inter~print-error interface condition))
       (inter+warning (condition)
		      (inter~print-warning interface condition))))
 (:documentation "Prompt with PROMPT for an object of type TYPE from 
INTERFACE, using DEFAULT if requested."))

(defgeneric inter~prompt-from-choices (interface prompt choices)
  (declare 
   (authors nesmith)
   (input "An INTERFACE, a PROMPT, and a list of CHOICES.")
   (effect "The interface is requested to prompt for return an object
out of the list of choices. ")
   (value "The object if one is returned."))
  (:method (interface prompt choices)
    (sys~signal
     (sys~make-condition
     'inter+error
     :format-string
     "Don't know how to prompt with args: ~S, ~S"
     :interface interface 
     :args (list prompt choices))))
  (:method (interface prompt (choices null))
    (declare (ignore prompt))
    (sys~signal
     (sys~make-condition
     'inter+error
     :format-string
     "No choice was given"
     :interface interface 
     :args nil)))
  (:method ((interface inter+face) prompt (choices list))
   (let* ((obj nil) 
	  (n 1)
	  (newchoices nil))
     (dolist (choice choices (setq newchoices (nreverse newchoices)))
       (push (cons n choice) newchoices)
       (incf n))
    (loop
     (sys~handler-bind ((arg+input-error 
		     #'(lambda (c) (inter~print-error interface c) nil)))
      (inter~output-object interface prompt)
      (inter~terpri interface)
      (inter~output-object interface "Choices are: ")
      (inter~terpri interface)
      (dolist (choice newchoices)
	(inter~output-object interface (car choice))
	(inter~output-object interface ": ")
	(inter~output-object interface (cdr choice))
	(inter~terpri interface))
      (setq obj (inter~input-object interface 'posinteger))
      (let ((found (find obj newchoices :key #'car)))
	(if found
          (return-from inter~prompt-from-choices (cdr found))
	  (inter~print-error
	   interface
	   (sys~make-condition
	    'inter+error
	    :format-string
	    "You must enter a positive integer between 1 and ~D"
	    :interface interface 
	    :args (list (length choices))))))))))
 (:documentation "Prompt with PROMPT for an object from CHOICES."))

#{\section{Output functions}
Here are some basic output functions, that just about every interface
should be able to handle.
#}

(defgeneric inter~output-object (interface object)
  (declare 
   (authors nesmith)
   (input "An INTERFACE and an OBJECT.")
   (effect "The interface is requested to somehow output the object.")
   (value "Undefined."))
 (:method (interface object)
  (sys~signal
   (sys~make-condition 'inter+error
		       :format-string
		       "Don't know how to output ~S"
		       :interface interface
		       :args (list object))))
 (:documentation "Output OBJECT to INTERFACE."))

(defgeneric inter~print-error (interface error-object)
  (declare 
   (authors nesmith)
   (input "An INTERFACE and an ERROR-OBJECT.")
   (effect "The interface is requested to display the 
object as an error message.")
   (value "Undefined."))
 (:method (interface error-object)
   (sys~signal
    (sys~make-condition
     'inter+error
     :format-string
     "Don't know print error ~S" 
     :interface interface 
     :args (list error-object))))
 (:documentation "Output the ERROR-OBJECT to INTERFACE."))

(defgeneric inter~print-warning (interface warning-object)
  (declare 
   (authors nesmith)
   (input "An INTERFACE and an WARNING-OBJECT.")
   (effect "The interface is requested to display the object as a 
warning message.")
   (value "Undefined."))
 (:method (interface warning-object)
   (sys~signal
    (sys~make-condition
     'inter+error
     :format-string
     "Don't know print warning ~S" 
     :interface interface 
     :args (list warning-object))))
 (:documentation "Output the WARNING-OBJECT to INTERFACE."))

(defgeneric inter~terpri (interface)
  (declare 
   (authors nesmith)
   (input "An INTERFACE.")
   (effect "The interface is requested to send a new line (if meaningful).")
   (value "Undefined."))
  (:method ((interface t))
   (sys~signal
     (sys~make-condition
     'inter+error
     :format-string
     "Don't know how to TERPRI:"
     :interface interface 
     :args nil)))
  (:documentation "Output a new line to INTERFACE."))

(defgeneric inter~fresh-line (interface)
  (declare 
   (authors nesmith)
   (input "An INTERFACE.")
   (effect "The interface is requested to send a fresh line (if meaningful).")
   (value "Undefined."))
  (:method ((interface t))
   (sys~signal
     (sys~make-condition
     'inter+error
     :format-string
     "Don't know how to FRESH-LINE:"
     :interface interface 
     :args nil)))
  (:documentation "Output a new line (if not at beginning of line) 
to INTERFACE."))


