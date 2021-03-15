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

(mod~defmod sksym :uses (env keim mod post sym term type )
	    :documentation "Skolem constants"
	    :exports (
		      sksym+sk-const
		      sksym~p
		      sksym~arity
		      sksym~set-arity!
		      sksym~env-enter
		      sksym~create
		      )
	    )

#{\section{Skolem Constants}\label{mod:sksym}
Skolem constants are special constants obtained during Skolemization. 
In order to ensure the correctness of higher-order Skolemization,
Skolem constants have to keep track of a number of necessary arguments. Hence
Skolem constants have an arity, in addition to the type.
#}

;; renaming term+sk-const -> sksym+sk-const

(eval-when (load compile eval)
(defclass sksym+sk-const (sym+const)
  ((arity :reader sksym=arity
	  :writer sksym=set-arity!
	  :initarg :arity))
  (:documentation "A symbol which has a determined arity, to be used as
a Skolem constant.")))

(term~warning-rename-fn term~skolem-constant-p sksym~p)

(defun sksym~p (object)
  (declare (edited  "12-SEP-1991 13:42")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff {\\vb TERM} is a Skolem-constant.")
	   (example "Let c be of type {\\vb SYM+CONST}"
		    "(change-class c 'sksym+sk-const) --> T"))
  (typep object 'sksym+sk-const))

(term~warning-rename-fn term~skolem-constant-arity sksym~arity)

(term~defgeneric sksym~arity ((const))
  (declare (edited  "3-MAI-1993 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "The arity of the skolem constant {\\vb CONST}.")
	   (example "Let c be of type {\\vb SYM+CONST}"
		    "(change-class c 'SKSYM+SK-CONST)"
		    "(sksym~set-arity! c 3)"
		    "c --> 3"))		    
  (:method ((const sksym+sk-const))
	   (sksym=arity const)))

(term~warning-rename-fn term~set-skolem-constant-arity! sksym~set-arity!)

(term~defgeneric sksym~set-arity! ((const) arity) 
		 (declare (edited  "13-MAI-1993 16:40" )
			  (authors KOHLHASE )
			  (input   "A term and a natural number." )
			  (effect  "Sets the arity of{\\vb  CONST} to ARITY." )
			  (value   "Undefined")
			  (example "Let SK be of type {\\vb SKSYM+SK-CONST}:"
				   "SK   1 --> the arity of SK is changed to 1"))
		 (:method ((const sksym+sk-const) arity)
			  (sksym=set-arity! arity const)))

(term~warning-rename-fn term~env-enter-skolem-constant sksym~env-enter)

(defun sksym~env-enter (symbol type arity env)
  (declare  (authors nesmith)
	    (input "A lisp symbol, a type+type, an arity (natural number corresponding to the type)"
		   "and an environment.")
	    (effect "If the symbol has no association in environment, then a new"
		    "skolem constant with the symbol as name and given type is created and"
		    "entered in the environment.  If the symbol is associated with a"
		    "non-constant or with a constant of a differing type, an error is signaled."
		    "If the symbol is already associated with a constant of the proper type,"
		    "then that constant is returned.")
	    (value "The constant, if one is successfully made.")
	    (example "'C (I I I) 2 (env~create) --> C"))
  (when (not (symbolp symbol))
    (error "~S is not a symbol." symbol))
  (when (not (type~p type))
    (error "~S is not a type." type))
  (when (not (env~p env))
    (error "~S is not an environment." env))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((null thing)
	   (let ((constant (sksym~create symbol type arity)))
	     (env~enter symbol constant env)
	     constant))
	  ((not (sksym~p thing))
	   (post~error "Can't declare ~A as a skolem constant, because ~
                       it already exists in the environment as a ~A." 
		       symbol (class-name (class-of thing))))
	  ((not (keim~equal (term~type thing) type))
	   (post~error "Can't declare ~A as a skolem constant of type ~A, ~
               because it already exists in the environment with type ~S." 
		       symbol type (term~type thing)))
	  (t thing))))

(term~warning-rename-fn term~skolem-constant-create sksym~create)

(defgeneric sksym~create (thing type arity &optional refsym)
  (declare 
   (authors kohlhase nesmith)
   (input   "A shared symbol or a symbol, a type, and an integer arity.
            Optionally a reference symbol.")
   (effect  "none")
   (value   "Creates a new skolem constant using the name of the given symbol, arity and the type and returns it.")
   (example "'sk   (I I I)  2   (G X Y)  --> sk"))
  (:method (thing (type type+type) (arity integer) &optional refsym)
	   (if (<= 0 arity (length (type~n-domain type)))
	       (let ((newconstant (sym~constant-create thing type refsym)))
		 (setq newconstant (change-class newconstant 'sksym+sk-const))
		 (sksym~set-arity! newconstant arity)
		 newconstant)
	     (post~error "The arity ~A is not possible for a skolem ~
                            constant of type ~A" arity type))))





(defmethod term~set-term :after ((term1 term+term) (sym sksym+sk-const))
  (sksym=set-arity! (sksym=arity sym) term1))





