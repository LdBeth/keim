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

(mod~defmod abstr :uses ( env keim mod post sym term type )
	    :documentation "Abstractions, that is, formulas which
bind a variable."
	    :exports (
		      abstr+abstr
		      abstr~bound-variable
		      abstr~scope
		      abstr~set-scope!
		      abstr~create
		      abstr~p
		      )
	    )




#{\section{Abstractions}\label{mod:abstr}

This model handels terms, which are abstractions.
#}

(eval-when (load compile eval)
(defclass abstr+abstr (term+term)
    ((bound-variable :initarg :bound-variable 
		     :reader abstr=bound-variable
		     :writer abstr=write-bound-variable!)
     (scope :initarg :scope :reader abstr=scope 
	    :writer abstr=write-scope!))
    (:documentation "The class of all lambda-abstractions that 
are simple terms.")))

(term~warning-rename-fn term~abstraction-bound-variable abstr~bound-variable)

(term~defgeneric abstr~bound-variable ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is an abstraction of the form ([x] A), then the variable x, else error.")
	   (example "[X].(Q X) --> X"))
  (:method ((term abstr+abstr))
	   (abstr=bound-variable term)))


(term~warning-rename-fn term~abstraction-scope abstr~scope)

(term~defgeneric abstr~scope ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term which is an abstraction" )
	   (effect  "None." )
	   (value   "If TERM is an abstraction of the form ([x] A), then the scope A, else error.")
	   (example "[X].(Q X) --> (Q X)"))
  (:method ((term abstr+abstr))
	   (abstr=scope term)))

(term~warning-rename-fn term~set-abstraction-scope! abstr~set-scope!)

(term~defgeneric abstr~set-scope! ((term) (scope)) 
		 (declare (edited  "16-JUN-1992 16:40")
			  (authors Huang KOHLHASE )
			  (input   "Two terms." )
			  (effect  "None." )
			  (value   "If TERM is an abstraction of the form ([x] A) and SCOPE has"
				   "the same type as A, then SCOPE is substituted for A, else error.")
			  (example "Let QQ be a binary function, [X].(Q X) (QQ X X) --> [X] (Q X X)"))
		 (:method ((term abstr+abstr) (scope term+term))
			  (let ((sco (abstr=scope term)))
			    (if (keim~equal (term~type sco) (term~type scope))
				(progn (abstr=write-scope! scope term)
				       term);add this line XH
			      (error "The proposed new scope ~A has a different type ~
                         than the scope of ~A" scope term)))))

(term~defgeneric abstr~set-bound-variable! ((term) (variable)) 
 (declare (edited  "16-JUN-1992 16:40")
	  (authors Huang KOHLHASE )
	  (input   "Two terms." )
	  (effect  "None." )
	  (value   "If TERM is an abstraction of the form ([x] A) and VARIABLE has"
		   "the same type as X, then VARIABLE is substituted for X, 
else error.")
	  (example "Let QQ be a binary function, [Y].(Q Y) X --> [X] (Q Y)"))
 (:method ((term abstr+abstr) (var sym+var))
   (let ((oldvar (abstr=bound-variable term)))
     (if (keim~equal (term~type var) (term~type oldvar))
	 (progn (abstr=write-bound-variable! var term)
		term);add this line XH
	 (error "The proposed new variable ~A has a different type ~
                         than the variable of ~A" var term)))))


(defmethod keim~copy ((abstraction abstr+abstr))
  (abstr~create 
   (abstr~bound-variable abstraction) 
   (abstr~scope abstraction)))


(defmethod term~copy ((abstraction abstr+abstr))
  (declare (edited  "12-SEP-1991 14:56")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "A new object equal to object where all subcomponents are copied recursively."
		    "Also the term-bindings remain shared."))
  (abstr~create 
   (term~copy (abstr~bound-variable abstraction)) 
   (term~copy (abstr~scope abstraction))))


(defmethod term~set-term ((term1 term+term) (abstraction abstr+abstr))
    (declare (edited  "12-SEP-1991 15:03")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "All slots of TERM1 are set to those of TERM2 (the property list is copied).")
	   (value   "The changed TERM1."))
   (change-class term1 'abstr+abstr)
   (abstr=write-bound-variable! term1 (abstr~bound-variable abstraction))
   (abstr~set-scope! (abstr~scope abstraction) term1)
   term1)

(defmethod term~top ((abstraction abstr+abstr))
  (declare (edited  "02-AUG-1991 20:40")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If TERM is a constant or variable TERM itself,"
		    "if it's an application the applied function of the application in n-normal form,"
                    " that is, if ((f a b) b) is the term then f,"
		    "if it's an abstraction the top of the scope of the abstraction."))
  (term~top (abstr~scope abstraction)))

(defmethod term~subterms ((abstraction abstr+abstr))
  (declare (edited  "02-AUG-1991 20:46")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If term is a constant or variable NIL,"
		    "if it's an application a list containing the applied function and the arguments,"
		    "if it's an abstraction a list containing the scope."))
   (list (abstr~scope abstraction)))

(term~warning-rename-fn term~abstraction-create abstr~create)


(term~defgeneric abstr~create ((bound-variable) (scope) &key)
		 (declare (edited  "15-AUG-1991 13:55")
			  (authors RICHTS)
			  (input   "A variable and a term.")
			  (effect  "None.")
			  (value   "The new term [bound-variable]-scope.")
			  (example "Let QQ be a binary function, X (QQ X X) -->[X].(QQ X X)"))
		 (:method ((bound-variable sym+sym) (scope term+term) &key)
			  (make-instance 'abstr+abstr
					 :bound-variable bound-variable
					 :scope scope
					 :type (type~abstract 
						(term~type scope) 
						(term~type bound-variable)))))

(term~warning-rename-fn term~abstraction-p abstr~p)

(defun abstr~p (object)
  (declare (edited  "12-SEP-1991 14:25")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is an ABSTRACTION."))
  (typep object 'abstr+abstr))

(defmethod print-object ((abstr abstr+abstr) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   )
	   (special *print-level* *print-length*))
  (format stream "[~A].~A"
	  (abstr~bound-variable abstr)
	  (abstr~scope abstr)))


 
(defmethod term~equal ((abstraction1 abstr+abstr) (abstraction2 abstr+abstr))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two abstractions.")
	   (effect  "None.")
	   (value   "True iff the abstractions' bound variables and scopes are term~equal."))
  (or (eq abstraction1 abstraction2)
      (and (term~equal (abstr~bound-variable abstraction1)
		       (abstr~bound-variable abstraction2))
	   (term~equal (abstr~scope abstraction1) 
		       (abstr~scope abstraction2)))))

(defmethod term~equal-p ((abstraction1 abstr+abstr) (abstraction2 abstr+abstr))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two abstractions.")
	   (effect  "None.")
	   (value   "True iff the abstractions' bound variables and scopes are term~equal-p."))
  (or (eq abstraction1 abstraction2)
      (and (term~equal-p (abstr~bound-variable abstraction1)
			 (abstr~bound-variable abstraction2))
	   (term~equal-p (abstr~scope abstraction1) 
			 (abstr~scope abstraction2)))))

(defmethod term~=equal-p-ab-aux ((abstraction1 abstr+abstr)
				(abstraction2 abstr+abstr) (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two abstractions and an alist of symbols.")
	   (effect  "None.")
	   (value   "True if the abstractions bind variables of same type,
and are term~equal-p up to change of bound variables in VARALIST."))
  (or (eq abstraction1 abstraction2)
      (and 
       (keim~equal (term~type (abstr~bound-variable abstraction1))
		   (term~type (abstr~bound-variable abstraction2)))
       (term~=equal-p-ab-aux 
	(abstr~scope abstraction1) 
	(abstr~scope abstraction2)
	(acons (abstr~bound-variable abstraction1)
	       (abstr~bound-variable abstraction2)
	       varalist)))))


(defmethod term~=equal-ab-aux ((abstraction1 abstr+abstr)
				(abstraction2 abstr+abstr) (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two abstractions and an alist of symbols.")
	   (effect  "None.")
	   (value   "True if the abstractions bind variables of same type,
and are term~equal up to change of bound variables in VARALIST."))
  (or (eq abstraction1 abstraction2)
      (and 
       (keim~equal (term~type (abstr~bound-variable abstraction1))
		   (term~type (abstr~bound-variable abstraction2)))
       (term~=equal-ab-aux 
	(abstr~scope abstraction1) 
	(abstr~scope abstraction2)
	(acons (abstr~bound-variable abstraction1)
	       (abstr~bound-variable abstraction2)
	       varalist)))))

(defmethod type~subst-apply ((subst type+substitution) (term abstr+abstr))
  (if (or (type~subst-empty-p subst) (type~ground-p (term~type term)))
      term
      (abstr~create (abstr~bound-variable term)
			       (type~subst-apply subst (abstr~scope term)))))


(defmethod term~poly-subterm-find  ((term abstr+abstr))
  (if (type~ground-p (term~type term))
      (term~poly-subterm-find (abstr~scope term))
    term))


(term~warning-rename-fn term~lambda-bd-p abstr=lambda-list-p)

(defun abstr=lambda-list-p (thing)
  (declare
   (authors nesmith)
   (input "A Lisp object")
   (effect "none")
   (value "Returns T if the object could be a POST representation of a
lambda-bound term."))
  (and (listp thing)
       (>= (length thing) 3)
       (symbolp (car thing))
       (string= (string (car thing)) "LAM")))

(term~warning-rename-fn term=env-lookup-abstraction abstr=env-lookup)

(defun abstr=env-lookup (var scope env)
  (declare
   (authors nesmith)
   (input "A variable with type, a scope and an environment.")
   (effect "none")
   (value "Attempts to parse the list in the environment as an abstraction."
	   "First puts the variable in the environment, then parses the scope."
	   "The variable is removed from the environment whether the parse fails or"
	   "succeeds."))
  (post~read-object var env :variable-multiple)
  (unwind-protect
      (abstr~create (env~lookup-object (car var) env)
		    (term~env-lookup scope env))
    (env~remove (car var) env)))

(defmethod post~print ((abstraction abstr+abstr) stream)
  (declare (edited  "27-JAN-1993 17:37")
	   (authors RICHTS)
	   (input   "A abstraction, a stream and a list of keyword parameters.")
	   (effect  "(lam (<bound-var> <type>) <scope>)")
	   (value   "Like format."))
  (let ((bound-variable (abstr~bound-variable abstraction))
	(scope (abstr~scope abstraction)))
    (format stream "(lam ")
    (post~print-declaration bound-variable stream)
    (format stream " ")
    (post~print scope stream)
    (format stream ")")))

(defmethod env~post-print (key (abstr abstr+abstr) stream)
  (declare (ignore key stream))
  (values))

(defmethod post~read-object :around
  ((term list) (env env+environment)
	       (indicator (eql :existing-term)))
  (if (abstr=lambda-list-p term)
      (abstr=env-lookup (cadr term) (cdr (cdr term)) env)
    (call-next-method)))

