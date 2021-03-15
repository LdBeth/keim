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

(mod~defmod termc :uses (abstr appl keim mod post shsym sym term type )
	    :documentation "Meta-properties of terms."
	    :exports (
		      termc~compound-term-p
		      termc~quantification-create
		      termc~quantification-bound-variable
		      termc~quantification-scope
		      termc~universal-quantor-p
		      termc~existential-quantor-p
		      termc~universal-quantification-p
		      termc~existential-quantification-p
		      termc~read-atom
		      termc~atom-p
		      termc~negation-p
		      termc~disjunction-p
		      termc~conjunction-p
		      termc~equivalence-p
		      termc~implication-p
		      termc~all-bound-variables
		      termc~depth
		      termc~ground-p
		      )
	    )

#{\section{Compound Terms}\label{mod:termc}
In this section the basic functionality for manipulating compound terms is provided.
#}

(term~warning-rename-fn term~compound-term-p termc~compound-term-p)

(defun termc~compound-term-p (object)
  (declare (edited  "12-SEP-1991 14:25")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a compound term, i.e. if it is an application or abstraction.")
	   (example "(G X Y) --> T"
		    "([X].[Y].(G X Y) X D) --> T"
		    "X --> NIL"))
  (or (appl~p object) (abstr~p object)))

#{\subsection{Quantifications and semantically-predefined symbols} #}


(term~warning-rename-fn term~quantification-create termc~quantification-create)

(defun termc~quantification-create (quantor bound-var scope)
  (declare (edited  "15-JUN-1992 17:39" )
	   (authors KOHLHASE )
	   (input   "A quantor, a variable, amd a term {\vb SCOPE} of type o.")
	   (effect  "If {\vb QUANTOR} has a polymorphic type the type variables can be instantiated"
		    "so that the the shared symbol will be changed.")
	   (value   "If {\vb SCOPE} is of type o, {\vb BOUND-VAR} of type A and quantor of type ((A -> o) -> o)"
		    "Then the term (quantor [bound-var] scope)."
		    "Else Error.")
	   (example "Let FORALL be of type ((A) -> o) -> o"
		    "FORALL     X     (P X)  --> FORALL [X].(P X)"))
  (if (type~o-p (term~type scope))
      (appl~poly-create quantor (list (abstr~create bound-var scope)))
      (error "~A is not a formula of type O, that can be quantified over"
	     scope)))

(term~warning-rename-fn term~quantification-bound-variable termc~quantification-bound-variable)


(defun termc~quantification-bound-variable (term)
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is a quantification, then the bound variable, else error.")
	   (example "(FORALL [X].(P X))  -->  X"))
  (if (or (termc~existential-quantification-p term)
	  (termc~universal-quantification-p term))
      (abstr~bound-variable (car (appl~arguments term)))
      (error "term ~A is not a quantification" term)))

(term~warning-rename-fn term~quantification-scope termc~quantification-scope)

(defun termc~quantification-scope (term)
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is a quantification, then the scope, else error.")
	   (example "(FORALL [X].(P X))  -->  (P X)"))
  (if (or (termc~existential-quantification-p term)
	  (termc~universal-quantification-p term))
      (abstr~scope (car (appl~arguments term)))
      (error "term ~A is not a quantification" term)))

(term~warning-rename-fn term~universal-quantor-p termc~universal-quantor-p)

(defun termc~universal-quantor-p (term)
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "True, if TERM is an universal quantor.")
	   (example "FORALL --> T"
		    "EXISTS --> NIL"
		    "X --> NIL"))
  (and (sym~constant-p term)
       (string= "FORALL" (keim~name term))))

(term~warning-rename-fn term~existential-quantor-p termc~existential-quantor-p)

(defun termc~existential-quantor-p (term)
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "True, if TERM is an existential quantor.")
	   (example "EXISTS --> T"
		    "FORALL --> NIL"
		    "X      --> NIL"))
  (and (sym~constant-p term)
       (string= "EXISTS" (keim~name term))))

(term~warning-rename-fn term~universal-quantification-p termc~universal-quantification-p)

(term~defgeneric termc~universal-quantification-p ((term))
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "True, if TERM is a universal quantification, i.e. a term of the form (forall [X] A).")
	   (example "(FORALL [X].(P X)) --> T"
		    "([X].[Y].(G X Y) X D) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
	   (and (termc~universal-quantor-p (appl~function term))
		(abstr~p (car (appl~arguments term))))))

(term~warning-rename-fn term~existential-quantification-p termc~existential-quantification-p)

(term~defgeneric termc~existential-quantification-p ((term))
  (declare (edited  "16-JUN-1992 16:15" ) 
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "True, if TERM is a existential quantification, i.e. a term of the"
		    "form (exists [X] A).")
	   (example "(EXISTS [X].(P X)) --> T"
		    "([X].[Y].(G X Y) X D) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
	   (and (termc~existential-quantor-p 
		 (appl~function term))
		(abstr~p (car (appl~arguments term))))))


(term~warning-rename-fn term~read-atom termc~read-atom)

(defun termc~read-atom (term env)
  (declare 
   (authors nesmith)
   (effect "")
   (input "A POST representation of an atom, and an environment.")
   (value "If the input can be parsed as an atom in the environment"
	  "it is returned, otherwise an error is signaled.")
   (example "Let P, X, and Y be in the environment ENV"
	    "(P X Y)    ENV --> (P X Y)"))
  (let ((readterm (term~env-lookup term env)))
    (if (termc~atom-p readterm)
	readterm
      (post~error "Expected a term atom, read ~A" readterm))))

(term~warning-rename-fn term~atom-p termc~atom-p)

(term~defgeneric termc~atom-p ((term))
  (declare (edited  "7-JUL-1993 14:49" )
	   (authors nesmith )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True if {\vb TERM} is an atom, else NIL. For our purposes, an"
		    "atom is what you think of in the first-order sense of atom, i.e., no lambda"
		    "bindings, no quantifiers, no connectives.  So an atom is a term with type O,"
		    "such that either it is a symbol, or it is an application whose function is"
		    "a predicate and is not a quantification, negation, conjunction, disjunction,"
		    "implication or equivalence.")
	   (example "(P X Y) --> T"
		    "(F X)   --> NIL"
		    "([X].[Y].(G X Y) X D) --> NIL"
		    "(EXISTS [X].(Q X))  --> NIL"
		    "(CON (EXISTS [X].(Q X))) --> T"))
  (:method ((term sym+sym))
      (type~o-p (term~type term)))
  (:method ((term abstr+abstr))
      nil)
  (:method ((term appl+appl))
      (and (type~o-p (term~type term))
	   (not (termc~universal-quantification-p term))
	   (not (termc~existential-quantification-p term))
	   (not (termc~negation-p term))
	   (not (termc~disjunction-p term))
	   (not (termc~conjunction-p term))
	   (not (termc~equivalence-p term))
	   (not (termc~implication-p term)))))
   
(eval-when (load compile eval)
(defmacro termc=head-has-same-name-and-type-p (term name type)
  `(let* ((top (term~top ,term))
	  (topname (keim~name top))
	  (toptype (term~type top)))
     (and (string= topname ,name)
	  (keim~equal toptype ,type))))
)

(term~warning-rename-fn   term~negation-p termc~negation-p)

(term~defgeneric termc~negation-p ((term))
  (declare (edited  "19-JUN-1992 14:49" )
	   (authors KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a negation.")
	   (example "(NOT (EXISTS [X].(Q X))) --> T"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
      (termc=head-has-same-name-and-type-p term "NOT"
       (type=create (type~o) (list (type~o))))))

(term~warning-rename-fn   term~disjunction-p termc~disjunction-p)

(term~defgeneric termc~disjunction-p ((term))
  (declare (edited  "19-JUN-1992 14:49" )
	   (authors KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a disjunction.")
	   (example "(OR (Q A) (NOT (Q A)))  --> T"
		    "(AND (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
	   ;(keim~equal (term~top term) (term~env-get-symbol 'or))
      (termc=head-has-same-name-and-type-p term "OR"
       (type=create (type~o) (list (type~o) (type~o))))
    ;;(keim~get (term~top term)'or)
    ))

(term~warning-rename-fn   term~conjunction-p termc~conjunction-p)
(term~defgeneric termc~conjunction-p ((term))
  (declare (edited  "19-JUN-1992 14:49" )
	   (authors KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is a conjunction.")
	   (example "(AND (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
      (termc=head-has-same-name-and-type-p term "AND"
       (type=create (type~o) (list (type~o) (type~o))))))

(term~warning-rename-fn   term~equivalence-p termc~equivalence-p)
(term~defgeneric termc~equivalence-p ((term))
  (declare (edited  "19-JUN-1992 14:49" )
	   (authors KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is an equivalence.")
	   (example "(EQUIV (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
	   (termc=head-has-same-name-and-type-p 
	    term "EQUIV"
	    (type=create (type~o) (list (type~o) (type~o))))
    ))

(term~warning-rename-fn   term~implication-p termc~implication-p)

(term~defgeneric termc~implication-p ((term))
  (declare (edited  "19-JUN-1992 14:49" )
	   (authors KOHLHASE )
	   (input   "A term.")
	   (effect  "None.")
	   (value   "true, iff {\vb TERM} is an implication.")
	   (example "(IMPLIES (Q A) (NOT (Q A)))  --> T"
		    "(OR (Q A) (NOT (Q A)))) --> NIL"))
  (:method ((term term+term))
	   nil)
  (:method ((term appl+appl))
      (termc=head-has-same-name-and-type-p term "IMPLIES"
       (type=create (type~o) (list (type~o) (type~o))))))


#{\subsection{Simple operations on terms} #}

(term~warning-rename-fn   term~all-bound-variables termc~all-bound-variables)

(term~defgeneric termc~all-bound-variables ((term))
  (declare (edited  "05-oct-1990 19:20")
	   (authors richts)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "The list of all variables bound by any abstraction inside of {\vb TERM}.")
	   (example "(FORALL [A].(FORALL [B].(EQUIV (P B) (FORALL [X].(IMPLIES (A X) (B X)))))) --> (A B X)"))
  (:method ((termlist list))
   (mapcan #'termc~all-bound-variables termlist))
  (:method ((variable sym+var))
   nil)
  (:method ((constant sym+const))
   nil)
  (:method ((application appl+appl))
   (mapcan #'termc~all-bound-variables (term~subterms application)))
  (:method ((abstraction abstr+abstr))
   (cons (abstr~bound-variable abstraction) 
	 (termc~all-bound-variables (abstr~scope abstraction)))))

(term~warning-rename-fn   term~depth termc~depth)

(defun termc~depth (term)
  (declare (edited  "05-oct-1990 19:20")
	   (authors richts)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "The depth of TERM.")
	   (example "x --> 1"
		    "(G X Y) --> 2"
		    "(P (F C) C) --> 3"
		    "([X].(Q X) C) --> 4"
		    "(FORALL [X].(PPP (F X) X)) --> 5"))
  (if (listp term)
      (apply #'max (mapcar #'termc~depth term))
      (1+ (apply #'max 0 (mapcar #'termc~depth (term~subterms term))))))

(term~warning-rename-fn   term~ground-p termc~ground-p)

(term~defgeneric termc~ground-p ((term))
  (declare (edited  "31-OCT-1991 13:06")
	   (authors RICHTS)
	   (input   "A term or nested termlist.")
	   (effect  "None.")
	   (value   "True iff {\vb TERM} contains no free variables.")
	   (example "(P X X) --> NIL"
		    "(P A B) --> T"
		    "((FORALL [X].(Q X)) --> T"))
  (:method ((termlist list))
   (every #'termc~ground-p termlist))
  (:method ((variable sym+var))
   (sym~label variable))
  (:method ((constant sym+const))
   t)
  (:method ((application appl+appl))
   (every #'termc~ground-p (term~subterms application)))
  (:method ((abstraction abstr+abstr))
   (let* ((symbol (sym~shared-symbol (abstr~bound-variable abstraction)))
	  (old-label (shsym~label symbol))
	  (cons-label (cons t old-label)))
     (unwind-protect
	 (progn (shsym~set-label! symbol cons-label)
		(termc~ground-p (abstr~scope abstraction)))
       (shsym~set-label! symbol old-label)))))







