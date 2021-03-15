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


(IN-PACKAGE "KEIM")

(mod~defmod fo :uses (abstr appl keim mod sym term termc type )
	    :documentation "Special interface for first order terms."
	    :exports (
		      fo~arguments
		      fo~set-arguments!
		      fo~variable-create
		      fo~variable-p
		      fo~variable-list-p
		      fo~function-create
		      fo~function-p
		      fo~arity
		      fo~constant-create
		      fo~constant-p
		      fo~predicate-create
		      fo~predicate-p
		      fo~application-create
		      fo~application-p
		      fo~quantification-create
		      fo~quantification-p
		      fo~quantification-bound-variable
		      fo~set-quantification-bound-variable!
		      fo~quantification-scope
		      fo~set-quantification-scope!
		      fo~term-p
		      fo~formula-p
		      fo~atom-p
		      fo~copy
		      fo~set-term
		      fo~termlist-p
		      fo~nested-termlist-p
		      fo~subterms
		      fo~binding
		      fo~symbol-binding
		      fo~term-binding
		      )

	    )

#{
\section{Special interface for first order terms}\label{mod:fo}
\subsection{Introduction}

In order to facilitate the use of \keim\ for users who only want to
use first order predicate logic, \keim\ features a special interface.
The functions in the module {\vb fo} are abbreviations for functions
in {\vb term} that screen out the use of types (first order terms
always have type I).  {\vb fo} does not support all \keim\
functionality for that the {\vb term} functions can be used.


%\begin{figure}
%\input{fo-fig}
%\end{figure}
#}
;;;begin{lisp}
;;; Implementation:
;;;
;;; Clos-classes:              TERM+TERM                     
;;;                       /     /     \     \
;;;                      /     /       \     \
;;;                     /     /         \     \
;;;              SYM+VAR SYM+CONST APPL+APPL ABSTR+ABSTR
;;;                 :      :   :   :         :
;;;                 :      :   :   :         :
;;; special    VARIABLE    :   :   :         :
;;; instances        FUNCTION  :  PREDICATE QUANTIFICATION
;;; for FO:                :   :
;;;                  CONSTANT PREDEFINED
;;;end{lisp}

#{The special instances are present in the names of the interface
functions, and therefore only a mnemonic aid for the programmer.
They include:

\begin{tabular}{lp{11.9cm}}
{\vb VARIABLE}        &  {\vb SYM+VAR}s of type $\iota$\\
{\vb FUNCTION}        &  {\vb SYM+CONST}s of type $\iota \ldots \iota \rightarrow \iota$\\
{\vb CONSTANT}        &  {\vb SYM+CONST}s of type $\iota$\\
{\vb PREDICATE}       &  {\vb SYM+CONST}s of type $\iota \ldots \iota \rightarrow o$\\
{\vb APPLICATION}     &  The same as {\vb APPL+APPL}\\
{\vb QUANTIFICATION}  &  {\vb APPL}ications with a unitary topfunction and an {\vb TERM+AB\-ST}raction as argument
                  $(Q [x].t)$, where $Q \in  \{ALL, EX, \ldots \}$
\end{tabular}
#}

;;;begin{lisp}
;;;Instances for first order logic:
;;;VARIABLE           VARs of type i
;;;FUNCTION           CONSTs of type i..i->i
;;;CONSTANT           CONSTs of type i
;;;PREDICATE          CONSTs of type i..i->o
;;;PREDEFINED         Predefined CONSTs like ALL, EX, TRUE, FALSE, ...
;;;APPLICATION        The same as APPL
;;;QUANTIFICATION     APPLications with an unitary topfunction and an ABSTRaction as arguments (Q [x].t) Q = ALL, EX, ...
;;;end{lisp}

#{First order terms are not defined to be \clos\ subclasses of the {\vb
term}-classes, even though conceptually the set of first-order
\keim\-terms is a subset of the set of \keim-terms. Otherwise it would
not be possible to use terms of the two classes mixedly. The property
of a term to be first order can only be checked by a complex parsing
process and can change over the lifespan of a term (e.g. by
substitution). Thus a classifier would constantly have to check
whether the terms that were created as \keim-terms are now first-order
\keim-terms and accordingly change the class of the term.
#}


#{\subsection{Special first order procedures}#}

(defgeneric fo~arguments (term)
  (declare (edited  "02-AUG-1991 20:46")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "Nil if term is a constant or variable,"
		    "a list containing the scope if it's a quantification,"
		    "a list containing the arguments of the application otherwise.")
	   (example "Let c be an object constant, x an object variable and P a binary predicate constant"
		    "c --> nil"
		    "x --> nil"
		    "(forall (lam (y I) (P y c))) --> ((P y c))"
		    "(P x c) --> (x c)"))
  (:method ((variable sym+var))
	   nil)
  (:method ((constant sym+const))
	   nil)
  (:method ((application appl+appl))
	   (if (fo~quantification-p application)
	       (list (fo~quantification-scope application))
	       (appl~arguments application)))
  (:method ((abstraction abstr+abstr))
	   (error "This function is not defined for abstractions.")))

(defgeneric fo~set-arguments! (term new-arguments)
  (declare (edited  "26-AUG-1992 13:45")
	   (authors RICHTS)
	   (input   "A term and the new list of arguments.")
	   (effect  "The argument-list of term is replaced by new-arguments which must be of equal length.")
	   (value   "New-arguments.")
	   (example "(P x c)       (x1 c1) --> (x1 c1), the original term is changed to (P x1 c1)"))
  (:method ((variable sym+var) new-arguments)
   (declare (ignore new-arguments))
   (error "~A is not an application and so no arguments can be set." variable))
  (:method ((constant sym+const) new-arguments)
   (declare (ignore new-arguments))
   (error "~A is not an application and so no arguments can be set." constant))
  (:method ((application appl+appl) new-arguments)
   (if (and (= (length new-arguments) (length (fo~arguments application)))
	    (every #'(lambda (arg1 arg2)
		       (keim~equal (term~type arg1) (term~type arg2)))
		   new-arguments (fo~arguments application)))
       (if (fo~quantification-p application)
	   (abstr~set-scope! (car (appl~arguments application)) (car new-arguments))
	   (appl~set-arguments! application new-arguments))
       (error "The new arguments ~A do not fit the arguments in ~A." new-arguments application)))
  (:method ((abstraction abstr+abstr) new-arguments)
   (declare (ignore new-arguments))
   (error "~A is not an application and so no arguments can be set." abstraction)))



#{\subsection{Variables}
The following procedures manipulating variables are prefixed by fo~variable-.
#}

(defvar fo*variable-counter 0)
;A counter for uniquely numerating new variables. Increased by 1 each
;time, when fo~variable-create is called.

(defun fo~variable-create (&optional name &rest term)
  (declare (edited  "25-JUL-1991 13:03")
	   (authors huang RICHTS)
	   (input   "name: a symbol."
		    " A reference term")
	   (effect  "None.")
	   (value   "The new variable (if NAME is nil its name will be "x-<number>").")
	   (example "(fo~variable-create) --> x-2"
		    "(fo~variable-create) --> x-3"
		    "(term~type (fo~variable-create)) --> i"))
  (apply #'sym~variable-create
	 (if name name (intern (format nil "x-~A" (incf fo*variable-counter))))
	 (type~i)
	 term))

(defun fo~variable-p (object)
  (declare (edited  "25-JUL-1991 13:13")
	   (authors RICHTS )
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order variable.")
	   (example "x --> t"
		    "c --> nil"))
  (and (sym~variable-p object) (type~i-p (term~type object))))


(defun fo~variable-list-p (termlist)
  (declare (edited  "08-AUG-1991 13:30")
	   (authors RICHTS)
	   (input   "A list of terms.")
	   (effect  "None.")
	   (value   "True iff TERMLIST is a non-empty list of first order variables.")
	   (example "(x y) --> t"
		    "(x c) --> nil"))
  (and (consp termlist) (fo~variable-p (car termlist)) (or (null (cdr termlist)) (fo~variable-list-p (cdr termlist)))))

#{\subsection{Functions}
The following procedures manipulating functions are prefixed by fo~function-.
#}

(defvar fo*function-counter 0)
;A counter for uniquely numerating new function symbols. Increased by 1 each
;time, when fo~function-create is called.


(defun fo~function-create (term arity &optional name &rest keyword-pairs);???
  (declare (edited  "25-JUL-1991 13:03")
	   (authors RICHTS)
	   (input   "A type(????), something that can be coerced into a string, a value and a sort.")
	   (effect  "None.")
	   (value   "The new function  (if NAME is nil its name will be "f-<number>")."))
  (apply #'sym~constant-create
	 term
	 (type~function-create arity)
	 (if name (string name) (format nil "f-~A" (incf fo*function-counter)))
	 keyword-pairs))

(defun fo~function-p (object)
  (declare (edited  "08-AUG-1991 13:38")
	   (authors RICHTS)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order function.")
	   (example "f --> t"
		    "c --> nil"))
  (and (sym~constant-p object)
       (type~i-p (type~n-range (term~type object)))
       (every #'(lambda (type) (type~i-p type)) (type~n-domain (term~type object)))))

(defun fo~arity (term)
  (declare (edited  "02-AUG-1991 20:59")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The arity of the term (computed from its type).")
	   (example "Let f be a binary function symbol and P a binary predicate symbol"
		    "f --> 2"
		    "P --> 2"
		    "(P a b) --> 0"))
  (length (type~n-domain (term~type term))))

#{\subsection{Constants}
The following procedures manipulating constants are prefixed by fo~constant-.
#}

(defvar fo*constant-counter 0)
;A counter for uniquely numerating new constant symbols. Increased by 1 each
;time, when fo~constant-create is called.

(defun fo~constant-create (&optional name &rest term)
  (declare (edited  "25-JUL-1991 13:03")
	   (authors huang RICHTS)
	   (input   "name: a symbol"
		    "and a reference term")
	   (effect  "None.")
	   (value   "The new constant (if name is nil its name will be "c-<number>").")
	   (example "(fo~constant-create 'c)--> C"
		    "(fo~constant-create) --> c-3"
		    "(term~type (fo~constant-create)) --> i"))
  (apply #'sym~constant-create
	 (if name name (intern (format nil "c-~A" (incf fo*function-counter))))
	 (type~i)
	 term))

(defun fo~constant-p (object)
  (declare (edited  "02-AUG-1991 21:23")
	   (authors RICHTS)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order constant.")
	   (example "c --> t"
		    "f --> nil"
		    "x --> nil"))
  (and (sym~constant-p object) (type~i-p (term~type object))))

#{\subsection{Predicates}
The following procedures manipulating predicate symbols are prefixed by fo~predicate-.
#}

(defvar fo*predicate-counter 0)
;A counter for uniquely numerating new predicate symbols. Increased by 1 each
;time, when fo~predicate-create is called.

(defun fo~predicate-create (term arity &optional name &rest keyword-pairs);???
  (declare (edited  "25-JUL-1991 13:03")
	   (authors RICHTS)
	   (input   "A number, something that can be coerced into a string, a value and a sort.")
	   (effect  "None.")
	   (value   "The new predicate (if name is nil its name will be "P-<number>").")
	   (example "???"))
  (apply #'sym~constant-create
	 term
	 (type~predicate-create arity)
	 (if name (string name) (format nil "P-~A" (incf fo*predicate-counter)))
	 keyword-pairs))

(defun fo~predicate-p (object)
  (declare (edited  "08-AUG-1991 13:38")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a first order predicate.")
	   (example "P --> t"
		    "f --> nil"))
  (and (sym~constant-p object)
       (type~o-p (type~n-range (term~type object)))
       (every #'(lambda (type) (type~i-p type)) (type~n-domain (term~type object)))))

#{\subsection{Applications}
The following procedures manipulating application are prefixed by fo~application-.
#}

(defun fo~application-create (function arguments &rest keyword-pairs)
  (declare (edited  "15-AUG-1991 13:41")
	   (authors RICHTS)
	   (input   "A term with arity n and a list of terms with length = n.")
	   (effect  "None.")
	   (value   "The term (function . arguments).")
	   (example "f       (c1 c2) --> (f c1 c2)"))
  (if (= (fo~arity function) (length arguments))
      (apply #'appl~create function arguments keyword-pairs)
      (error "Wrong number of arguments (~A) for function ~A (with arity ~A) to create a first order application."
	     (length arguments) function (fo~arity function))))

(defun fo~application-p (object)
  (declare (edited  "12-SEP-1991 14:25")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is an application,"
		    "i.e. an instance of APPL+APPL which is not a quantification.")
	   (example "(f c1 c2) --> t"
		    "f --> nil"))
  (and (appl~p object) (not (fo~quantification-p object))))



#{\subsection{Quantification}
The following procedures manipulating quantification symbols are prefixed by fo~quantification-.
#}

(defun fo~quantification-create (quantor variable scope)
  (declare (edited  "15-AUG-1991 13:46")
	   (authors RICHTS)
	   (input   "A Quantor, a variable and a term.")
	   (effect  "None.")
	   (value   "The new term (quantor [variable]-scope).")
	   (example "(forall x (P x)) --> (forall [x].(P x))"))
  (appl~create quantor (list (abstr~create variable scope))))

(defun fo~quantification-p (object)
  (declare (edited  "15-AUG-1991 13:45")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff the term is a quantification"
		    "(i.e. it's an application with one argument which is an abstraction).")
	   (example "(forall [x].(P x)) --> t"
		    "(P x) --> nil)"))
  (and (appl~p object)
       (null (cdr (appl~arguments object)))
       (abstr~p (car (appl~arguments object)))))

(defun fo~quantification-bound-variable (quantification)
  (declare (edited  "15-AUG-1991 13:48")
	   (authors RICHTS)
	   (input   "A quantification.")
	   (effect  "None.")
	   (value   "The bound variable of the quantification.")
	   (example "(forall [x].(P x)) --> x"))
  (abstr~bound-variable (car (appl~arguments quantification))))

(defun fo~set-quantification-bound-variable! (quantification new-bound-variable)
  (declare (edited  "15-AUG-1991 13:50")
	   (authors RICHTS)
	   (input   "A quantification and a variable.")
	   (effect  "The bound variable of the quantification is replaced by the new variable.")
	   (value   "The new bound variable.")
	   (example "(forall [x].(P x))     y --> (forall [y].(P y))"))
  (abstr~set-bound-variable! (car (appl~arguments quantification)) new-bound-variable))

(defun fo~quantification-scope (quantification)
  (declare (edited  "15-AUG-1991 13:49")
	   (authors RICHTS)
	   (input   "A quantification.")
	   (effect  "None.")
	   (value   "The scope of the quantification.")
	   (example "(forall [x].(P x)) --> (P x)"))
  (abstr~scope (car (appl~arguments quantification))))

(defun fo~set-quantification-scope! (quantification new-scope)
  (declare (edited  "15-AUG-1991 13:50")
	   (authors RICHTS)
	   (input   "A quantification and a term.")
	   (effect  "The scope of QUANTIFICATION is replaced by NEW-SCOPE.")
	   (value   "The new scope.")
	   (example "(forall [x].(P x))     (Q x x) --> (forall [x].(Q x x))"))
  (abstr~set-scope! (car (appl~arguments quantification)) new-scope))


#{\subsection{First order test procedures.}
The following procedures are special tests for first order objects.#}

(term~warning-rename-fn termc~fo-term-p fo~term-p)

(defun fo~term-p (term)
  (declare (edited  "07-jul-1993 19:20")
	   (authors nesmith)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a first order logic term."))
  (and (type~i-p (term~type term))
       (or (sym~p term)
	   (and (appl~p term)
		(sym~constant-p (appl~function term))
		(every #'fo~term-p (appl~arguments term))))))

(term~warning-rename-fn termc~fo-formula-p fo~formula-p)

(defun fo~formula-p (term)
  (declare (edited  "07-jul-1993 19:20")
	   (authors nesmith)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a first order logic formula."))
  (cond ((fo~atom-p term) t)
	((not (appl~p term)) nil)
	((or (termc~universal-quantification-p term)
	     (termc~existential-quantification-p term))
	 (and (type~i-p (termc~quantification-bound-variable term))
	      (fo~formula-p (termc~quantification-scope term))))
	((or (termc~negation-p term)
	     (termc~disjunction-p term)
	     (termc~conjunction-p term)
	     (termc~equivalence-p term)
	     (termc~implication-p term))
	 (every #'fo~formula-p (appl~arguments term)))
	(t nil)))

(term~warning-rename-fn termc~fo-atom-p fo~atom-p)

(defun fo~atom-p (term)
  (declare (edited  "05-oct-1990 19:20")
	   (authors richts)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a first order atom."))
  (and (type~o-p (term~type term))
       (or (and (sym~p term)
		(sym~constant-p (term~top term)))
	   (and (appl~p term)
		(every #'fo~term-p (appl~arguments term))))))





#{In addition, the following functions are provided, they simply call the
corresponding functions of the module \ref{mod:term}.
#}

(defun fo~copy (object)
  (declare (edited  "12-SEP-1991 14:56")
	   (authors RICHTS)
	   (input   "An {\vb OBJECT}.")
	   (effect  "None.")
	   (value   "A new object equal to object where all subcomponents are copied recursively."
		    "Also the term-bindings remain shared."))
  (term~copy object))
  
(defun fo~set-term (term1 term2)
  (declare (edited  "12-SEP-1991 15:03")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "All slots of TERM1 are set to those of TERM2 (the property list is copied).")
	   (value   "The changed TERM1."))
  (term~set-term term1 term2))

(defun fo~termlist-p (object)
  (declare (edited  "04-AUG-1991 12:10")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff object is a non-empty list of terms."))
  (term~termlist-p object))

(defun fo~nested-termlist-p (object)
  (declare (edited  "04-AUG-1991 12:15")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff object is a nested termlist."
		    "(a simple term, ((((nil)))) and (nil nil nil) are nested termlists, too)."))
  (term~nested-termlist-p object))

(defun fo~subterms (term)
  (declare (edited  "02-AUG-1991 20:46")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If term is a constant or variable NIL,"
		    "if it's an application a list containing the applied function and the arguments,"
		    "if it's an abstraction a list containing the scope."))
  (term~subterms term))

(defun fo~binding (term)
  (declare (edited  "18-OCT-1991 17:18")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "An error if TERM is a constant or variable and has a term-binding"
		    "and a symbol-binding.")
	   (value   "The term-binding or the symbol-binding if TERM has these slots and"
		    "one of them contains something."))
  (term~binding term))

(defun fo~symbol-binding (term)
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A logical symbol, e.g., a variable or constant.")
	   (effect  "None.")
	   (value   "The binding of the shared symbol contained in TERM."))
  (sym~symbol-binding term))

(defun fo~term-binding (term) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "The value of the binding cell of TERM." ))
   (term~term-binding term))
