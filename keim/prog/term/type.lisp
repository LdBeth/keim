;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(in-package  "KEIM")

(mod~defmod type :uses (env keim mod post )
	    :documentation "Datastructures and basic functionality of simple types."
	    :exports (
		      type+type
		      type+complex
		      type+primitive
		      type+constant
		      type+variable
		      type~constant-create
		      type~variable-create
		      type~apply
		      type~abstract
		      type~p
		      type~primitive-p
		      type~constant-p
		      type~variable-p
		      type~complex-p
		      type~ground-p
		      type~n-range
		      type~n-domain
		      type~c-range
		      type~c-domain
		      type~i
		      type~o
		      type~i-p
		      type~o-p
		      type~predicate-p
		      type~function-create
		      type~predicate-create
		      type+substitution
		      type~substitution-create
		      type~subst-domain
		      type~subst-codomain
		      type~subst-get-component
		      type~subst-empty-p
		      type~subst-apply
		      type~unify
		      type~read-new-type-variables
		      type~read-new-type-variable
		      type~enter-type-variable
		      type~read-new-type-constants
		      type~read-new-type-constant
		      type~enter-type-constant
		      type~read-existing-type
		      type~env-lookup
		      )
	    )

#{
\section{Types}\label{mod:type}

Types are a well-known mechanism for syntactically ensuring the
consistency of higher-order logic. \keim\ types are inductively built
up from type variables and type constants by making function types: if
$\beta$, $\alpha^1$, \ldots, $\alpha^n$ are types then there is a
function type, written as ($\alpha^1, \ldots,\alpha^n\rightarrow
\beta$), such that $\beta$ is the type of the range and the cartesian
product of the types $\alpha^1$, \ldots, $\alpha^n$ is the domain. 

In the presence of higher-order types functions do not have a fixed
arity: The function $+$ can be viewed to take 2 arguments and yield
an in dividual, the sum. Therefore it is of the type 
$(\iota,\iota)\to\iota$. On the other hand it can be viewed as a function taking
one argument, e.g. 1 and yielding the 
unary function {\tt inc} which increases each number by 1. Generally we see that the types 
$(\alpha^1,\ldots,\alpha^n\rightarrow ((\beta^1,\ldots,\beta^n\rightarrow \gamma)))$ and   
$(\alpha^1,\ldots,\alpha^n,\beta^1,\ldots,\beta^n\rightarrow \gamma)$ have to be equal. There are two normal forms
for types: 
\begin{itemize}
\item {\em n-normal form}, where the number of occurrences of the function constructor $\rightarrow$ is
minimal (in this case the range of the type is primitive and the number of arguments of each
function is maximal), and 
\item {\em c-normal form} where occurrences of $\rightarrow$ is unary (in this case the number
occurrances of $\rightarrow$ is maximal). This form of types corresponds to the {\em Curried
form of functions}
\end{itemize}
Corresponding to these normal forms many {\bf TYPE} functions have {\bf N} and {\bf C} forms.

 This file contains the primitive types i and o to represent higher order types
 but everything can be a primitive type (for example terms to represent sorts of functions).
 A new primitive type must be a subclass of type+primitive or understand all the methods defined for this class.

 A type is in n-normal-form if the range of all subtypes is primitive (t0 = i or o)
 and in curry-normal-form if the domain of all complex subtypes contains only one element (n = 1).
#}
;; 
;;
;;  Implementation: The types are represented in n-normal-form. If the range of the c-normal-form must be constructed
;;  it is saved into a slot.




(eval-when (load compile eval) 
  (defclass type+type (keim+object)
      ()
    (:documentation "The class of all types in KEIM.")))

(eval-when (load compile eval)
  (defclass type+complex (type+type)
      ((n-domain :initarg :n-domain :reader type=n-domain)	;documented below
       (n-range :initarg :n-range :reader type=n-range)	;documented below
       (c-range :initarg :c-range :reader type=c-range :writer type=write-c-range!))
    (:documentation "The class of function types.")))

(eval-when (load compile eval)
  (defclass type+primitive (keim+name type+type)
    ()
    (:documentation "The class of primitive (non complex) types.")))

(eval-when (load compile eval)
  (defclass type+constant (type+primitive) 
    ()
    (:documentation "The class of constant types e.g. the type of individuals.")))

(defvar type*const-types ())


(eval-when (load compile eval)
  (defclass type+variable (type+primitive)
      ((binding :initform nil :reader type=binding :writer type=write-binding!))
    (:documentation "The class of type variables. Type variables are only used for parsing terms.")))

(defun type=create (n-range n-domain &key c-range)
  (declare (edited  "08-AUG-1991 11:08")
	   (authors RICHTS)
	   (input   "A type t0 and a list of types (t1 ... tn).")
	   (effect  "None.")
	   (value   "The structure representing the type (t1,...,tn) -> t0."))
  (if n-domain
      (make-instance 'type+complex :n-range n-range :n-domain n-domain :c-range c-range)
      n-range))

;;;
;;;;; Constructors

(defun type~constant-create (name)
  (declare (edited  "27-Jan-1992 19:30")
	   (authors Kohlhase)
	   (input   "A print name for the type")
	   (effect  "None.")
	   (value   "A type constant with name NAME.")
	   (example "(type~constant-create 'a) --> A"))
  (or (find name type*const-types :test #'string= :key #'keim~name)
      (let ((newtype (make-instance 'type+constant :name name)))
	(push newtype type*const-types)
	newtype)))


(defun type~variable-create (name)
  (declare (edited  "27-Jan-1992 19:30")
	   (authors Kohlhase)
	   (input   "A print name for the type")
	   (effect  "None.")
	   (value   "A type variable with name NAME.")
	   (example "(type~variable-create 'x) --> X"))
  (make-instance 'type+variable :name name))


(defun type~apply (applied-type argument-types)
  (declare (edited  "08-AUG-1991 11:27")
	   (authors RICHTS)
	   (input   "A type (t1,...,tn) -> t0 and a list of types (s1 ... sm) or a type s1.")
	   (effect  "An error if not (m<=n and t1=s1,...,tm=sm).")
	   (value   "The type (tm+1,...,tn) -> t0  if m<=n and t1=s1,...,tm=sm.")
	   (example "(i,i) -> o   i --> (i) -> o)"))
  (if (listp argument-types)
      (let ((n-domain (type~n-domain applied-type)))
	(if (and (<= (length argument-types) (length n-domain)) (every #'keim~equal argument-types n-domain))
	    (type=create (type~n-range applied-type) (subseq (type~n-domain applied-type) (length argument-types)))
	    (error "Type ~A not appliable to the types ~A." applied-type argument-types)))
      (if (keim~equal (type~c-domain applied-type) argument-types)
	  (type~c-range applied-type)
	  (error "Type ~A not appliable to type ~A." applied-type argument-types))))

(defun type~abstract (abstracted-type argument-types)
  (declare (edited  "08-AUG-1991 11:29")
	   (authors RICHTS)
	   (input   "A type (t1,...,tn) -> t0 and a list of types (s1 ... sm) or a type s1.")
	   (effect  "None.")
	   (value   "The type (s1,...,sm,t1,...,tn) -> t0.")
	   (example "(i,i) -> o   ((i) -> o i) --> ((i) -> o,i,i,i) -> o"))
  (if (listp argument-types)
      (if (type~primitive-p abstracted-type)
	  (type=create abstracted-type argument-types)
	  (type=create (type~n-range abstracted-type) (append argument-types (type~n-domain abstracted-type))))
      (if (type~primitive-p abstracted-type)
	  (type=create abstracted-type (list argument-types) :c-range abstracted-type)
	  (type=create (type~n-range abstracted-type)
		       (cons argument-types (type~n-domain abstracted-type))
		       :c-range abstracted-type))))




;;;;; Predicates

(defmethod keim~equal ((type1 type+type) (type2 type+type))
  (declare (edited  "08-AUG-1991 13:08")
	   (authors RICHTS)
	   (input   "Two types.")
	   (effect  "None.")
	   (value   "True iff the two types are equal."))
   (eq type1 type2))

(defmethod keim~equal ((type1 type+complex) (type2 type+complex))
  (declare (edited  "08-AUG-1991 13:08")
	   (authors RICHTS)
	   (input   "Two complex types.")
	   (effect  "None.")
	   (value   "True iff the two types are equal."))
  (or (eq type1 type2)
      (and (eq (type~n-range type1) (type~n-range type2))
	   (= (length (type~n-domain type1)) (length (type~n-domain type2)))
	   (every #'keim~equal (type~n-domain type1) (type~n-domain type2)))))

(defun type~p (object)
  (declare (edited  "21-OCT-1991 11:09")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is a type."))
  (typep object 'type+type))

(defun type~primitive-p (type)
  (declare (edited  "08-AUG-1991 12:56")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is a type constant or type variable (e.g. i or o).")
	   (example "(i,i) --> o --> NIL"
		     "i --> T"))
  (typep type 'type+primitive))


(defun type~constant-p (type)
  (declare (edited  "24-JAN-1992 18:30")
	   (authors Kohlhase)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is a type-constant."))
  (typep type 'type+constant))

(defun type~variable-p (type)
  (declare (edited  "24-JAN-1992")
	   (authors Kohlhase)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is a type-variable."))
  (typep type 'type+variable))

(defun type~complex-p (type)
  (declare (edited  "08-AUG-1991 12:56")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is complex (i.e. (i1,...in) -> i0 ,n>0).")
	   (example "(i,i) -> o  --> T"))
  (typep type 'type+complex))

(defgeneric type~ground-p (type)
  (declare (edited  "27-AUG-1992 13:11")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is ground, i.e. it doesn't contain any type-variable.")
	   (example "(X,i,i) -> o  --> NIL"
		     "(i,i) -> o --> T"))
  (:method ((type type+complex))
   (and (every #'type~ground-p (type~n-domain type))
	(type~ground-p (type~n-range type))))
  (:method ((type type+constant)) t)
  (:method ((type type+variable)) nil))


;;;
;;;;; Selectors

(defmethod keim~set-name! ((type type+primitive) name)
  (declare (ignore name))
  (error "It is not legal to alter the name of a type."))

(defgeneric type~n-range (type)
  (declare (edited  "08-AUG-1991 13:00")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "The range of the type in its n-normal-form.")
	   (example "(i,i) -> o --> o, compare with {\vb type~c-range}"))
  (:method ((type type+primitive))
   type)
  (:method ((type type+complex))
	   (type=n-range type)))
     
(defgeneric type~n-domain (type)
  (declare (edited  "08-AUG-1991 13:00")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "The list of the domain of the type in its n-normal-form.")
	   (example "(i,i) -> o --> (i i), compare {\vb type~c-domain}"))
  (:method ((type type+primitive))
   nil)
  (:method ((type type+complex))
	   (type=n-domain type)))

(defgeneric type~c-range (type)
  (declare (edited  "08-AUG-1991 13:00")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "The range of the type in its c-normal-form.")
	   (example "(i,i) -> o --> (i) -> o"))
  (:method ((type type+complex))
	   (or (type=c-range type)
	       (let* ((ran (type~n-range type))
		      (dom (cdr (type~n-domain type)))
		      (c-range (if dom
				   (type=create  ran dom)
				   ran)))
		 (type=write-c-range! c-range type)
		 c-range)))
  (:method ((type type+primitive))
	   type))

(defgeneric type~c-domain (type)
  (declare (edited  "08-AUG-1991 13:00")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "The domain (a type) of the type in its c-normal-form.")
	   (example "(i,i) -> o --> i"))
  (:method ((type type+complex))
   (car (type~n-domain type)))
  (:method ((type type+primitive))
   nil))

; Printing

(defmethod print-object ((type type+primitive) stream)
  (let ((format-string 
	 ;; if type o or i, and *print-pretty*, then lowercase it.
	 ;; if type is a keyword, print the colon
	 ;; otherwise just print the symbol's name
	 (cond ((or (type~o-p type) (type~i-p type))
		(if *print-pretty* "~(~A~)" "~A"))
	       ((keywordp (keim~name type)) "~S")
	       (t "~A"))))
  (format stream format-string (keim~name type))))

(defmethod print-object ((type type+complex) stream)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   ))
  (declare (special *print-level* *print-length*))
  (cond ((or (null *print-length*) (< (length (type~n-domain type)) *print-length*))
	 (format stream "(~A~{,~A~}) -> ~A"
		 (car (type~n-domain type))
		 (cdr (type~n-domain type))
		 (type~n-range type)))
	(t (format stream "(~{~A,~}...) -> ~A"
		   (subseq (type~n-domain type) 0 (1- *print-length*))
		   (type~n-range type)))))


#{
\subsection{Primitive types}

The type constants I for individuals and O for truth values are predefined.
They should occur in every environment.
#}

;; Changing definitions to *not* use lower case I and O, which cause 
;; confusion when reading in types.  Instead use capital I and O, but
;; change print-object to print these guys as lower case by default. DAN

(defvar type*i (type~constant-create 'i))
(defvar type*o (type~constant-create 'o))

(defun type~i ()
  (declare (edited  "21-AUG-1991 19:30")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A structure representing the type i."))
  
  type*i)

(defun type~o ()
  (declare (edited  "21-AUG-1991 19:30")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A structure representing the type o."))
  type*o)


(defun type~i-p (type)
  (declare (edited  "23-AUG-1991 12:24")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type i."))
  (eq (type~i) type))

(defun type~o-p (type)
  (declare (edited  "27-JAN-1992 12:24")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the primitive type o."))
  (eq (type~o) type))


(defgeneric type~predicate-p (type)
  (declare (edited  "27-JAN-1992 12:24")
	   (authors RICHTS)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "True iff the type is the form type (A) -> o")
	   (example "(i,i) -> o --> T"
		     "(i,i) -> i --> NIL"))
  (:method ((type type+primitive)) nil)
  (:method ((type type+complex)) (keim~equal (type~o) (type~n-range type))))

#{
\subsection{Constructors for first order function and predicate types}
#}

(defun type~function-create (arity)
  (declare (edited  "08-AUG-1991 11:13")
	   (authors RICHTS)
	   (input   "A number n.")
	   (effect  "None.")
	   (value   "The structure representing the type (i1,...,in) -> i.")
	   (example "3 --> (i,i,i) -> i"))
  (type~abstract (type~i) (make-list arity :initial-element (type~i))))

(defun type~predicate-create (arity)
  (declare (edited  "08-AUG-1991 11:13")
	   (authors RICHTS)
	   (input   "A number n.")
	   (effect  "None.")
	   (value   "The structure representing the type (i1,...,in) -> o.")
	   (example "3 --> (i,i,i) -> o"))
  (type~abstract (type~o) (make-list arity :initial-element (type~i))))


;;Bindings for Type Variables

(defvar type*binding-list nil)

(defun type=create-binding-substitution ()
  (declare (edited  "07-NOV-1991 12:01")
	   (authors KOHLHASE)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A substitution that contains all variables in the actual binding-list as its domain"
		    "and the terms in the binding slots of these variables as codomain."
		    "The codomain-terms are built by inserting the type=bindings, i. e. the binding slots are regarded."))
  (do* (domain
	codomain
	(binding-list-tail type*binding-list (cdr binding-list-tail))
	(type (car binding-list-tail) (car binding-list-tail)))
       ((null binding-list-tail) (type~substitution-create domain codomain))
    (when (type~variable-p type)
      (push type domain)
      (push (type=insert-bindings (type=binding type)) codomain))))

(defgeneric type=insert-bindings (type)
  (declare (edited  "05-NOV-1991 10:55")
	   (authors KOHLHASE)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "A new type where the bindings of all subterms of TYPE are inserted into the term recursively."))
  (:method ((var type+variable))
	   (if (null (type=binding var))
	       var	   
	       (type=insert-bindings (type=binding var))))
  (:method ((const type+constant))
	   const)
  (:method ((type type+complex))
	   (type~abstract (type=insert-bindings (type~n-range type))
			  (mapcar #'type=insert-bindings (type~n-domain type)))))





#{
\subsection{Type substitutions}
#}

(eval-when (load compile eval)
  (defclass type+substitution (keim+object)
      ((domain :initarg :domain :initform nil
	       :reader type=subst-domain :writer type=write-subst-domain)
       (codomain :initarg :codomain :initform nil
		 :reader type=subst-codomain :writer type=write-subst-codomain))
    (:documentation "A type substitution")))

(defun type~substitution-create (domain codomain)
  (declare (edited  "24-JUN-1992 12:56" )
	   (authors HUANG KOHLHASE)
	   (input   "{\vb domain} is a list of type variables, and {\vt codomain} is a list of types, both list must be of the same length")
	   (effect  "A new type-substitution is created.")
	   (value   "The new type substitution.")
	   (example "Let X and Y be type variables. "
		     "(X Y) ((i,i) -> o (i) -> o)  --> {X -> (i,i) -> o; Y -> (i) -> o}"))
  (make-instance 'type+substitution :domain domain :codomain codomain))

(defgeneric type~subst-domain (subst)
  (declare (edited  "24-JUN-1992 16:36" )
	   (authors KOHLHASE )
	   (input   "A type-substitution." )
	   (effect  "None.")
	   (value   "The domain of SUBST.")
	   (example "{X -> (i,i) -> o; Y -> (i) -> o}"
		     "--> (X Y)"))
  (:method ((subst type+substitution))
	   (type=subst-domain subst)))

(defgeneric type~subst-codomain (subst)
  (declare (edited  "24-JUN-1992 16:36" )
	   (authors KOHLHASE )
	   (input   "A type-substitution." )
	   (effect  "None.")
	   (value   "The codomain of SUBST.")
	   (example "{(i,i) -> o -> X; (i) -> o -> (X,i,i) -> o}"
		     "-->(X (X,i,i) -> o)"))
  (:method ((subst type+substitution))
	   (type=subst-codomain subst)))

;Start here again

(defgeneric type~subst-get-component (variable substitution)
  (declare (edited  "09-AUG-1992 15:29")
	   (authors RICHTS KOHLHASE)
	   (input   "A type variable and a type substitution.")
	   (effect  "None.")
	   (value   "The image VARIABLE or NIL if there is none.")
	   (example "Y {X -> (i,i) -> o; Y -> (i) -> o} --> (i) -> o"))
  (:method ((var type+type) (subst type+substitution))
   (error "~A is not a type variable" var))
  (:method ((variable type+variable) (substitution type+substitution))
   (some #'(lambda (var type)
	     (if (keim~equal var variable) type nil))
	 (type~subst-domain substitution)
	 (type~subst-codomain substitution))))

(defun type~subst-empty-p (subst)
  (declare (edited  "27-AUG-1992 13:16")
	   (authors RICHTS)
	   (input   "A type-substitution.")
	   (effect  "None.")
	   (value   "True iff SUBST is empty, i.e. its domain is NIL."))
  (null (type~subst-domain subst)))


(defmethod print-object ((subst type+substitution) stream)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   ))
  (declare (special *print-level* *print-length*))
  (do ((domain (type~subst-domain subst) (cdr domain))
       (codomain (type~subst-codomain subst) (cdr codomain))
       (n (if *print-length* *print-length* -1))
       (komma "{" "; "))
      ((or (zerop n) (null domain))
       (if (null domain)
	   (format stream "}")
	   (format stream " ...}")))
    (format stream "~A~A -> ~A" komma (car domain) (car codomain))))


(defgeneric type~subst-apply (subst type)
	   (declare (edited  "09-AUG-1992 15:32")
		    (authors RICHTS )
		    (input   "A type-substitution and a type.")
		    (effect  "None.")
		    (value   "The instantiated type.")
		    (example "{X -> (i,i) -> o; Y -> (i) -> o} (X,i,i) -> o"
			      "--> ((i,i) -> o,i,i) -> o"))
	   (:method ((subst type+substitution) (list list))
	    (mapcar #'(lambda (object) 
			(type~subst-apply subst object))
		    list))
	   (:method ((subst type+substitution) (type type+complex))
		    (type~abstract (type~subst-apply subst (type~n-range type))
				   (mapcar #'(lambda (domain-type) 
					       (type~subst-apply subst domain-type))
					   (type~n-domain type))))
	   (:method ((subst type+substitution) (type type+constant))
		    type)
	   (:method ((subst type+substitution) (type type+variable))
		    (or (type~subst-get-component type subst)
			type))			;j: diese Zeile hinzugefuegt
	   (:method ((subst1 type+substitution) (subst2 type+substitution))
		    (type~substitution-create (type~subst-domain subst1)
					      (mapcar #'(lambda (type) (type~subst-apply subst2 type))
						      (type~subst-codomain subst2)))))



#{
\subsection{Type unification}
#}

(defun type~unify (type1 type2)
  (declare (edited  "09-AUG-1992 15:47")
	   (authors RICHTS)
	   (input   "Two types or list of types of equal length.")
	   (effect  "None.")
	   (value   "A substitution that is a most general unifier for TYPE1 and TYPE2, or NIL if it does not exist.")
	   (example "(i,i) -> o  (Y,i) -> o  -->  {Y -> i}"
		     "(Y,i) -> o  (X,i,i) -> o --> NIL"))
  (unwind-protect
      (progn
	 (setq type*binding-list nil)
	 (if (if (listp type1)
		 (type=unify-rec (car type1) (car type2) (cdr type1) (cdr type2))
		 (type=unify-rec type1 type2 nil nil))
	     (type=create-binding-substitution)))
    (mapc #'(lambda (type-variable)		;j: cleanup-forms neu
	      (type=write-binding! nil type-variable))
	  type*binding-list)
    (setf type*binding-list nil)))

(defgeneric type=unify-rec (type1 type2 typelist1 typelist2)
  (:method ((type1 type+type) (type2 type+type) typelist1 typelist2)
      nil)
  (:method ((t1 (eql nil)) (t2 (eql nil)) typelist1 typelist2)
	   (when (or typelist1 typelist2)
	     (error "Programmierfehler: Es sind noch Typen da."))
	   t)
  (:method ((variable1 type+variable) (variable2 type+variable) typelist1 typelist2)
	   (let ((binding1 (type=binding variable1))
		 (binding2 (type=binding variable2)))
	     (cond ((keim~equal variable1 variable2)
		    (type=unify-rec (car typelist1) (car typelist2) (cdr typelist1) (cdr typelist2)))
		   ((and binding1 binding2)
		    (type=unify-rec binding1 binding2 typelist1 typelist2))
		   (binding1
		    (type=unify-rec binding1 variable2 typelist1 typelist2))
		   (binding2
		    (type=unify-rec variable1 binding2 typelist1 typelist2))
		   (t (push variable1 type*binding-list) (type=write-binding! variable2 variable1)
		      (type=unify-rec (car typelist1) (car typelist2) (cdr typelist1) (cdr typelist2))))))
  (:method ((variable type+variable) (type type+type) typelist1 typelist2)
	   (cond ((type=binding variable)
		  (type=unify-rec (type=binding variable) type typelist1 typelist2))
		 ((type=occurs-p variable type) nil)
		 (t (push variable type*binding-list) (type=write-binding! type variable)
		    (type=unify-rec (car typelist1) (car typelist2) (cdr typelist1) (cdr typelist2)))))
  (:method ((type type+type) (variable type+variable) typelist1 typelist2)
	   (type=unify-rec variable type typelist1 typelist2))
  (:method ((constant1 type+constant) (constant2 type+constant) typelist1 typelist2)
	   (if (keim~equal constant1 constant2)
	       (type=unify-rec (car typelist1) (car typelist2) (cdr typelist1) (cdr typelist2))
	       nil))
  (:method ((comp1 type+complex) (comp2 type+complex) typelist1 typelist2)
   (type=unify-rec (type~c-domain comp1) (type~c-domain comp2)
		   (cons (type~c-range comp1) typelist1) (cons (type~c-range comp2) typelist2))))

#|
	   (let ((domain1 (type~n-domain comp1))
		 (domain2 (type~n-domain comp2)))
	     (if (/= (length domain1) (length domain2))
		 nil
		 (type=unify-rec (type~n-range comp1)
				      (type~n-range comp2)
				      (append domain1 typelist1)
				      (append domain2 typelist2))))))|#



(defgeneric type=occurs-p (variable obj)
  (:method ((variable1 type+variable) (variable2 type+variable))
	   (cond ((keim~equal variable1 variable2) t)
		 ((type=binding variable2) (type=occurs-p variable1 (type=binding variable2)))
		 (t nil)))
  (:method (variable (constant type+constant))
	   (declare (ignore variable))
	   nil)
  (:method ((variable type+variable) (comp type+complex))
	   (or (type=occurs-p variable (type~n-range comp))
	       (some #'(lambda (argument)
			 (type=occurs-p variable argument))
		     (type~n-domain comp)))))


#{
\subsection{\post\ Input and Output}
#}

(defmethod post~print ((type type+primitive) stream)
  (format stream "~A" (keim~name type)))

(defmethod post~print ((type type+complex) stream)
  (format stream "(")
  (post~print (type~n-range type) stream)
  (format stream " ")
  (post~print (reverse (type~n-domain type)) stream)
  (format stream ")"))

(defmethod env~post-print (key (type type+variable) stream)
  (format stream "~&(type-variables ~A)~%" (keim~name type))
  )

(defmethod env~post-print (key (type type+constant) stream)
  (format stream "~&(type-constants ~A)~%" (keim~name type))
  )


(defun type~read-new-type-variables (typevars env)
  (declare
   (authors nesmith)
   (input "a POST representation of a list of type variables and an environment")
   (effect "Reads the type variable declarations into the environment")
   (value "undefined")
   (example "(type~read-new-type-variables '(type type1 type2) env)"))
  (post~read-object typevars env :type-variables))

(defun type~read-new-type-variable (typevar env)
  (declare
   (authors nesmith)
   (input "a POST representation of a type variable and an environment")
   (effect "Reads the type variable declaration into the environment")
   (value "undefined"))
  (post~read-object typevar env :type-variable))


(defmethod post~read-object (typevars (env env+environment) (indicator (eql :type-variables)))
  (dolist (typevar typevars nil)
    (type~enter-type-variable typevar env)))
	    
(defmethod post~read-object (typevar (env env+environment) 
			    (indicator (eql :type-variable)))
  (type~enter-type-variable typevar env))

(defun type~enter-type-variable (symbol env)
  (declare
   (authors nesmith)
   (input "a symbol representing a type variable and an environment")
   (effect "Reads the type variable declaration into the environment")
   (value "undefined")
   (example "(type~enter-type-variable 'type3 env)"))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((and thing (not (type~variable-p thing)))
	   (error "Can't declare ~S as a type variable, because it already exists in the environment as a ~A. " symbol (class-name (class-of thing))))
	  (thing nil)
	  ((not (symbolp symbol))
	   (error "Can't declare ~S as a type variable, because it is not a symbol." symbol))
	  (t (env~enter symbol (type~variable-create symbol) env)
	     nil))))

(defun type~read-new-type-constants (typeconsts env)
  (declare
   (authors nesmith)
   (input "a POST representation of a list of type constants
and an environment")
   (effect "Reads the type constant declarations into the environment")
   (value "undefined"))
  (post~read-object typeconsts env :type-constants))

(defun type~read-new-type-constant (typeconst env)
  (declare
   (authors nesmith)
   (input "a POST representation of a type constant and an environment")
   (effect "Reads the type constant declaration into the environment")
   (value "undefined"))
  (post~read-object typeconst env :type-constant))


(defmethod post~read-object (typevars (env env+environment) 
			     (indicator (eql :type-constants)))
  (dolist (typevar typevars nil)
    (type~enter-type-constant typevar env)))
	    
(defmethod post~read-object (typevar (env env+environment) 
			    (indicator (eql :type-constant)))
  (type~enter-type-constant typevar env))


(defun type~enter-type-constant (symbol env)
  (declare
   (authors nesmith)
   (input "a Lisp symbol and an environment")
   (effect "Declares the symbol as a type constant in the environment")
   (value "undefined"))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((and thing (not (type~constant-p thing)))
	   (error "Can't declare ~S as a type constant, because it already exists in the environment as a ~A. " symbol (class-name (class-of thing))))
	  (thing nil)
	  ((not (symbolp symbol))
	   (error "Can't declare ~S as a type constant, because it is not a symbol." symbol))
	  (t (env~enter symbol (type~constant-create symbol) env)
	     nil))))

; existing-types

(defun type~read-existing-type (type env)
  (declare
   (authors huang nesmith)
   (input "A POST representation of an existing TYPE and an environment ENV.")
   (effect "None")
   (value "The type is looked up in the environment and a type is returned.
If the type is not in the environment an error will be signaled.")
   (example "(type~read-existing-type '(i i) env) --> (i) -> i"))
  (type~env-lookup type env))

(defmethod post~read-object ((type symbol) (env env+environment) 
			    (indicator (eql :existing-type)))
  (type~env-lookup type env))

(defmethod post~read-object ((type cons) (env env+environment) 
			    (indicator (eql :existing-type)))
  (type~env-lookup type env))

(defgeneric type~env-lookup (type env)
  (declare 
   (authors NESMITH)
   (input   "A POST representation of an existing TYPE and an environment ENV.")
   (effect  "None.")
   (value   "The type is looked up in the environment and a type is returned.
If the type is not in the environment an error will be signaled.")
   (example "(type~read-existing-type '(i i) env) --> (i) -> i"))
  (:method ((type list) (env env+environment))
	   (if (= (length type) 1)
	       (type~env-lookup (car type) env)
	     (type=env-lookup-complex type env)))
  (:method ((type symbol) (env env+environment))
	   (type=env-lookup-symbol type env)))

(defun type=env-lookup-symbol (type env)
  (declare
   (authors nesmith)
   (input "an object and an environment")
   (effect "Looks up the object in environment, returns it if it is associated with a type, otherwise error")
   (value "undefined"))
  (let ((obj (env~lookup-object type env)))
    (cond ((not obj) 
	   (error "~A is not an existing type." type))
	  ((not (type~p obj))
	   (error "~A is declared in the environment as a ~A, not as a type."
		  type (class-of obj)))
	  (t obj))))


(defun type=env-lookup-complex (type env)
  (declare
   (authors nesmith)
   (input "an object and an environment")
   (effect "Looks up the object in environment, returns it if it is associated with a type, otherwise error")
   (value "undefined"))
  (if (consp type)
      (type~abstract (type~env-lookup (car type) env)
		     (mapcar #'(lambda (x) (type~env-lookup x env))
			     (reverse (cdr type))))
    (error "complex type  ~S should be a cons" type)))


    

