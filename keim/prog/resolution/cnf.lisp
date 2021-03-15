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

(in-package :keim)

(mod~defmod cnf :uses (abstr appl assum cl  conc delta env 
			     hop keim lit mod poly pos post prob 
			     proof res simpl sksym sym term termc 
			     top type )
	    :documentation "Conjunctive normal forms and splitting"
	    :exports (
		      cnf~normalize-problem!
		      cnf~normalize-problem
		      cnf~normalize-list
		      cnf~normalize-and-split
		      cnf~normalize
		      cnf+conclusion-clause
		      cnf+axiom-clause
		      cnf~conclusion-clause-p
		      cnf~axiom-clause-p
		      )
	    )

#{\section{Conjunctive normal forms}
\label{mod:cnf}

This module provides algorithms and interface functions for the
normilzation of formulae to conjunctive normal form like it is used in
resolution.#}


(defvar cnf*env ())
;; Global variable, that is bounded to an environment in the interface
;; functions. All functions in this module refer to this environment
;; if they need to.

(defvar cnf*delta-relation ())
;; Global variable that is bounded to the actual delta-relation.


(defvar cnf*skolem-constants ())
;; Global variable that stores all new skolem constants.


;; ................ provisorisch ...............

(defvar cnf*simplification-in-splitting-on nil)


;; -----------------------------------------------------------------------------------
;;;begin{latex}
;;;\subsection{Higher-level interfaces}
;;;end{latex}
;; -----------------------------------------------------------------------------------

(defun cnf~normalize-problem! (problem &key (delta (delta~create-relation)))
  (declare (edited  "10-FEB-1993 19:22")
	   (authors SCHEJA)
	   (input   "A problem. Keyword argument for a delta relation or NIL"
		    "(default is a new and empty delta relation).")
	   (effect  "The problem is destructively changed to a 
resolution proof data-structure
(i.e. its class is changed to RES+PROOF),
the clauses slot is set to the clause normal form of the problem and the
delta relation slot is set to DELTA. If DELTA is not NIL it is changed to the delta
relation that is linked with the clause normal form. The new skolem
constant and clauses are inserted into the environment of PROBLEM.")
	   (value   "The changed PROBLEM."))
  (change-class problem 'res+proof)
  (res~set-proof-clauses! problem
			  (cnf~normalize-list (cons (prob~conclusion problem)
						    (prob~assumptions problem))
					      (prob~environment problem)
					      :delta delta))
  (res~set-proof-empty-clause! problem NIL)
  (proof~set-steps! problem NIL)
  (when delta (res~set-proof-delta-relation! problem delta))
  problem)

(defun cnf~normalize-problem (problem &key (delta (delta~create-relation)))
  (declare (edited  "10-FEB-1993 19:22")
	   (authors SCHEJA)
	   (input   "A problem, that contains conclusions,
assumptions and an environment. Keyword argument for a delta relation (defaults
to new and empty delta relation).")
	   (effect  "None.")
	   (value   "A new instance of a resolution proof, whose
clauses slot is set to the clause normal form of the problem. Its
delta-relation slot is set to the delta relation that is linked
with the clause normal form, unless the delta parameter is set to nil."))
  (let* ((name (keim~name problem))
	 (status (prob~status problem))
	 (env (env~copy (prob~environment problem)))
	 (ass (prob~assumptions problem))
	 (conc (list (prob~conclusion problem)))
	 (new-problem (prob~create name status env ass conc)))
    (cnf~normalize-problem! new-problem :delta delta)))

(defun cnf~normalize-list (list env &key (delta nil))
  (declare (edited  "10-FEB-1993 18:23")
	   (authors SCHEJA)
	   (input   "A list of assumptions and conclusions, an environment and optionally a delta-relation.")
	   (effect  "The new skolem constants are entered in the environment.
If a delta relation is given, the new pairs are added .")
	   (value   "A list of lists of clauses, each sublist corresponding to the
clausal normal form of the assumption resp.\ negated conclusion."))
  (do* ((wffs list (rest wffs))
	(formula (first wffs) (first wffs))
	(result ()))
       ((null wffs) (nreverse result))
    (setq result (nconc (if (cnf=closed-wff-o-p formula)
			    (cond  ((assum~p formula)
				    (mapcar #'(lambda (clause)
						(change-class clause  'cnf+axiom-clause))
					    (cnf~normalize formula env :delta delta)))
				   ((conc~p formula)
				    (mapcar #'(lambda (clause)
						(change-class clause 'cnf+conclusion-clause))
					    (cnf~normalize formula env :delta delta :negation t)))
				   (t (error "The list ~A should contain only assumptions and conclusions." list)))
			    (error "The formula ~A in the list of assumptions and conclusions ~A is not of type o or not closed. ~
                                   Therefore it doesn't match the input of this routine." formula list))
			result))))

(defun cnf=closed-wff-o-p (formula)
  (and (type~o-p (term~type formula))
       (null (top~free-variables formula))))

;; ---------------------------------------------------------------------------
;;;begin{latex}
;;;\subsection{Splitting}
;;;end{latex}
;; ---------------------------------------------------------------------------



(defun cnf~normalize-and-split (formula splitdepth cnf*env)
  (declare (edited  "13-AUG-1992 11:57")
	   (authors SCHEJA)
	   (input   "A formula that is simplified with simpl~simplify, an integer that indicates the split depth,
 and the  environment the formula comes from." )
	   (effect  "The skolem constants are added to the environment.")
	   (value   "A list of term-object-set of clauses, that means the disjunction of
the splitted parts of formula. Formula is unsatisfiable iff each element of the list is unsatisfiable."))
  (mapcar #'(lambda (x) (cnf=normalize x nil nil))
	  (cnf=split  formula splitdepth)))



(defun cnf=split (formula splitdepth)
  (declare (edited  "05-SEP-1992 18:14")
	   (authors SCHEJA)
	   (input   "A formula and as splitdepth an integer.")
	   (effect  "None.")
	   (value   "The until splitdepth splitted formula, that means a list of formulas,
the disjunction of it is refutational equivalent to formula (it is partially skolemized)."))
  (if (= 0 splitdepth) (list formula)
      (cond ((cnf=literal-p formula) (list formula))
	    ((termc~negation-p formula) (cnf=split (cnf=push-negation formula) splitdepth))
	    ((termc~existential-quantification-p formula)
	     (cnf=split (cnf=skolemize-one-level formula) splitdepth))
	    ((termc~universal-quantification-p formula)
	     (cnf=split-universal-quantor formula splitdepth))
	    ((cnf=term-pi-term formula) (list formula))
	    ;; Behandlung kommt spaeter.
	    (t (let ((arguments (appl~arguments formula)))
		 (cond ((termc~implication-p formula)
	                (cnf=split (appl~create (cnf=environment-get 'or)
							    (list (appl~create (cnf=environment-get 'not)
											   (list (first arguments)))
								  (second arguments)))
				   splitdepth))
		       ((termc~conjunction-p formula) (cnf=disjunctive-form formula (1- splitdepth)))
		       ((termc~disjunction-p formula) (mapcan #'(lambda (x) (cnf=split x (1- splitdepth)))
							     (cnf=operator-parts (appl~arguments formula) 'termc~disjunction-p)))
		       ((termc~equivalence-p formula) (mapcan #'(lambda (x) (cnf=split x (1- splitdepth)))
							     (cnf=equivalences-into-disjunctions
							      (cnf=operator-parts (appl~arguments formula) 'termc~equivalence-p))))
		       (t (error "~a is nothing what might be accepted by this routine."))))))))




(defun cnf=split-universal-quantor (formula splitdepth)
  (declare (edited  "21-DEC-1992 15:03")
	   (authors SCHEJA)
	   (input   "A universal quantification and as splitdepth an integer.")
	   (effect  "None.")
	   (value   "The down to splitdepth splitted formula."))
  (let* ((scope (termc~quantification-scope formula))
	 (variable (termc~quantification-bound-variable formula))
	 (position (cnf=subformula-position scope #'cnf=no-free-var-in splitdepth))
	 (position-list (when (not (null position)) (term~subterm-positions (term~at-position scope position) scope))))
    ;;(cnf=other-occurrences scope position splitdepth)
    (cond ((null position) (list formula))
	  ((pos~empty-p position) (cnf=split scope (1- splitdepth)))
	  ;; ie., scope doesn't contain the bound variable of the quantification
	  (t (let ((a (term~copy scope))
		   (b (term~copy scope)))
	       (if cnf*simplification-in-splitting-on
		   (progn
		     (mapcar #'(lambda (x) (setq a (top~replace-at-position! a x (cnf=environment-get 'true)))) position-list)
		     (setq a (simpl~simplify a cnf*env))
		     (mapcar #'(lambda (x) (setq b (top~replace-at-position! b x (cnf=environment-get 'false)))) position-list)
		     (setq b (simpl~simplify b cnf*env)))
		   (progn
		     (mapcar #'(lambda (x) (setq a (cnf=simplify! (top~replace-at-position! a x (cnf=environment-get 'true)) x))) position-list)
		     (mapcar #'(lambda (x) (setq b (cnf=simplify! (top~replace-at-position! b x (cnf=environment-get 'false)) x))) position-list)))
	       (setq a (cnf=universal-quantification-create variable a))
	       (setq b (cnf=universal-quantification-create variable b))
	       (cond ((keim~equal a b) (cnf=split a (1- splitdepth)))
		     ((cnf=subsumes a b) 
		      (append (cnf=split a (1- splitdepth))
			      (appl~create (cnf=environment-get 'and)
						       (list (cnf=negate (term~at-position scope position))
							     (cnf=split b (1- splitdepth))))))
		     ((cnf=subsumes b a)
		      (append (appl~create (cnf=environment-get 'and)
						       (list (term~at-position scope position)
							     (cnf=split a (1- splitdepth))))
			      (cnf=split b (1- splitdepth))))
		     (t (let ((p (term~at-position scope position)))
			  (append
			   (mapcar #'(lambda (x) (appl~create (cnf=environment-get 'and) (list p x)))
				   (cnf=split a (1- splitdepth)))
			   (mapcar #'(lambda (y) (appl~create (cnf=environment-get 'and)
									  (list (cnf=negate p) y)))
				   (cnf=split b (1- splitdepth))))))))))))


(defun cnf=subformula-position (formula test termdepth)
  (declare (edited  "21-DEC-1992 18:22")
	   (authors SCHEJA)
	   (input  "A formula that corresponds to the n-normalform of her
type, is betareduced and which quantors are eta-expanded, a predicate and as termdepth an integer." )
	   (effect  "None.")
	   (value   "The position of the first subterm of formula, that fulfills the test
and is not deeper in formula as termdepth ('first' in the natural order on positions).
If no such subterm exists, nil."))
  (if (funcall test formula)
      (pos~empty)
      (cond ((<= termdepth 0) nil)
	    ((termc~atom-p formula) nil)
	    ((cnf=quantification-p formula)
	     (let* ((scope (termc~quantification-scope formula))
		    (position (cnf=subformula-position scope test (1- termdepth))))
	       (if (null position)
		   nil
		   (pos~add-front 1 (pos~add-front 0 position)))))
	    ((termc~negation-p formula)
	     (let ((position (cnf=subformula-position (first (appl~arguments formula)) test (1- termdepth))))
	       (if (null position)
		   nil
		   (pos~add-front 1 position))))
	    (t (let* ((arguments (appl~arguments formula))
		      ;; now, op must be a binary
		      ;; operator
		      (position (cnf=subformula-position (first arguments) test (1- termdepth))))
		 (if (null position)
		     (progn
		       (setq position (cnf=subformula-position (second arguments) test (1- termdepth)))
		       (if (null position)
			   nil
			   (pos~add-front 2 position)))
		     (pos~add-front 1 position)))))))		       
	  

      
(defun cnf=simplify! (formula position)
  (declare (edited  "22-DEC-1992 15:44")
	   (authors SCHEJA)
	   (input  "A beta-reduced formula,which quantors are eta--expanded, and a (non empty) position, the subterm
at position being a truthvalue." )
	   (effect "Formula is destroyed.")
	   (value  "The simplified formula."))
  (if (pos~empty-p (pos~butlast position))
      ;; ie. simplification on top-level necessary
      (cnf=simplified-formula formula position)
      (do ((sequence (pos~butlast position) (pos~butlast sequence))
	   (nextposition (pos~last position) (pos~last sequence))
	   (help ())
	   (testterm ()))
	  ((pos~empty-p sequence) formula)
	(setq help (term~at-position formula sequence))
	(setq testterm (term~at-position help nextposition))
	(cond ((abstr~p help) ())
	      ((appl~p help)
	       (if (or (termc~equivalence-p help)
		       (termc~conjunction-p help)
		       (termc~disjunction-p help))
		   (progn (top~replace-at-position! formula sequence (cnf=simplified-formula help nextposition))
			  (return formula))
		   ;; no further simplification possible
		   (if (termc~negation-p help)
		       (top~replace-at-position! formula sequence (cnf=simplified-formula help nextposition))
		       (error "~a doesn't match the input of this routine." formula))))
	      (t (error "~a, ~a doesn't match the input of this routine." formula position))))))


(defun cnf=simplified-formula (formula position)
  (declare (edited  "23-DEC-1992 15:54")
	   (authors SCHEJA)
	   (input   "A conjunction, disjunction, equivalence or negation,
where the argument denoted by position are a truthvalue.")
	   (effect  "None.")
	   (value   "The simplified formula."))
  (let ((testterm (term~at-position formula position)))
    (cond
      ((termc~negation-p formula)
       (cnf=dual-truthvalue testterm))
      ((or (and (termc~conjunction-p formula) (cnf=false-p testterm))
	   (and (termc~disjunction-p formula) (cnf=true-p testterm)))
       testterm)
      ((or (and (termc~conjunction-p formula) (cnf=true-p testterm))
	   (and (termc~disjunction-p formula) (cnf=false-p testterm)))
       (term~at-position formula (pos~list-position (list (- 3 (pos~first position))))))
      ((termc~equivalence-p formula)
       (if (cnf=true-p testterm)
	   (term~at-position formula (pos~list-position (list (- 3 (pos~first position)))))
	   (cnf=negate  (term~at-position formula (pos~list-position (list (- 3 (pos~first position))))))))
      (t (error "~a doesn't match the input of this routine.")))))
		
	
    
(defun cnf=subsumes (formula1 formula2)
  (declare (edited  "22-DEC-1992 18:36")
	   (authors SCHEJA)
	   (input   "Two formulas.")
	   (effect  "None.")
	   (value   "It should be: T, if formula1 subsumes formula2.
Up to now it is implementes as term~equal."))
  (term~equal formula1 formula2))


(defun cnf=disjunctive-form (formula splitdepth)
  (declare (edited  "05-SEP-1992 18:26")
	   (authors SCHEJA)
	   (input   "A conjunction and as splitdepth an integer.")
	   (effect  "None.")
	   (value   "A list (x1 ... xn) of formulas, such that formula is equivalent
to (or x1 ... xn), resp. (formula) if this is not possible."))
  (let* ((arguments (appl~arguments formula))
	 (split-first (cnf=split (first arguments) splitdepth))
	 (split-second (cnf=split (second arguments) splitdepth)))
    (cnf=cross-conjunctions split-first split-second)))

(defun cnf=cross-conjunctions (list1 list2)
  (declare (edited  "09-OCT-1992 17:45")
	   (authors SCHEJA)
	   (input   "Two lists of formula.")
	   (effect  "None.")
	   (value   "A list of formulas, their disjunction being equivalent to
the conjunction of the disjunctions of the elements of list1 and that of list2."))
  (do ((help list1 (rest help))
       (result () (append result (mapcar #'(lambda (x)
					     (appl~create (cnf=environment-get 'and) (list (first help) x)))
					 list2))))
      ((null help) result)))


(defun cnf=equivalences-into-disjunctions (arglist)
  (declare (edited  "09-OCT-1992 18:18")
	   (authors SCHEJA)
	   (input   "A list of formulas.")
	   (effect  "None.")
	   (value   "A list of formulas, their disjunction being equivalent to
the equivalence of the elements of arglist."))
  (let* ((list (cnf=odd-and-even-sublists arglist))
	 (even-list (mapcar #'(lambda (z) (mapcar #'(lambda (x) (cnf=negate x))
						  z))
			    (second list)))
	 (complement-list (cnf=complements-of-sublists (second list) arglist)))
    
    (mapcar #'(lambda (x y) (cnf=multi-application-create (cnf=environment-get 'and) (append x y)))
	    even-list
	    complement-list)))



(defun cnf=operator-parts (arglist operatortest)
  (declare (edited  "13-AUG-1992 12:30")
	   (authors SCHEJA)
	   (input   "A list (x1 ... xn) of formulas and an operatortest for one of the binary connectives and, or, equiv." )
	   (effect  "None.")
	   (value   "(y1 ... yn), where yi=xi if (operatortest xi) is nil, else
  yi= (cnf=operator-parts (appl~arguments xi) operatortest). " ))
  (mapcan #'(lambda (x) (if (funcall operatortest x)
			    (cnf=operator-parts (appl~arguments x)  operatortest)
			    (list x)))
	  arglist))
 


(defun cnf=skolemize-one-level (formula)
  (declare (edited  "11-SEP-1992 12:30")
	   (authors SCHEJA)
	   (input   "An existential quantification.")
	   (effect  "Error, if formula is no existential quantification.")
	   (value   "The scope of formula, where the bound variable is replaced by a new constant."))

  (if (termc~existential-quantification-p formula)
      (let* ((variable (termc~quantification-bound-variable formula))
	     (scope (termc~quantification-scope formula))
	     (skolem-term (cnf=skolem-constant-create  (term~type variable) 0)))
	(cnf=introduce-skolem-term scope variable skolem-term))
      (error "~a doesn't match the input of this routine, it must be an existential quantification." formula)))



(defun cnf=get-skolem-term-and-extend-env (variable varlist)
  (declare (edited  "15-DEC-1992 18:59")
	   (authors SCHEJA)
	   (input   "A list of variables.")
	   (effect  "The created skolemconstant is added to cnf*env.")
	   (value   "A new skolem-constant if varlist is empty,
else an application of a new skolem-constant to the list of variables;
the type of the result being the type of variable."))
  (let* ((domain-type (mapcar #'term~type  (reverse varlist)))
					;(skolem-constant (sym~env-enter-constant  (gentemp "SK") (type~abstract (term~type variable) domain-type) cnf*env))
	 (skolem-constant (cnf=skolem-constant-create  (type~abstract (term~type variable) domain-type) (length varlist)))
	 )
    (sym~env-enter-constant  (intern (sym~name skolem-constant)) (term~type skolem-constant) cnf*env)
    (cnf=add-skolem-constant skolem-constant)
    (if (null varlist)
	skolem-constant
	(appl~create skolem-constant (reverse varlist)))))


(defun cnf=skolem-constant-create (type arity)
  (declare (edited  "29-MAR-1993 17:05")
	   (authors SCHEJA)
	   (input   "A type and a positive integer.")
	   (effect  "None.")
	   (value   "A new skolem constant with type type and arity arity."))
  (sksym~create (gentemp "SK") type arity))


(defun cnf=introduce-skolem-term (formula variable skolem-term)
  (declare (edited  "14-SEP-1992 14:11")
	   (authors SCHEJA)
	   (input   "A formula, a variable and a skolem-term.")
	   (effect  "None.")
	   (value   "A new variant of formula, where variable is substituted by skolem-term."))
  (let ((help-copy (term~copy-with-properties formula)))
    (top~replace-free-variables!! help-copy (list variable) (list skolem-term))))
 
(defun cnf=push-negation (formula)
  (declare (edited  "12-JAN-1993 12:27")
	   (authors SCHEJA)
	   (input   "A negation and an integer.")
	   (effect  "None.")
	   (value   "A version of formula, where negations are pushed until the
next negation or until term-depth depth.
examples: (not (not f)) --> f.
          (not (or a b)) --> (and (not a) (not b)), if termdepth=1."))

  (if (cnf=literal-p formula)
      formula
      (let* ((next-formula (first (appl~arguments formula)))
	     (arguments (appl~arguments next-formula))
	     (operator (term~top next-formula)))
	(cond ((termc~negation-p next-formula) (first arguments))
	      ((or (termc~conjunction-p next-formula)
		   (termc~disjunction-p next-formula))
	       (appl~create (cnf=dual-operator operator)
					(list
					 (cnf=negate (first arguments))
					 (cnf=negate (second arguments)))))
	      ((termc~equivalence-p next-formula)
	       (appl~create (cnf=environment-get 'equiv)
					(list (cnf=negate (first arguments))
					      (second arguments))))
	      ((termc~implication-p next-formula)
	       (appl~create (cnf=environment-get 'and)
					(list (first arguments)
					      (cnf=negate (second arguments)))))
	      ((termc~existential-quantification-p next-formula)
	       (cnf=universal-quantification-create (termc~quantification-bound-variable next-formula)
						    (cnf=negate (termc~quantification-scope next-formula))))
	      ((termc~universal-quantification-p next-formula)
	       (cnf=existential-quantification-create (termc~quantification-bound-variable next-formula)
						      (cnf=negate (termc~quantification-scope next-formula))))
	      (t (error "The formula ~a doesn't match the input of this routine." formula ))))))
 


;; ---------------------------------------------------------------------------
;;;begin{latex}
;;;\subsection{Normalization}
;;;end{latex}
;; ---------------------------------------------------------------------------
                                                                                        


(defun cnf~normalize (formula cnf*env &key (delta cnf*delta-relation) (negation nil))
  (declare (edited  "14-SEP-1992 12:21")
	   (authors SCHEJA)
	   (input   "A formula of type o, that is in long beta-eta-normalform,
an environment and optionally a delta-relation, a flag for negation, and a list.
The formula is assumed to be closed. ")
	   (effect  "cnf*delta-relation is completed destructively with delta-relations,
if cnf*delta-relation is not nil.
The introduced skolem constants are added to the environment.")
	   (value   "A list of clauses that is equivalent to formula. The clauses
are variable disjunct, all variables used in clauses are new variables.
As a second value, you get a list of all introduced skolem-constants."))
  (unless (type~o-p (term~type formula))
    (error "~a should be of type o." formula))
  (setq cnf*delta-relation delta)
  (setq cnf*skolem-constants nil)
  (let* ((new-formula (term~copy (cond ((conc~p formula)
					(conc~formula formula))
				       ((assum~p formula)
					(assum~formula formula))
				       (t formula)))))
    (unless (null (top~free-variables new-formula))
      (error "The formula ~a should be a closed wff_0." new-formula))
    (if (null cnf*delta-relation)
	(values
	 (cnf=rename-clausel-list (cnf=normalize new-formula negation nil))
	 cnf*skolem-constants)
	(progn
	  (cnf=set-atomar-positions! new-formula)
	  (let ((result (cnf=rename-clausel-list (cnf=normalize new-formula negation nil))))
	    (cnf=fill-delta-relation formula result)
	    ;; delta-relation refers to the input-formula,
	    ;; new-formula is changed destructively.
	    (values result
		    cnf*skolem-constants))))))
      


(defun cnf=normalize (formula negation varlist)
  (declare (edited  "14-SEP-1992 12:23")
	   (authors SCHEJA)
	   (input   "A formula,a flag for negation and a list of variables.")
	   (effect  "None.")
	   (value   "A list of clauses that is equivalent to formula if flag is nil,
and equivalent to the negated formula, if flag is t. The list of variables
is needed for skolemization."))
  (cond ((termc~atom-p formula) (cnf=atom-treatment formula negation))	
	((cnf=quantification-p formula) (cnf=quantor-treatment formula negation varlist))
	#+doof((cnf=term-pi-term formula)
	       (cnf=normalize (appl~create (first (appl~arguments formula))
						       (list (sym~variable-create :type (type~c-range (term~type (first (appl~arguments formula)))))))
			      negation varlist))
	(t (let ((arguments (appl~arguments formula)))
	     (cond ((termc~negation-p formula)
		    (cnf=normalize (first arguments) (not negation) varlist))
		   ((termc~conjunction-p formula)
		    (cnf=and-treatment arguments negation varlist))
		   ((termc~disjunction-p formula)
		    (cnf=or-treatment arguments negation varlist ))
		   ((termc~implication-p formula)
		    (cnf=implication-treatment arguments negation varlist))
		   ((termc~equivalence-p formula )
		    (cnf=equivalence-treatment (cnf=operator-parts arguments 'termc~equivalence-p)  negation varlist ))
		   (t (error "~a is not in the supposed normal form." formula)))))))

;; -------------------- Treatment of Atoms -------------------------------------------

(defun cnf=atom-treatment (formula negation)
  (declare (edited  "21-JAN-1993 16:50")
	   (authors SCHEJA)
	   (input   "An atom and a flag for negation.")
	   (effect  "None.")
	   (value   "A unary clause, it's literal is formula, if negation is nil,
resp. (not formula) if negation is t."))

  (let ((literal (lit~literal-create formula (not negation))))
    (keim~put literal :position (keim~get formula :position))
    	(list (cl~create (list literal)))))


;; -------------------- Treatment of Disjunctions ------------------------------------
						 
(defun cnf=or-treatment (arglist  negation varlist)
  (declare (edited  "14-SEP-1992 12:25")
	   (authors SCHEJA)
	   (input   "A list of length 2 of formulas, a flag and a list of variables.")
	   (effect  "None.")
	   (value   "A list of clauses, that is the cnf-form of the disjunction elements
in the arglist if negation is nil, else the cnf-form of the
disjunction negated elements. The list of variables is needed for
skolemization."))
  (let ((list1 (cnf=normalize (first arglist) negation varlist))
	(list2 (cnf=normalize (second arglist) negation varlist)))
    (if negation
	(append list1 list2)
	(cnf=or-product list1 list2))))



(defun cnf=or-product (list1 list2)
  (declare (edited  "14-SEP-1992 12:32")
	   (authors SCHEJA)
	   (input   "Two lists of clauses.")
	   (effect  "None.")
	   (value   "As a list the cartesian product of the lists list1 and list2." ))
  (if (null list1) ()
      (append (cnf=pairs (first list1) list2) (cnf=or-product (rest list1) list2))))

(defun cnf=pairs (element list2)
  (declare (edited  "14-SEP-1992 12:57")
	   (authors SCHEJA)
	   (input   "A clause and a list of clauses.")
	   (effect  "None.")
	   (value   "A list of the union of element with each of the clauses of the list."))
  (if (null list2) ()
      (cons (cnf=melt-clauses element (first list2))
	    (cnf=pairs element (rest list2)))))

(defun cnf=melt-clauses (clause1 clause2)
  (declare (edited  "15-SEP-1992 14:20")
	   (authors SCHEJA)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "A clause that consists of all literals of clause 1 and 2,
duplicate literals are remmoved."))
  (cl~create (remove-duplicates (append(cl~literals clause1)
				       (cl~literals clause2))
				:test #'keim~equal)))
   

;; ------------------------- Treatment of Conjunctions ------------------------------


(defun cnf=and-treatment (arglist negation varlist)
  (declare (edited  "14-SEP-1992 12:59")
	   (authors SCHEJA)
	   (input   "A list of length 2 of formulas, a flag and a list of variables.")
	   (effect  "None.")
	   (value   "A list of clauses, that is the cnf-form of the conjunction
elements in the arglist if negation is nil, else the cnf-form of the
conjunction negated elements.The list of variables
is needed for skolemization."))
  (let ((list1 (cnf=normalize (first arglist) negation varlist))
	(list2 (cnf=normalize (second arglist) negation varlist)))
    (if negation
	(cnf=or-product list1 list2)
	(append list1 list2))))

;; --------------------------- Treatment of Implications ---------------------------


(defun cnf=implication-treatment (arglist negation varlist)
  (declare (edited  "17-DEC-1992 15:49")
	   (authors SCHEJA)
	   (input   "Two arguments, a flag for negation and a varlist.")
	   (effect  "None.")
	   (value   "A list of clauses, that is the cnf-form
 of (element1 implies element2), resp. (not (element1 implies element2))
if negation is t, where arglist=(element1 element2). The list of variables
is needed for skolemization."))
  (let ((result1 (cnf=normalize (first arglist) (not negation) varlist))
	(result2 (cnf=normalize (second arglist) negation varlist)))
    (if negation
	(append result1 result2)
      (cnf=or-product result1 result2))))

;; ------------------------- Treatment of Equivalences ------------------------------


(defun cnf=equivalence-treatment (arglist negation varlist)
  (declare (edited  "07-OCT-1992 14:46")
	   (authors SCHEJA)
	   (input   "A list of formulas,a flag for negation and a list of variables.")
           (effect "None.")
	   (value  "A list of clauses, that is the cnf-form of the equivalence of the elements
 in the arglist if negation is nil, else the negated form.The list of variables
is needed for skolemization."))
  
  (when cnf*delta-relation 
    (error "~a are arguments of an equivalence, it can't be recorded.
Normalization is broken. If you want to normalize this formula, either give no delta-relation as argument
or simplify this formula with simpl~~simplify first (included simpl*equiv-elimination-on t)." arglist))
  
  (when negation (setq arglist (cons (cnf=negate (first arglist))
				     (rest arglist))))
  ;; the first element of arglist is negated if the negation-flag is
  ;; set
  
  (let ((list (cnf=odd-and-even-sublists arglist)))
    (do* ((odd-list (first list) (rest odd-list))
	  (complement-list (cnf=complements-of-sublists odd-list arglist) (rest complement-list))
	  (result (cnf=n-ary-or-treatment
		   (append (first odd-list)
			   (mapcar #'cnf=negate	  (first complement-list)))
		   nil varlist)
		  (append result (cnf=n-ary-or-treatment
				  (append (first odd-list)
					  (mapcar #'cnf=negate (first complement-list)))
				  nil varlist))))
	 ((= 1 (length  odd-list)) result))))


(defun cnf=n-ary-or-treatment (arglist negation varlist)
  (declare (edited  "08-OCT-1992 17:31")
	   (authors SCHEJA)
	   (input   "A list of formulas, a flag for negation and a list of variables.")
	   (effect  "None.")
	   (value   "A list of clauses equivalent to the disjunction of the elements
of arglist if negation is nil, else of the disjuncion of the negated elements.
The list of variables is needed for skolemization."))
  (do* ((list arglist (rest list))
	(element (cnf=normalize (term~copy (first list)) negation varlist)
		 (cnf=normalize (term~copy (first list)) negation varlist))
	;; term~copy is necessary, because skolem-constants are intoduced
	;; destructively.
	(result element (cnf=or-product result element)))
       ((= (length list) 1) result)))
	
;; -------------------------------- Treatment of Quantifications ---------------------


(defun cnf=quantor-treatment (formula negation varlist)
  (declare (edited  "11-SEP-1992 12:30")
	   (authors SCHEJA)
	   (input   "A formula that is a quantification, a flag if the formula is
negated and a list of variables.")
	   (effect  "Error, if formula is no quantification.")
	   (value   "Formula is suggested to be in the scope of the variables in the list (universal
quantified), the value is the normalized formula, all skolem constants additional dependent
of the variables in the varlist, whether existential or universal bounded variables are instantiated
is dependent of negation:
negation = nil  : existential bound variables
negation = true : universal bound variables." ))

  (if (cnf=quantification-p formula)
      (let* ((variable (termc~quantification-bound-variable formula))
	     (scope (termc~quantification-scope formula)))
	
	(if (or (and negation
		     (termc~universal-quantification-p formula))
		(and (not negation)
		     (termc~existential-quantification-p formula)))
	    (cnf=normalize (top~replace-free-variables!! scope (list variable) (list (cnf=get-skolem-term-and-extend-env variable varlist)))
			   negation varlist)
	  ;; the reverse below replaces an nreverse which was destructively altering varlist and causing the
	  ;; wrong number of variables to be schlepped around DAN
	  (cnf=normalize scope negation (reverse (cons variable varlist)))))	
	
      (error "~a doesn't match the input of this routine, it must be a quantification." formula)))    


;; ------------------------------------------------------------------------------------
;;                                      Service Routines
;; ------------------------------------------------------------------------------------
(defun cnf=add-skolem-constant (constant)
  (declare (edited  "30-MAR-1993 16:45")
	   (authors SCHEJA)
	   (input   "A constant.")
	   (effect  "The constant is added to the set of skolem constants.")
	   (value   "Undefined."))
  (setq cnf*skolem-constants (cons constant cnf*skolem-constants)))




(defun cnf=fill-delta-relation (formula clause-list)
  (declare (edited  "22-OCT-1992 22:00")
	   (authors SCHEJA)
	   (input   "A formula and a list of clauses.")
	   (effect  "All literals in the list of clauses are added to
the delta-relation referring to formula." )
	   (value   "undefined."))
  (do* ((clauses clause-list (rest clauses))
	(element-as-clause (first clauses) (first clauses)))
       ((null clauses) ())
    (let ((element-as-list (cl~literals element-as-clause)))
      (mapcar #'(lambda (x) (delta~add-pair! cnf*delta-relation formula (keim~get x :position)
					     element-as-clause (pos~list-position (list (position x element-as-list)))))
	      element-as-list))))

(defun cnf=set-atomar-positions! (formula)
  (declare (edited  "14-SEP-1992 16:15")
	   (authors SCHEJA)
	   (input   "A formula and a position.")
	   (effect  "The atoms of formula are marked with position .")
	   (value   "The marked formula."))
  (do ((atom-list (term~positions formula #'termc~atom-p) (rest atom-list)))
      ((null atom-list) formula)
    (keim~put (term~at-position formula (first atom-list)) :position (first atom-list))))




(defun cnf=term-pi-term (formula)
  (and (termc~compound-term-p formula)
       (term~equal-p (term~top formula) (cnf=environment-get 'forall))
       (not (abstr~p (first (appl~arguments formula))))))


    

(defun cnf=environment-get (symbol)
  (declare (edited  "24-SEP-1992 14:48")
	   (authors AAYARI)
	   (input   "A symbol.")
	   (effect  "None.")
	   (value   "Gives the keim~object named symbol out of the actual environment.")
	   (special cnf*env))
  (env~lookup-object symbol cnf*env))



#+comment(defun cnf=poly-environment-get (symbol type)
  (declare (edited  "24-SEP-1992 14:48")
	   (authors AAYARI)
	   (input   "A symbol and optionally a type.")
	   (effect  "None.")
	   (value   "Gives the keim~object named symbol out of the environment.")
	   (special cnf*env))
  (poly~env-lookup symbol cnf*env type)) 


(defun cnf=literal-p (formula)
  (declare (edited  "14-SEP-1992 15:16")
	   (authors SCHEJA)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "True, if formula is an atom or a negated atom."))
  (or (termc~atom-p formula)
      (and (termc~negation-p formula)
	   (termc~atom-p (first (appl~arguments formula))))))



(defun cnf=universal-quantification-create (variable scope)
  (declare (edited  "28-SEP-1992 16:04")
	   (authors SCHEJA)
	   (input   "A variable and a term of type o.")
	   (effect  "None.")
	   (value   "The scope, universally quantified over variable."))
  (termc~quantification-create (env~lookup-object  'forall cnf*env :type (type~abstract (type~o) (type~abstract (type~o) (term~type variable))))
			      variable scope))


(defun cnf=existential-quantification-create (variable scope)
  (declare (edited  "28-SEP-1992 16:04")
	   (authors SCHEJA)
	   (input   "A variable and a term of type o.")
	   (effect  "None.")
	   (value   "The scope, existentially quantified over variable."))
  (termc~quantification-create (env~lookup-object  'exists cnf*env :type (type~abstract (type~o) (type~abstract (type~o) (term~type variable))))
			      variable scope))


(defun cnf=quantification-p (term)
  (declare (edited  "29-MAR-1993 16:32")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  )
	   (value   "True, iff term is an existential or universal quantification."))
  (or (termc~existential-quantification-p term)
      (termc~universal-quantification-p term)))




(defun cnf=odd-and-even-sublists (list)
  (declare (edited  "07-OCT-1992 13:35")
	   (authors SCHEJA)
	   (input   "A list of objects, that are not eq.")
	   (effect  "None.")
	   (value   "A List, consisting of one list of all sublists  with odd length and one of all aublists with even length."))
  (let ((odd-list ())
        (even-list ()))
    (do* ((step list (rest step))
          (help-list (mapcar #'list list) (cnf=cons-new-elements step help-list)))
         ((null step) (list odd-list (cons nil even-list)))
      (if (oddp (length (first help-list)))
          (setq odd-list (append odd-list help-list))
          (setq even-list (append even-list help-list))))))
          


(defun cnf=cons-new-elements (elements target-list)
  (declare (edited  "07-OCT-1992 13:40")
	   (authors SCHEJA)
	   (input   "A list of elements, and a list of lists.")
	   (effect  "None.")
	   (value   "Each element of elements is consed at the elements of target-list,until
the first element of an element of target-list is eq to element."))
  (do* ((result ())
	(help-list elements (rest help-list))
	(element (first help-list) (first help-list)))
       ((null help-list) result)
    (do ((run-list target-list (rest run-list)))
	((eq (first (first run-list)) element) ())
      (setq result (append result (list (cons element (first run-list))))))))



(defun cnf=complements-of-sublists (sublists list)
  (declare (edited  "07-OCT-1992 17:58")
	   (authors SCHEJA)
	   (input   "A list of sublists of list and a list.")
	   (effect  "None.")
	   (value   "A list of the complements of sublists in list." ))
  (mapcar #'(lambda (x)
	      (block nil
	      (do* ((help-list x (rest help-list))
		    (element (first help-list) (first help-list))
		    (result (if (null help-list)
				(return list)
				(remove-if #'(lambda (y) (keim~equal element y)) list :count 1))
			    (remove-if #'(lambda (y) (keim~equal element y)) result :count 1)))
		   ((= 1 (length help-list)) result))))
	  sublists))

(defun cnf=multi-application-create (operator list)
  (declare (edited  "12-JAN-1993 11:50")
	   (authors SCHEJA)
	   (input   "A binary logical operator and a non-empty list of formulas (x1 .. xn).")
	   (effect  "None.")
	   (value   "The keim-form of (operator x1 .. xn)."))
  (if (= 1 (length list))
      (first list)
      (appl~create operator (list (first list) (cnf=multi-application-create operator (rest list))))))


(defun cnf=no-free-var-in (term)
  (declare (edited  "22-DEC-1992 18:55")
	   (authors SCHEJA)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True, iff in term are no free variables."))
  (null (top~free-variables term)))



(defun cnf=true-p (arg)
  (declare (edited  "30-JUN-1992 13:31" )
	   (authors SCHEJA )
	   (input  "A formula."  )
	   (effect "None." )
	   (value  "True, iff arg is the constant true." ))
  (keim~equal (cnf=environment-get 'true) arg))

(defun cnf=false-p (arg)
  (declare (edited  "30-JUN-1992 13:31" )
	   (authors SCHEJA )
	   (input  "A formula."  )
	   (effect "None." )
	   (value  "True, iff arg is the constant false." ))
  (keim~equal (cnf=environment-get 'false) arg))

(defun cnf=truthvalue-p (formula)
  (declare (edited  "27-JUN-1992 11:08" )
	   (authors SCHEJA )
	   (input   "An object." )
	   (effect  "None." )
	   (value   "True, iff formula is a truthvalue." ))
  (or (keim~equal formula (cnf=environment-get 'true)) (keim~equal formula (cnf=environment-get `false))))


(defun cnf=dual-truthvalue (formula)
  (declare (edited  "27-JUN-1992 11:12" )
	   (authors SCHEJA )
	   (input   "A truthvalue." )
	   (effect  "Error, if formula is no truthvalue." )
	   (value   "The negated truthvalue." ))
  (cond ((keim~equal formula (cnf=environment-get 'true)) (cnf=environment-get `false))
	((keim~equal formula (cnf=environment-get 'false)) (cnf=environment-get `true))
	(t (error "~a is no truthvalue."))))

(defun cnf=negate (formula)
  (declare (edited  "22-DEC-1992 20:03")
	   (authors SCHEJA)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "The negated formula."))
  (appl~create (cnf=environment-get 'not) (list formula)))


(defun cnf=dual-operator (operator)
  (declare (edited  " 2-JUL-1992 12:43" )
	   (authors SCHEJA )
	   (input   "An operator." )
	   (effect  "None." )
	   (value   "Iff operator is 'and then 'or, and viceversa." ))
  (if (keim~equal operator (cnf=environment-get 'and))
      (cnf=environment-get 'or)
      (cnf=environment-get 'and)))



(defun cnf=rename-clausel-list (clausel-list)
  (declare (edited  "09-FEB-1993 14:01")
	   (authors SCHEJA)
	   (input   "A list of clauses.")
	   (effect  "None.")
	   (value   "The clausel-list, where clauses are copied that have variables in
common with other clauses. These variables are renamed."))
  (mapcar #'top~rename!! (mapcar #'term~copy-with-properties  clausel-list)))



(eval-when (load compile eval)
(defclass cnf+conclusion-clause (cl+clause)
  ()
  (:documentation "Clauses that come from a conclusion."))
(defclass cnf+axiom-clause (cl+clause)
  ()
  (:documentation "Clauses that come from an axiom."))
)

(defun cnf~conclusion-clause-p (x)
  (declare
   (input "a lisp object")
   (value "t if object is a conclusion clause, otherwise nil"))
  (typep x 'cnf+conclusion-clause))

(defun cnf~axiom-clause-p (x)
  (declare
   (input "a lisp object")
   (value "t if object is an axiom clause, otherwise nil"))
  (typep x 'cnf+axiom-clause))

(defmethod post~print ((clause cnf+conclusion-clause) stream)
  (format stream "(conclusion-clause ~A (" (keim~name clause))
  (post~print-declaration (termc~all-bound-variables clause) stream)
  (format stream ") ")
  (post~print (cl~literals clause) stream)
  (format stream ")"))

(defmethod post~print ((clause cnf+axiom-clause) stream)
  (format stream "(axiom-clause ~A (" (keim~name clause))
  (post~print-declaration (termc~all-bound-variables clause) stream)
  (format stream ") ")
  (post~print (cl~literals clause) stream)
  (format stream ")"))



(defmethod post~read-object ((clause list) (env env+environment) 
			    (indicator (eql :conclusion-clause)))
  (let ((new-clause (post~read-object clause env :clause)))
    (change-class new-clause (find-class 'cnf+conclusion-clause))))

(defmethod post~read-object ((clause list) (env env+environment) 
			    (indicator (eql :axiom-clause)))
  (let ((new-clause (post~read-object clause env :clause)))
    (change-class new-clause (find-class 'cnf+axiom-clause))))

(defmethod post~read-object ((clause symbol) (env env+environment) 
			     (indicator (eql :conclusion-clause)))
  (cl~env-lookup clause env))

(defmethod post~read-object ((clause string) (env env+environment) 
			     (indicator (eql :conclusion-clause)))
  (cl~env-lookup clause env))

(defmethod post~read-object ((clause symbol) (env env+environment) 
			     (indicator (eql :axiom-clause)))
  (cl~env-lookup clause env))

(defmethod post~read-object ((clause string) (env env+environment) 
			     (indicator (eql :axiom-clause)))
  (cl~env-lookup clause env))


