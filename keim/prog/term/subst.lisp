;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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

(in-package "KEIM")

(mod~defmod subst :uses ( env keim mod post sym term top )
	    :documentation "Datastructures and basic functionality of substitutions."
	    :exports (
		      subst+substitution
		      subst~create
		      subst~domain
		      subst~codomain
		      subst~p
		      subst~empty-p
		      subst~list-p
		      subst~insert-component
		      subst~insert-component!
		      subst~add-component
		      subst~add-component!
		      subst~add-component!!
		      subst~get-component
		      subst~remove-component
		      subst~remove-component!
		      subst~remove-components
		      subst~remove-components!
		      subst~apply
		      subst~apply!
		      subst~apply!!
		      subst~apply-and-rename
		      subst~apply-and-rename!
		      subst~apply-and-rename!!
		      subst~compose-substitution
		      subst~compose-substitution!
		      subst~compose-substitution!!
		      subst~disjoint-compose-substitution
		      subst~disjoint-compose-substitution!
		      subst~disjoint-compose-substitution!!
		      subst~restrict-substitution
		      subst~restrict-substitution!
		      subst~delete-duplicates
		      subst~delete-duplicates!
		      subst~create-binding-substitution
		      subst~read
		      )

	    )

#{
\section{Substitutions}\label{mod:subst}
This module provides elementary functions for substitutions.
Functions without a `!' have no side-effects; functions with 
one `!' only have side-effects on substitutions,
and functions with two `!' have side-effects on substitutions and terms.
#}


(eval-when (load compile eval)
(defclass subst+substitution (keim+object)
    ((domain :initarg :domain :reader subst=domain :writer subst=write-domain!)
     (codomain :initarg :codomain :reader subst=codomain :writer subst=write-codomain!))
    (:documentation "The class of substitutions.")))

(defun subst~create (domain codomain &optional copy)
  (declare (edited  "04-NOV-1991 11:00")
	   (authors RICHTS)
	   (input   "A list of variables and a list of terms. Optional a flag.")
	   (effect  "None.")
       	   (value   "The new substitution with the domain DOMAIN and the codomain CODOMAIN. If COPY = T the input-lists are copied.")
	   (example "(x y) (a (f a)) --> {(X --> A) (Y --> (F A))}"))
  (if copy
      (make-instance 'subst+substitution :domain (copy-list domain) :codomain (copy-list codomain))
    (make-instance 'subst+substitution :domain domain :codomain codomain)))

(defgeneric subst~domain (substitution)
  (declare (edited  "24-JUN-1992 13:00" )
	   (authors KOHLHASE )
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "The domain of the substitution, i.e. the list of variables that will be subtituted.")
	   (example "{(X --> A) (Y --> (F A))}-->(X Y)"))
  (:method ((substitution subst+substitution))
	   (subst=domain substitution)))

(defgeneric subst~codomain (substitution)
  (declare (edited  "24-JUN-1992 13:00" )
	   (authors KOHLHASE )
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "The codomain of the substitution, i.e. the list of terms that will be substituted for variables.")
	   (example "{(X --> A) (Y --> (F A))}-->(A (F A))"))
  (:method ((substitution subst+substitution))
	   (subst=codomain substitution)))

(defmethod print-object ((substitution subst+substitution) stream)
  (declare (edited  "05-NOV-1991 12:22")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   )
	   (special *print-length*))
  (cond ((subst~empty-p substitution)
	 (format stream "{}"))
	(t
	 (format stream "{(~A --> ~A)" (car (subst~domain substitution)) (car (subst~codomain substitution)))
	 (do ((i (if  *print-length* (1- *print-length*) -1) (1- i))
	      (domain-tail (cdr (subst~domain substitution)) (cdr domain-tail))
	      (codomain-tail (cdr (subst~codomain substitution)) (cdr codomain-tail)))
	     ((or (null domain-tail) (zerop i))
	      (if domain-tail
		  (format stream " ...}")
		  (format stream "}")))
	   (format stream " (~A --> ~A)" (car domain-tail) (car codomain-tail))))))


(defun subst~p (object)
  (declare (edited  "04-NOV-1991 11:01")
	   (authors RICHTS)
	   (input   "A Lisp object.")
	   (effect  "None.")
	   (value   "True, iff OBJECT is a substitution.")
	   (example "{(X --> A) (Y --> (F A))}-->T"))
  (typep object 'subst+substitution))

(defmethod keim~copy ((substitution subst+substitution))
  (declare (edited  "04-NOV-1991 11:02")
      	   (authors RICHTS)
	   (input   "A substitution.")
 	   (effect  "None.")
	   (value   "The copied substitution. Only the substitution structure is copied; not the terms."))  
  (subst~create (copy-list (subst~domain substitution)) (copy-list (subst~codomain substitution))))

(defun subst~empty-p (substitution)
  (declare (edited  "04-NOV-1991 11:02")
	   (authors RICHTS)
	   (input   "A substitution.")
	   (effect  "None.")
       	   (value   "True iff SUBSTITUTION is empty.")
	   (example "{(X --> A) (Y --> (F A))}-->NIL"
		     "{}--> NIL"))
  (null (subst~domain substitution)))

(defmethod keim~equal ((substitution1 subst+substitution) (substitution2 subst+substitution))
  (declare (edited  "04-NOV-1991 11:02")
	   (authors RICHTS)
	   (input   "Two substitutions.")
	   (effect  "None.")
	   (value   "True iff the two substitutions are equal."))
  (let ((domain1 (subst~domain substitution1))
	(domain2 (subst~domain substitution2)))
    (and (every #'(lambda (variable)
		    (member variable domain2 :test #'keim~equal))
		domain1)
	 (every #'(lambda (variable)
		    (member variable domain1 :test #'keim~equal))
		domain2)
	 (every #'(lambda (variable)
		    (keim~equal (subst~get-component variable substitution1) (subst~get-component variable substitution2)))
		domain1))))

(defun subst~list-p (object)
  (declare (edited  "04-NOV-1991 11:08")
	   (authors RICHTS)
	   (input   "OBJECT could be any lisp object.")
	   (value   "True iff OBJECT is a (possible empty) list of substitutions.")
	   (example "({(X --> F) (Y --> (F A))} {}-->T"
		     "(a b)"))
  (and (listp object)
       (or (null object)
	   (and (subst~p (car object)) (subst~list-p (cdr object))))))
		      
(defun subst~insert-component (variable term &optional substitution)
  (declare (edited  "04-NOV-1991 11:08")
	   (authors RICHTS)
	   (input   "A variable, a term and a substitution (which may be nil).")
	   (effect  "None.")
       	   (value   "A new substitution where [VARIABLE --> TERM] is added at the front of SUBSTITUTION.")
	   (remark  "There is no check if the added component [VARIABLE --> TERM] is in the domain"
		    "and codomain. Also occurrences of VARIABLE in the codomain terms are not substituted.")
	   (example "Z B {(X --> F) (Y --> (F A))} -->"
		     "{(Z --> B) (X --> F) (Y --> (F A))}"))
  (if substitution
      (subst~create (cons variable (copy-list (subst~domain substitution)))
		    (cons term (copy-list (subst~codomain substitution))))
    (subst~create (list variable) (list term))))

(defun subst~insert-component! (variable term substitution)
  (declare (edited  "04-NOV-1991 11:09")
	   (authors RICHTS)
	   (input   "A variable, a term and a substitution.")
	   (effect  "The pair [VARIABLE --> TERM] is added at the front of the domain and codomain lists"
		    "of SUBSTITUTION.")
       	   (value   "The changed SUBSTITUTION.")
	   (remark  "There is no check if the added component [VARIABLE --> TERM] is in the domain and codomain."
		    "Also occurences of VARIABLE in the codomain terms are not substituted.")
	   (example "X B {(X --> F) (Y --> (F A))} -->"
		     "{(X --> B) (X --> F) (Y --> (F A))}"))
  (let ((new-domain (cons variable (subst~domain substitution)))
	(new-codomain (cons term (subst~codomain substitution))))
    (subst=write-domain! new-domain substitution)
    (subst=write-codomain! new-codomain substitution)
    substitution))


(defun subst~add-component (variable term &optional substitution)
  (declare (edited  "04-NOV-1991 11:10")
	   (authors RICHTS)
	   (input   "A variable, a term and a substitution (which may be nil).")
	   (effect  "None.")
       	   (value   "A new substitution where the pair [VARIABLE --> TERM] is added at the front of the domain and codomain lists"
		    "of SUBSTITUTION and is applied as a substitution to the codomain list.")
	   (example "X B {(X --> F) (Y --> (F X))}"
		     "{(X --> B) (Y --> (F B)) (X --> F)}"))
  (if substitution
      (subst~insert-component! variable term
			       (top~replace-free-variables substitution (list variable) (list term)))
    (subst~create (list variable) (list term))))

(defun subst~add-component! (variable term substitution)
  (declare (edited  "04-NOV-1991 11:11")
	   (authors RICHTS)
	   (input   "A variable, a term and a substitution.")
	   (effect  "The pair [variable --> term] is added at the front of the domain and codomain lists"
		    "of SUBSTITUTION and is applied as a substitution to the codomain list."
		    "(The codomain terms are not destroyed-)")
       	   (value   "The changed SUBSTITUTION.")
	   (example "X B {(X --> F) (Y --> (F X))}"
		     "{(X --> B) (Y --> (F B)) (X --> F)}"))  
  (subst~insert-component! variable term
			   (top~replace-free-variables! substitution (list variable) (list term))))

(defun subst~add-component!! (variable term substitution)
  (declare (edited  "04-NOV-1991 11:11")
	   (authors RICHTS)
	   (input   "A variable, a term and a substitution (which may be nil).")
	   (effect  "The pair [variable --> term] is added at the front of the domain and codomain lists"
		    "of SUBSTITUTION and is applied as a substitution to the codomain terms destructively.")
       	   (value   "The changed SUBSTITUTION, with all terms, VARIABLE and TERM unshared.")
	   (example "X B {}-->{(X --> B)}"))
  (subst~insert-component! (term~copy variable) (term~copy term)
			   (top~replace-free-variables!! substitution (list variable) (list term))))

(defun subst~get-component (variable substitution)
  (declare (edited  "04-NOV-1991 11:13")
	   (authors RICHTS prckln)
	   (input   "A variable and a substitution.")
	   (effect  "None.")
	   (value   "The codomain term corresponding to VARIABLE or NIL if there is none.")
	   (example "X {(X --> F) (Y --> (F A))} --> F"))
  (some #'(lambda (var term)
	    (if (keim~equal var variable) term nil))
	(subst~domain substitution)
	(subst~codomain substitution)))

(defun subst~remove-component (variable substitution)
  (declare (edited  "04-NOV-1991 11:13")
	   (authors RICHTS gk)
           (input   "A variable and a substitution.")
       	   (effect  "None.")
	   (value  "A new substitution without VARIABLE and its codomain term.")
	   (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (subst~remove-component! variable (keim~copy substitution)))

(defun subst~remove-component! (variable substitution)
  (declare (edited  "04-NOV-1991 11:14")
	   (authors RICHTS gk)
           (input   "A variable and a substitution.")
       	   (effect  "If VARIABLE is in the domain of the substitution, the component of the substitution"
		    "is removed destructively from the domain- and codomain-list.")
	   (value  "SUBSTITUTION without VARIABLE and its codomain term.")
	   (example "X {(X --> F) (Y --> (F A))} --> {(Y --> (F A))}"))
  (labels ((subst=delete (domain codomain)
	     (cond ((null domain) (values nil nil))
		   ((keim~equal variable (car domain))
		    (values (cdr domain) (cdr codomain)))
		   (t (multiple-value-bind (domain-tail codomain-tail)
			  (subst=delete (cdr domain) (cdr codomain))
			(setf (cdr domain) domain-tail
			      (cdr codomain) codomain-tail)
			(values domain codomain))))))
    (multiple-value-bind (domain-tail codomain-tail)
	(subst=delete (subst~domain substitution) (subst~codomain substitution))
      (subst=write-domain! domain-tail substitution)
      (subst=write-codomain! codomain-tail substitution)))
  substitution)

(defun subst~remove-components (variables substitution)
  (declare (edited  "04-NOV-1991 11:13")
	   (authors RICHTS gk)
           (input   "A list of variables and a substitution.")
       	   (effect  "None.")
	   (value   "A new substitution without VARIABLES and its codomain terms.")
	   (example "X Y {(X --> F) (Y --> (F A))} --> {}"
		     "NIL {(X --> F) (Y --> (F A))} --> {(X --> F) (Y --> (F A))}"))
  (subst~remove-components! variables (keim~copy substitution)))

(defun subst~remove-components! (variables substitution)
  (declare (edited  "04-NOV-1991 11:14")
	   (authors RICHTS gk)
           (input   "A list of variables and a substitution.")
       	   (effect  "if a variable in the domain of SUBSTITUTION is in VARIABLES,"
		    "it is removed destructively from the domain and also its codomain term.")
	   (value  "SUBSTITUTION without VARIABLES and its codomain terms.")
	   (example "X Y {(X --> F) (Y --> (F A))} --> {}"))
  (labels ((subst=delete (domain codomain)
	     (if (null domain)
		 (values nil nil)
	       (multiple-value-bind (domain-tail codomain-tail)
		   (subst=delete (cdr domain) (cdr codomain))
		 (cond ((member (car domain) variables :test #'keim~equal)
			(values domain-tail codomain-tail))
		       (t (setf (cdr domain) domain-tail
				(cdr codomain) codomain-tail)
			  (values domain codomain)))))))
    (multiple-value-bind (domain-tail codomain-tail)
	(subst=delete (subst~domain substitution) (subst~codomain substitution))
      (subst=write-domain! domain-tail substitution)
      (subst=write-codomain! codomain-tail substitution)))
  substitution)


#{
\subsection{Operations with substitutions}
#}

(defun subst~apply (substitution object)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS)
	   (input   "A substitution and an object.")
	   (effect  "None.")
       	   (value   "The new object with its terms instantiated by SUBSTITUTION.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (top~replace-free-variables object (subst~domain substitution) (subst~codomain substitution)))

(defun subst~apply! (substitution object)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS)
	   (input   "A substitution and an object where each non-term structure appears only once in OBJECT.")
	   (effect  "All objects but terms are changed destructively.")
       	   (value   "OBJECT with its terms instantiated by SUBSTITUTION if OBJECT is not a term,"
		    "the new, instantiated term else.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (top~replace-free-variables! object (subst~domain substitution) (subst~codomain substitution)))

(defun subst~apply!! (substitution object)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS gk)
	   (input   "A substitution and an object with unshared terms, i. e. each term~box appears only once in OBJECT.")
	   (effect  "All objects and terms are changed destructively.")
       	   (value   "OBJECT with its terms instantiated by SUBSTITUTION.")
	   (example "{(X --> A) (Y --> B)} (Q X (G X Y)) --> (Q A (G A B))"))
  (top~replace-free-variables!! object (subst~domain substitution) (subst~codomain substitution)))


(defun subst~apply-and-rename (substitution object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS)
	   (input   "A substitution and an object.")
	   (effect  "None.")
       	   (value   "The new object with its terms instantiated by SUBSTITUTION and all free variables renamed.")
	   (example "{(X --> A)} (Q X (G X Y)) --> (Q A (G A x-1)) {(Y --> x-1)}"))
  (multiple-value-bind (new-object domain codomain)
      (top~replace-free-variables-and-rename object (subst~domain substitution) (subst~codomain substitution)
					      :return-renaming return-renaming
					      :variables variables :not-variables not-variables)
    (if return-renaming
	(values new-object (subst~create domain codomain))
	new-object)))

(defun subst~apply-and-rename! (substitution object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS)
	   (input   "A substitution and an object where each non-term structure appears only once in OBJECT.")
	   (effect  "All objects but terms are changed destructively.")
       	   (value   "OBJECT with its terms instantiated by SUBSTITUTION and all free variables renamed if OBJECT is not a term,"
		    "a new, instantiated and renamed term otherwise.")
	   (example "{(X --> A)} (Q X (G X Y)) --> (Q A (G A x-2)) {(Y --> x-2)}"))
  (multiple-value-bind (new-object domain codomain)
      (top~replace-free-variables-and-rename! object (subst~domain substitution) (subst~codomain substitution)
					      :return-renaming return-renaming
					      :variables variables :not-variables not-variables)
    (if return-renaming
	(values new-object (subst~create domain codomain))
      new-object)))

(defun subst~apply-and-rename!! (substitution object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "04-NOV-1991 11:14")
       	   (authors RICHTS)
	   (input   "A substitution and an object with unshared terms, i. e. each term~box appears only once in OBJECT.")
	   (effect  "All objects and terms are changed destructively.")
       	   (value   "OBJECT with its terms instantiated by SUBSTITUTION and all free variables renamed."))
  (multiple-value-bind (new-object domain codomain)
      (top~replace-free-variables-and-rename!! object (subst~domain substitution) (subst~codomain substitution)
						:return-renaming return-renaming
						:variables variables :not-variables not-variables)
    (if return-renaming
	(values new-object (subst~create domain codomain))
	new-object)))


(defun subst~compose-substitution (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:19")
	   (authors RICHTS gk)
	   (input   "Two substitutions.")
	   #+comment
	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "None.")
	   (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and all domain-variables of"
		    "OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION are added with their codomain-terms:"
		    "The new substitution.")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		     "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z
--> A)}"))
  (let ((new-substitution (subst~apply outer-substitution inner-substitution)))
    (mapc #'(lambda (var term)
	      (unless (member var (subst~domain  new-substitution) :test #'term~equal)
		(subst~insert-component! var term new-substitution)))
	  (subst~domain outer-substitution) (subst~codomain outer-substitution))
    new-substitution))

(defun subst~compose-substitution! (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:19")
	   (authors RICHTS gk)
	   (input   "Two substitutions.")
	   #+comment
       	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and all domain-variables of"
		    "OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION are added with their codomain-terms.")
	   (value   "The changed INNER-SUBSTITUTION.")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		     "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z
--> A)}"))
  (subst~apply! outer-substitution inner-substitution)
  (mapc #'(lambda (var term)
	    (unless (member var (subst~domain inner-substitution))
	      (subst~insert-component! var term inner-substitution)))
	(subst~domain outer-substitution) (subst~codomain outer-substitution))
  inner-substitution)

(defun subst~compose-substitution!! (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:21")
	   (authors RICHTS gk)
	   (input   "Two substitutions.")
	   #+comment
       	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "OUTER-SUBSTITUTION is applied destructively to the codomain of INNER-SUBSTITUTION"
		    "and all domain-variables of OUTER-SUBSTITUTION not in the domain of INNER-SUBSTITUTION"
		    "are added with their codomain-terms.")
	   (value   "The changed INNER-SUBSTITUTION (sharing no terms with OUTER-SUBSTITUTION).")
	   (example "{(Z --> A)} {(X --> A) (Y --> (F Z))} --> {(Z --> A) (Y --> (F A)) (X --> A)}"
		     "{(X --> A) (Y --> (F Z))} {(Z --> A)} --> {(Y --> (F Z)) (X --> A) (Z
--> A)}"))
  (subst~apply!! outer-substitution inner-substitution)
  (mapc #'(lambda (var term)
	    (unless (member var (subst~domain inner-substitution))
	      (subst~insert-component! (term~copy var) (term~copy term) inner-substitution)))
	(subst~domain outer-substitution) (subst~codomain outer-substitution))
  inner-substitution)



(defun subst~disjoint-compose-substitution (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:22")
	   (authors RICHTS gk)
	   (input   "Two substitutions the domains of which are disjoint.")
	   #+comment
       	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "None.")
	   (value   "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and the domain and codomain of"
		    "OUTER-SUBSTITUTION is added to the resulting substitution:"
		    "A new substitution.")
	   (example "{(X --> F) (Y --> (F A))} {(Z --> C)} -->{(Z --> C) (X --> F) (Y --> (F A))}"))  
  (let ((new-substitution (subst~apply outer-substitution inner-substitution)))
    (subst=write-domain! (append (subst~domain new-substitution) (subst~domain outer-substitution)) new-substitution)
    (subst=write-codomain! (append (subst~codomain new-substitution) (subst~codomain outer-substitution)) new-substitution)
    new-substitution))

(defun subst~disjoint-compose-substitution! (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:22")
	   (authors RICHTS gk)
	   (input   "Two substitutions the domains of which are disjoint.")
	   #+comment
       	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "OUTER-SUBSTITUTION is applied to the codomain of INNER-SUBSTITUTION and the domain and codomain of"
		    "OUTER-SUBSTITUTION is added to INNER-SUBSTITUTION.")
	   (value   "The changed INNER-SUBSTITUTION.")
	   (example "subst --> {(X --> F) (Y --> (F A))}"
		     "subst1 --> {(Z --> C)}"
		     "(subst~disjoint-compose-substitution! subst1 subst) -->{(X --> F) (Y --> (F A)) (Z --> C)}"
		     "subst1 -->{(Z --> C)}"
		     "subst -->{(X --> F) (Y --> (F A)) (Z --> C)}, please notice the destructive changes made by this function"))
  (subst~apply! outer-substitution inner-substitution)
  (subst=write-domain! (append (subst~domain inner-substitution) (subst~domain outer-substitution)) inner-substitution) 
  (subst=write-codomain! (append (subst~codomain inner-substitution) (subst~codomain outer-substitution)) inner-substitution)
  inner-substitution)

(defun subst~disjoint-compose-substitution!! (outer-substitution inner-substitution)
  (declare (edited  "04-NOV-1991 11:23")
	   (authors RICHTS gk)
	   (input   "Two substitutions the domains of which are disjoint.")
	   #+comment
       	   (warning "In order to obtain an idempotent substitution the codomain of OUTER-SUBSTITUTION"
		    "and the domain of INNER-SUBSTITUTION must be variable disjoint.")
	   (effect  "OUTER-SUBSTITUTION is applied destructively to the codomain of INNER-SUBSTITUTION"
		    "and the domain and codomain of OUTER-SUBSTITUTION is added to INNER-SUBSTITUTION.")
	   (value   "The changed INNER-SUBSTITUTION (sharing no terms with OUTER-SUBSTITUTION).")
	   (example "This function will never return, if you dare to call it"))
  (subst~apply!! outer-substitution inner-substitution)
  (nconc (subst~domain inner-substitution) (term~copy (subst~domain outer-substitution)) )
  (nconc (subst~codomain inner-substitution) (term~copy (subst~codomain outer-substitution)))
  inner-substitution)

(defun subst~restrict-substitution (substitution variables)
  (declare (edited  "05-NOV-1991 10:25")
	   (authors RICHTS gk)
	   (input   "A substitution and a list of variables.")
       	   (effect  "None.")
	   (value   "A new substitution where the domain is the intersection of the domain of SUBSTITUTION and VARIBLES"
		    "and the codomain contains the corresponding terms of SUBSTITUTION.")
	   (example "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"))
  (let ((new-substitution (subst~create nil nil)))
    (mapc #'(lambda (var term)
	      (when (member var variables)
		(subst~insert-component! var term new-substitution)))
	  (subst~domain substitution) (subst~codomain substitution))
    new-substitution))


(defun subst~restrict-substitution! (substitution variables)
  (declare (edited  "05-NOV-1991 10:26")
	   (authors RICHTS gk)
	   (input   "A substitution and a list of variables.")
       	   (effect  "The variables in the domain of SUBSTITUTION which don't occur in VARIABLES and their"
		    "corresponding terms in the codomain are destructively deleted from these lists.")
	   (value   "The changed SUBSTITUTION.")
	   (example "subst --> {(X --> A) (Y --> (F A))}"
	             "{(X --> A) (Y --> (F A))} (X Z) -->{(X --> A)}"
		     "subst --> {(X --> A)}, please notice the destructive changes made to subst"))
  (do ((domain-tail (subst~domain substitution) (cdr domain-tail)))
      ((or (null domain-tail) (member (car domain-tail) variables :test #'keim~equal)))
    (subst=write-domain! (cdr (subst=domain substitution)) substitution)
    (subst=write-codomain! (cdr (subst=codomain substitution)) substitution))
  (do ((domain-tail (subst~domain substitution))
       (codomain-tail (subst~codomain substitution)))
      ((null (cdr domain-tail)))
    (cond ((member (cadr domain-tail) variables :test #'keim~equal)
	   (pop domain-tail)
	   (pop codomain-tail))
	  (t (pop (cdr domain-tail))
	     (pop (cdr codomain-tail)))))
  substitution)


(defun subst~delete-duplicates (substitution)
  (declare (edited  "05-NOV-1991 10:35")
	   (authors RICHTS ohlbach)
	   (input   "A substitution.")
	   (effect  "None.")
	   (value   "A new substitution with all pairs of SUBSTITUTION without that of the form (x --> x).")
	   (example "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"))
  (let ((new-substitution (subst~create nil nil)))
    (do ((domain-tail (subst~domain substitution) (cdr domain-tail))
	 (codomain-tail (subst~codomain substitution) (cdr codomain-tail)))
	((null domain-tail))
      (unless (keim~equal (car domain-tail) (car codomain-tail))
	(subst~insert-component! (car domain-tail) (car codomain-tail) new-substitution)))
    new-substitution))

(defun subst~delete-duplicates! (substitution)
  (declare (edited  "05-NOV-1991 10:37")
	   (authors RICHTS)
	   (input   "A substitution.")
	   (effect  "Identical domain - codomain pairs (x --> x) are deleted in the substitution.")
	   (value   "The changed SUBSTITUTION.")
	   (example "subst --> {(X --> X) (Y --> (F A))}"
		     "{(X --> X) (Y --> (F A))} --> {(Y --> (F A))}"
		     "subst {(Y --> (F A))}, please notice the destructive changes made to subst"))
  (do ((domain-tail (subst~domain substitution) (cdr domain-tail))
       (codomain-tail (subst~codomain substitution) (cdr codomain-tail)))
      ((or (null domain-tail) (not (keim~equal (car domain-tail) (car codomain-tail)))))
    (subst=write-domain! (cdr (subst=domain substitution)) substitution)
    (subst=write-codomain! (cdr (subst=codomain substitution)) substitution))
  (do ((domain-tail (subst~domain substitution))
       (codomain-tail (subst~codomain substitution)))
      ((null (cdr domain-tail)))
    (cond ((keim~equal (cadr domain-tail) (cadr codomain-tail))
	   (pop (cdr domain-tail))
	   (pop (cdr codomain-tail)))
	  (t (pop domain-tail)
	     (pop codomain-tail))))
  substitution)


(defun subst~create-binding-substitution (&optional (recursiv T))    ;RECURCIV hinzugefuegt
  (declare (edited  "07-NOV-1991 12:01")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "None.")
	   (value   "A substitution that contains all variables in the actual binding-list as its domain"
		    "and the terms in the binding slots of these variables as codomain."
		    "If recursiv is T the codomain-terms are built with top~insert-bindings!, i. e. the binding slots are regarded;"
		    "else the binding-terms itself are taken.")
	   (example "(top~binding-list!) -->NIL"
		    "(subst~create-binding-substitution) --> {}"))
  (do* ((new-substitution (subst~create nil nil))
	(binding-list-tail (top~binding-list!) (cdr binding-list-tail))
	(term (car binding-list-tail) (car binding-list-tail)))
      ((null binding-list-tail) new-substitution)
    (if (and (sym~variable-p term) (term~binding term))
	(subst~insert-component! term 
				 (if recursiv 
				     (top~insert-bindings! (term~binding term)) 
				     (term~binding term))
				 new-substitution))))



;; Term-Operations on Substitutions

(defmethod term~copy ((substitution subst+substitution))
  (declare (edited  "04-NOV-1991 11:02")
      	   (authors RICHTS)
	   (input   "A substitution.")
 	   (effect  "None.")
	   (value   "The copied substitution with copied terms."))
  (subst~create (term~copy (subst~domain substitution))
		(term~copy (subst~codomain substitution))))


(defmethod top~replace-terms ((substitution subst+substitution) old-terms new-terms &key (test #'keim~equal))
  (declare (edited  "05-NOV-1991 10:32")
	   (authors RICHTS)
	   (input   "A substitution and two termlists of equal length.")
	   (effect  "None.")
	   (value   "The new substitution where all occurrences of OLD-TERMS in the codomain of SUBSTITUTION"
		    "(tested for equality with TEST) are replaced with the corresponding terms in NEW-TERMS."))
  (let ((new-substitution (subst~create nil nil)))
    (mapc #'(lambda (variable term)
	      (let ((new-term (top~replace-terms term old-terms new-terms :test test)))
		(when (not (keim~equal variable new-term))
		  (subst~insert-component variable new-term new-substitution))))
	  (subst~domain substitution) (subst~codomain substitution))
    new-substitution))

(defmethod top~replace-terms! ((substitution subst+substitution) old-terms new-terms &key (test #'keim~equal))
  (declare (edited  "05-NOV-1991 10:32")
	   (authors RICHTS)
	   (input   "A substitution and two termlists of equal length.")
	   (effect  "SUBSTITUTION is changed destructively.")
	   (value   "The changed SUBSTITUTION where all occurrences of OLD-TERMS in the codomain of SUBSTITUTION"
		    "(tested for equality with TEST) are replaced by the corresponding terms in NEW-TERMS."))
  (let ((domain (subst~domain substitution))
	(codomain (subst~codomain substitution))
	(flag nil))
    (mapl #'(lambda (domain-tail codomain-tail)
	      (let ((new-term (top~replace-terms! (car codomain-tail) old-terms new-terms :test test)))
		(setf (car codomain-tail) new-term)
		(when (eq (car domain-tail) new-term)
		  (setf flag t
			(car domain-tail) nil
			(car codomain-tail) nil))))
	  domain codomain)
    (when flag
      (subst=write-domain! (delete nil domain) substitution)
      (subst=write-codomain! (delete nil codomain)  substitution))
    substitution))

(defmethod top~replace-terms!! ((substitution subst+substitution) old-terms new-terms &key (test #'keim~equal))
  (declare (edited  "04-NOV-1991 10:27")
	   (authors RICHTS)
	   (input   "A substitution and two termlists of equal length.")
	   (effect  "SUBSTITUTION and its terms are changed destructively.")
	   (value   "The changed SUBSTITUTION where all occurrences of OLD-TERMS in the codomain of SUBSTITUTION"
		    "(tested for equality with TEST) are replaced by the corresponding terms in NEW-TERMS."))
  (let ((domain (subst~domain substitution))
	(codomain (subst~codomain substitution))
	(flag nil))
    (mapl #'(lambda (domain-tail codomain-tail)
	      (let ((new-term (top~replace-terms!! (car codomain-tail) old-terms new-terms :test test)))
		(when (eq (car domain-tail) new-term)
		  (setf flag t
			(car domain-tail) nil
			(car codomain-tail) nil))))
	  domain codomain)
    (when flag
      (subst=write-domain! (delete nil domain) substitution)
      (subst=write-codomain! (delete nil codomain) substitution))
    substitution))


(defmethod top=replace-free-variables-and-rename ((substitution subst+substitution) variables)
   (declare (edited  "04-NOV-1991 11:14")
	    (authors RICHTS)
	    (input   "A substitution where some variables are labeled with terms.")
	   #+comment
	    (warning "In order to obtain an idempotent substitution, codomain of OUTER-SUBSTITUTION"
		     "and domain of INNER-SUBSTITUTION must be variable disjoint.")
	    (effect  "Unlabeled variables in the codomain of substitution which are not member of VARIABLES"
		     "are labeled with a new variable.")
	    (value   "A new substitution with new, unshared terms"
		     "where the variables in the codomain are replaced with their symbol-label"
		     "if they have one or get one."))
   (let ((new-substitution (subst~create nil nil)))
     (mapc #'(lambda (variable old-term)
	       (let ((new-term (top=replace-free-variables-and-rename old-term variables)))
		 (unless (keim~equal variable new-term)
		   (subst~insert-component! (term~copy variable) new-term new-substitution))))
	  (subst~domain substitution) (subst~codomain substitution))
     new-substitution))

(defmethod top=replace-free-variables-and-rename! ((substitution subst+substitution) variables)
    (declare (edited  "04-NOV-1991 11:17")
	     (authors RICHTS gk)
	     (input   "A substitution where some variables are labeled with terms.") 
	   #+comment
	     (warning "In order to obtain an idempotent substitution, the codomain of SUBSTITUTION"
		      "and the variables of the terms in the labels must be variable disjoint.")
	     (effect  "Unlabeled variables in the codomain of substitution which are not member of VARIABLES"
		      "are labeled with a new variable."
		      "The terms in the codomain-list of SUBSTITUTION are replaced"
		      "by the terms with the variable-labels inserted.")
	     (value   "The changed SUBSTITUTION."))
    (let ((domain (subst~domain substitution))
	  (codomain (subst~codomain substitution))
	  (flag nil))
      (mapl #'(lambda (domain-tail codomain-tail)
		(let ((new-term (top=replace-free-variables-and-rename! (car codomain-tail) variables)))
		  (setf (car codomain-tail) new-term)
		  (when (eq (car domain-tail) new-term)
		    (setf flag t
			  (car domain-tail) nil
			  (car codomain-tail) nil))))
	    domain codomain)
      (when flag
	(subst=write-domain! (delete nil domain) substitution)
	(subst=write-codomain! (delete nil codomain) substitution))
      substitution))

(defmethod top=replace-free-variables-and-rename!! ((substitution subst+substitution) variables)
   (declare (edited  "04-NOV-1991 11:17")
	    (authors RICHTS gk)
	    (input   "A substitution with unshared terms, i.e. each term-box appears only once in SUBSTITUTION,"
		     "where some variables are labeled with terms.") 
	   #+comment
	    (warning "In order to obtain an idempotent substitution, the domain of SUBSTITUTION"
		     "and the terms in the label-cells must be variable disjoint.")
	    (effect  "Unlabeled variables in the codomain of substitution which are not member of VARIABLES"
		     "are labeled with a new variable."
		     "The labeled variables are replaced destructively with their labels.")
	    (value   "The changed SUBSTITUTION."))
   (let ((domain (subst~domain substitution))
	 (codomain (subst~codomain substitution))
	 (flag nil))
     (mapl #'(lambda (domain-tail codomain-tail)
	       (let ((new-term (top=replace-free-variables-and-rename!! (car codomain-tail) variables)))
		 (when (eq (car domain-tail) new-term)
		   (setf flag t
			 (car domain-tail) nil
			 (car codomain-tail) nil))))
	   domain codomain)
     (when flag
       (subst=write-domain! (delete nil domain) substitution)
       (subst=write-codomain! (delete nil codomain)) substitution)
     substitution))

 

;;;POST interface


(defmethod post~print ((substitution subst+substitution) stream)
  (if (subst~empty-p substitution)
      (format stream "(substitution () ())")
    (progn 
      (format stream "(substitution (")
      (post~print (subst~domain substitution) stream)
      (format stream ") (")
      (post~print (subst~codomain substitution) stream)
      (format stream "))"))))

(defmethod post~read-object (sub (env env+environment) 
			    (indicator (eql :substitution)))
  (let ((domain
	 (mapcar #'(lambda (x) (sym~read-variable x env))
		 (car sub)))
	(codomain
	 (mapcar #'(lambda (x) (term~read x env))
		 (cadr sub))))
    (subst~create domain codomain)))

(defun subst~read (sub env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A substitution SUB in POST format and an environment.")
	   (effect  "The substitution is constructed.")
	   (value   "The new substitution is returned."))
  (post~read-object (cdr sub) env :substitution))
