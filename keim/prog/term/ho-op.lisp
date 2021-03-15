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


(mod~defmod hop :uses (abstr appl mod pos sym term top type )
	    :documentation "Higher-order functionality of terms."
	    :exports (
		      hop~matrix
		      hop~binder
		      hop~rename-bound-variables
		      hop~rename-bound-variables-aux
		      hop~beta-redex-p
		      hop~beta-normform-p
		      hop~contains-beta-redex-p
		      hop~beta-contract
		      hop~rename-top-variable
		      hop~beta-contract!
		      hop~beta-normform
		      hop~beta-normform!
		      hop~eta-redex-p
		      hop~eta-longform-p
		      hop~eta-expand
		      hop~eta-expand-1
		      hop~eta-longform
		      hop~eta-contract
		      hop~beta-equal-p
		      hop~eta-equal-p
		      hop~alpha-equal-p
		      hop~lambda-equal-p
		      )
	    )


#{\section{Higher-Order Term Operations}\label{mod:hop}
In this module the basic
procedure for manipulating $\lambda$-terms in Church's simple theory of
types are defined. In particular procedures for forming normal forms
and testing equality with respect to higher-order transformations are
given.
#}

#{\subsection{Selectors}#}
(term~defgeneric hop~matrix ((term))
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value  "1. The matrix of the term, i.e. if TERM is of the form ([X1]..[XN].M)"
		    "   where M is not an abstraction, the matrix is M;"
		    "2. The binder of the term, i.e. (X1 .. XN).")
	   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) -->"
                    "(G (F X) (G (F Y) Z)); "
                    "(X Y Z)"
		    " "
		    "(FORALL [A].(A C)) -->"
		    "(FORALL [A].(A C))"
		    "NIL"))
  (:method ((term sym+const)) (values term nil))
  (:method ((term sym+var)) (values term nil))
  (:method ((term appl+appl)) (values term nil))
  (:method ((term abstr+abstr))
	   (multiple-value-bind (matrix binder) (hop~matrix (abstr~scope term))
	     (values matrix (cons (abstr~bound-variable term) binder)))))
				       

(defun hop~binder (term)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value  "1. The binder of the term, i.e. if TERM is of the form ([X1]..[XN].M)"
		    "   where M is not an abstraction, the binder is (X1 .. XN);"
		    "2. The matrix of the term, i.e. M.")
	   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) -->"
                    "(X Y Z)"
                    "(G (F X) (G (F Y) Z))"
		    " "
		    "(FORALL [A].(A C)) -->"
		    "NIL"
		    "(FORALL [A].(A C))"))
  (multiple-value-bind (matrix binder) (hop~matrix term) (values binder matrix)))


#{\subsection{Alpha-Conversion}#}

(defun hop~rename-bound-variables (term)
  (declare (edited  "20-AUG-1991 9:55")
	   (authors Kohlhase)
	   (input   "TERM is any term.")
	   (effect  "None.")
	   (value   "A term where the variables bound by the binder of TERM and all other occurences"
		    "of these variable in the scope of TERM are substituted by new variables.")
	   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) --> [X1].[X2].[X3].(G (F X1) (G (F X2) X3))"))
  (or (hop~rename-bound-variables-aux term) term))

(term~defgeneric hop~rename-bound-variables-aux ((term))
  (declare
   (authors nesmith)
   (input   "TERM is any term.")
   (effect  "None.")
   (value   "A term wherein the variables of all abstractions have been renamed, if there are any;
If TERM contains no abstractions, returns nil, otherwise returns a new term with as much sharing
as possible with the input TERM.")
   (example "[X].[Y].[Z].(G (F X) (G (F Y) Z)) --> [X1].[X2].[X3].(G (F X1) (G (F X2) X3))"))
  (:method ((term abstr+abstr))
    (let* ((oldscope (abstr~scope term))
	   (newscope (hop~rename-bound-variables oldscope)))
      (hop~rename-top-variable 
       (if (eq oldscope newscope)
	   term
	   (abstr~create (abstr~bound-variable term) newscope)))))
  (:method ((term sym+sym))
    nil)
  (:method ((term appl+appl))
    (let* ((oldfun (appl~function term))
	   (newfun (hop~rename-bound-variables oldfun))
	   (oldargs (appl~arguments term))
	   (newargs (mapcar #'hop~rename-bound-variables oldargs)))
      (unless (and (eq oldfun newfun) (every #'eq oldargs newargs))
	(appl~create newfun newargs)))))

#{\subsection{Beta-Normalform}#}

(term~defgeneric hop~beta-redex-p ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value   "T iff  TERM is of the form (([x].A)B)")
	   (example "([x] (P x) a) --> t"))
  (:method ((term sym+const))
	   nil)
  (:method ((term sym+var)) nil)
  (:method ((term abstr+abstr)) nil)
  (:method ((term appl+appl))
	   (abstr~p (appl~function term))))


(defun hop~beta-normform-p (term)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "T iff TERM is in beta-normal form, i.e. TERM does not contain beta-redices.")
	   (example "([X].(Q X) C) --> nil"
		    "[X].(F (F X)) --> t"))
  (not (hop~contains-beta-redex-p term)))

(term~defgeneric hop~contains-beta-redex-p ((term))
  (declare (edited  "27-JAN-1991 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "T iff TERM contains a beta-redex")
	   (example "([X].(Q X) C) --> t"
		    "(FORALL [X].(Q X)) --> nil"))
  (:method ((term sym+const)) nil)
  (:method ((term sym+var)) nil)
  (:method ((term abstr+abstr)) (hop~contains-beta-redex-p (abstr~scope term)))
  (:method ((term appl+appl))
	   (let ((fun (appl~function term)))
	     (if (abstr~p fun)
		 t
		 (or (hop~contains-beta-redex-p fun)
		     (some #'hop~contains-beta-redex-p (appl~arguments term)))))))

; make pos default to empty (top level redex) DAN
(defun hop~beta-contract (term &optional (pos (pos~empty)))
  (declare (edited  "4-Feb-1993 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term and POS is a valid position in TERM. POS defaults to the empty position.")
	   (effect  "None.")
	   (value   "If the subterm of TERM at POS is a beta-redex of the form (([x].A)B)"
		    " then a new term is returned where the subterm at POS is changed to [B/x]A,"
		    "otherwise TERM.")
	   (example "([X].(P X) C) --> (P C)"
		    "([X].(X C) P) --> (P C)"
		    "([X].[Y].(G X Y) C D) --> ([Y].(G C Y) D)"
		    "([X].[Y].(G X Y) C D)  (1) --> ([Y].(G C Y) D)"
		    "([X].[Y].(G X Y) C D)  (1 0) --> ([X].[Y].(G X Y) C D)"))
  (let ((atpos (term~at-position term pos)))
    (if (hop~beta-redex-p atpos)
	(top~replace-at-position term pos (hop=beta-contract atpos))
	term)))

(term~defgeneric hop=beta-contract ((term))
  (declare (edited  "27-JAN-1992 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term.")
	   (effect  "None.")
	   (value   "If TERM is a beta-redex of the form (([x].A)B)"
		    " then a new term is [B/x]A, is returned, otherwise TERM."))
  (:method ((term sym+const)) term)
  (:method ((term sym+var)) term)
  (:method ((term appl+appl))
	   (let ((ab (appl~function term))
		 (arg (first (appl~arguments term))))
	     (if (abstr~p ab)
		 (appl~create
		  (hop=subst-term-var-rename arg (abstr~bound-variable ab)
					     (abstr~scope ab))					     
		  (cdr (appl~arguments term)))
		 term)))
  (:method ((term abstr+abstr)) term))

(defun hop=subst-term-var-rename (term var inwff)
  (or (hop=subst-term-var-rename-aux term var inwff) inwff))

(term~defgeneric hop=subst-term-var-rename-aux ((term) (var) (wff))
 (declare
  (authors nesmith)
  (input "A TERM to be substituted for a VAR in a WFF.")
  (effect "none")
  (value "nil if no substitution was made, otherwise a new wff, sharing as much
of the previous structure as possible is returned"))
 (:method ((term term+term) (var sym+sym) (wff sym+sym))
   (if (term~equal-p var wff) term nil))
 (:method ((term term+term) (var sym+sym) (wff abstr+abstr))
   (cond ((term~equal-p (abstr~bound-variable wff) var)
	  nil)
	 ((member (abstr~bound-variable wff) (top~all-unbound-symbols term) :test #'term~equal-p)
	  (if (member var (top~all-unbound-symbols (abstr~scope wff)) :test #'term~equal-p)
	      (hop=subst-term-var-rename-aux term var (hop~rename-top-variable wff))
	      nil))
	 (t (let ((newwff (hop=subst-term-var-rename-aux term var (abstr~scope wff))))
	      (if newwff 
		  (abstr~create (abstr~bound-variable wff) newwff)
		  nil)))))
 (:method ((term term+term) (var sym+sym) (wff appl+appl))
   (let* ((oldfun (appl~function wff))
	  (newfun (or (hop=subst-term-var-rename-aux term var (appl~function wff))
		      oldfun))
	  (oldargs (appl~arguments wff))
	  (newargs (mapcar #'(lambda (x) (or (hop=subst-term-var-rename-aux term var x) x))
			   oldargs)))
     (unless (and (eq newfun oldfun) (every #'eq oldargs newargs))
       (appl~create newfun newargs)))))

(defun hop~rename-top-variable (abstr)
  (let ((newvar (sym~rename-var (abstr~bound-variable abstr))))
    (abstr~create newvar 
		  (hop=subst-term-var-rename newvar 
					     (abstr~bound-variable abstr)
					     (abstr~scope abstr)))))

(defun hop~beta-contract! (term &optional pos)
  (declare (edited  "27-JAN-1992 9:55")
	   (authors KERBER Kohlhase)
	   (input   "TERM is a term and POS is a valid position in TERM.")
	   (effect  "If the subterm of TERM at POS is a beta-redex of the form (([x].A)B)"
		    " then it is changed destructively to [B/x]A, otherwise nothing is changed.")
	   (value   "The changed TERM.")
	   (example "destructive version of hop~beta-contract, see there"))
  (let ((atpos (if pos
		   (term~at-position term pos)
		   term)))
    (if (hop~beta-redex-p atpos)
        (progn (hop=beta-contract! atpos) term)
	term)))

(term~defgeneric hop=beta-contract! ((term))
  (declare (edited  "27-JAN-1992 9:55")
	   (authors KERBER Kohlhase)
	   (input   "TERM is a term and POS is a valid position in TERM.")
	   (effect  "None.")
	   (value   "If TERM is a beta-redex of the form (([x].A)B)"
		    " then a new term is [B/x]A, is returned,otherwise TERM."))
  (:method ((term sym+const)) term)
  (:method ((term sym+var)) term)
  (:method ((term appl+appl))
	   (let ((ab (appl~function term))
		 (arg (first (appl~arguments term)))
		 (rest (rest (appl~arguments term))))
	     (cond ((abstr~p ab)
		    (let ((newfun
			   (top~replace-free-variables! 
			    (abstr~scope ab)
			    (list (abstr~bound-variable ab))
			    (list arg))))
		      (term~set-term term (if rest
					      (appl~create newfun rest)
					      newfun))))
		   (t term))))
  (:method ((term abstr+abstr)) term))


(defgeneric hop~beta-normform (term)
  (declare (edited  "20-AUG-1991 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "None.")
	   (value   "The beta-normal form of TERM.")
	   (example "([X].(Q X) C) --> (Q C)"))
  (:method ((term sym+sym))
	   term)
  (:method ((term abstr+abstr))
    (let* ((scope (abstr~scope term))
	   (newscope (hop~beta-normform scope)))
      (if (eq newscope scope)
	  term
	(abstr~create (abstr~bound-variable term) 
				 newscope))))
  (:method ((term appl+appl))
    (let* ((fun (appl~function term))
	   (newfun (hop~beta-normform fun))
	   (args (appl~arguments term))
	   (newargs (mapcar #'hop~beta-normform args)))
      (cond
       ((and (eq fun newfun)
	     (every #'eq args newargs)
	     (not (abstr~p fun)))
	  term)
       ((not (abstr~p newfun))
	(appl~create newfun newargs))
       (t
	(hop~beta-normform (hop~beta-contract 	
			    (appl~create newfun newargs))))
	)))
  )


(defgeneric hop~beta-normform! (term)
  (declare (edited  "20-AUG-1991 9:55")
	   (authors KERBER Kohlhase)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "Contracts TERM to beta-normal form.")
	   (value   "The changed TERM.")
	   (example "destructive version of hop~beta-normform, see there"))
  (:method ((term term+term))
	   (term~set-term term (hop~beta-normform term))));;inefficient

#{\subsection{Eta-equal}
The functions concerning eta-equality are already specified, but not yet implemented.#}

(term~defgeneric hop~eta-redex-p ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors Kohlhase)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "None.")
	   (value   "T iff TERM is of the form ([x].(Ax)).")
	   (example "Let Q have type (O I), P have type (O I I), then
\\begin{itemize}
\\item (hop~eta-redex-p Q) -> nil
\\item (hop~eta-redex-p [X].(Q X) ) -> t
\\item (hop~eta-redex-p [X].(P X X) ) -> nil
\\end{itemize}")
)
    (:method ((term term+term))
      nil)
    (:method ((abstr abstr+abstr))
      (let* ((var (abstr~bound-variable abstr))
	     (scope (abstr~scope abstr))
	     (args nil))
	(and (appl~p scope)
	     (let ((args (appl~arguments scope)))
	       (term~equal var (car (last args)))
	       (not (member var 
			    (top~all-unbound-symbols 
			     (appl~function scope))
			    :test #'term~equal))
	       (dotimes (n (1- (length args)) t)
		 (when (member var 
			       (top~all-unbound-symbols
				(nth n args))
			       :test #'term~equal)
		   (return nil))))))))

(term~defgeneric hop~eta-longform-p ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term or nested termlist.")
	   (effect  "None.")
	   (value   "T iff TERM is in eta-long form, i.e. the matrix of TERM has"
		    "elementary type and all arguments of the scope are in eta-long form.")
	   (example  "Let Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-longform-p Q) -> nil
\\item (hop~eta-longform-p [X].[Y].(Q X Y) ) -> t
\\end{itemize}"))
  (:method ((term sym+sym))
    (type~primitive-p (term~type term)))
  (:method ((term appl+appl))
    (and (type~primitive-p (term~type term))
	 (hop~eta-longform-p (appl~function term))
	 (every #'hop~eta-longform-p (appl~arguments term))))
    (:method ((term abstr+abstr))
      (let ((matrix (hop~matrix term)))
      (and (type~primitive-p (term~type matrix))
	   (if (appl~p matrix)
	       (every #'hop~eta-longform-p
		      (appl~arguments matrix))
	       t)))))


(term~defgeneric hop~eta-expand ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "If the matrix M TERM is of type (i1,..,in)->i,"
		    " then a new term is returned where the matrix is ([X1]..[Xn].(M X1 .. Xn)),"
		    "otherwise TERM (M has elementary type).")
	   (example "Let R have type (O (O I I)), Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-expand Q) -> [E1].[E2].(Q E1 E2)
\\item (hop~eta-expand (R Q)) -> (R Q)
\\end{itemize}" ))
    (:method ((term term+term))
      (multiple-value-bind (matrix binder) (hop~matrix term)
	  (let ((type (term~type matrix)))
	    (if (type~primitive-p type)
		term
		(let* ((newvar (sym~rename-var "E" (type~c-domain type)))
		       (newterm (abstr~create newvar (appl~create matrix (list newvar)))))
		  (hop~eta-expand
		   (dolist (oldvar (reverse binder) newterm)
		     (setq newterm (abstr~create oldvar newterm))))))))
    ))

(term~defgeneric hop~eta-expand-1 ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "If the TERM is of type (i1)->i,"
		    " then a new term is returned of the form [X1].(TERM X1)"
		    "otherwise TERM (M has elementary type).")
	   (example "Let Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-expand-1 Q) -> [E1] (Q E1)
\\item (hop~eta-expand-1 [X].(Q X)) -> [E2].([E1].(Q E1) E2)
\\end{itemize}")
	   )
    (:method ((term term+term))
      (let* ((matrix (hop~matrix term))
	     (type (term~type matrix)))
	(if (type~primitive-p type)
	    term
	    (let* ((newvar (sym~rename-var "E" (type~c-domain type))))
	      (abstr~create newvar (appl~create term (list newvar))))))
    ))

(term~defgeneric hop~eta-longform ((term))
 (declare
  (authors nesmith)
  (input "A term")
  (value "The term with all subterms in eta-longform.")
  (example "Let R have type (O (O I I)), Q have type (O I I), then
\\begin{itemize}
\\item (hop~eta-longform Q) -> [E1].[E2].(Q E1 E2)
\\item (hop~eta-longform R) -> [E3].(R E3)
\\item (hop~eta-longform (R Q)) -> (R [E4].[E5].(Q E4 E5))
\\end{itemize}"))
 (:method ((term abstr+abstr))
   (multiple-value-bind (matrix binder) (hop~matrix term)
     (hop~eta-expand
      (let ((newterm (hop~eta-longform matrix)))
	(dolist (oldvar (reverse binder) newterm)
	  (setq newterm (abstr~create oldvar newterm)))))))
  (:method ((term sym+sym))
    (hop~eta-expand term))
  (:method ((term appl+appl))
    (let* ((args (appl~arguments term))
	   (new-args (mapcar #'hop~eta-longform
			     args)))
      (if (every #'eq new-args args)
	  term
	  (appl~create (appl~function term)
		       new-args)))))




(term~defgeneric hop~eta-contract ((term))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERM is a term")
	   (effect  "None.")
	   (value   "If TERM is an eta-redex, then the result of contracting it
is returned, otherwise the TERM itself is returned.")
	   (example "Let P be of type (O I), Q of type (O I I), then
\\begin{itemize}
\\item (hop~eta-contract [X].(P X) ) -> P
\\item (hop~eta-contract [X].(Q X X) ) -> [X].(Q X X)
\\end{itemize}"
		    ))
  (:method ((term abstr+abstr))
    (if (hop~eta-redex-p term)
	(appl~create 
	 (appl~function (abstr~scope term))
	 (butlast (appl~arguments (abstr~scope term))))
	term)))



#{\subsection{Equality predicates}
#}

(term~defgeneric hop~beta-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are beta-equal, i.e. TERMi have the same beta-normal form."))
    (:method ((term1 term+term) (term2 term+term))
      (term~equal-ab (hop~beta-normform term1) (hop~beta-normform term2))))



(term~defgeneric hop~eta-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are eta-equal, e.g. TERMi have the same eta-long form."))
    (:method ((term1 term+term) (term2 term+term))
      (term~equal-ab (hop~eta-longform term1) (hop~eta-longform term2))))

(term~defgeneric hop~alpha-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are alpha-equal, i.e. TERM1 can be obtained"
                    "from TERM2 by renaming of bound variables."))
    (:method ((term1 term+term) (term2 term+term))
      (term~equal-ab term1 term2)))

(term~defgeneric hop~lambda-equal-p ((term1) (term2))
  (declare (edited  "20-AUG-1991 9:55")
	   (authors nesmith)
	   (input   "TERMi are terms.")
	   (effect  "None.")
	   (value   "T iff TERMi are alpha-beta-eta-equal."))
    (:method ((term1 term+term) (term2 term+term))
      (term~equal-p-ab
       (hop~eta-longform (hop~beta-normform term1))
       (hop~eta-longform (hop~beta-normform term2)))))

