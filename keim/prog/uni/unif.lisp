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

(mod~defmod uni :uses (appl keim mod subst sym term top )
	    :documentation "Abstract interface to unification and matching, 
                includes algorithms and the unification (matching)-problem datastructure."
	    :exports (
		      uni~unify
		      uni~unify-list
		      uni~unify-multi
		      uni~match
		      uni~match-list
		      uni~renamed
		      uni~renamed-list
		      uni+unification-problem
		      uni~up-create
		      uni~up-list-create
		      uni~up-multi-create
		      uni~mp-create
		      uni~mp-list-create
		      uni~pop-solution
		      uni~top-solution
		      uni~remaining-solutions
		      uni~previous-solutions
		      uni~initial-problem
		      uni~merge
		      )
	    )

#{
\section{Unification}
In this module we define various unification procedures.  
 Only {\vb UNI~UNIFY} and {\vb UNI~MATCH} have been implemented so far.
\section{Simple functions}
#}

(defun uni~unify (term1 term2 &key (eq-test #'keim~equal))
  (declare (edited  "26-JAN-1992 12:57")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "A substitution that is a most general unifier for TERM1 and TERM2,"
		    "or NIL if it does not exist."))
  (uni=unify-free-terms term1 term2 :eq-test eq-test))

(defun uni~unify-list (termlist1 termlist2)
  (declare (edited  "26-JAN-1992 12:58")
	   (authors RICHTS)
	   (input   "Two termlists of equal length.")
	   (effect  "None.")
	   (value   "A substitution that is a most general unifier for the terms in TERMLIST1"
		    "and the corresponding terms in TERMLIST2, or NIL if it does not exist.")))

(defun uni~unify-multi (termlists)
  (declare (edited  "26-JAN-1992 13:00")
	   (authors RICHTS)
	   (input   "A list of lists of terms.")
	   (effect  "None.")
	   (value   "A substitution that is a most general unifier for all terms in each termlist in TERMLISTS,"
		    "or NIL if it does not exist.")))

(defun uni~match (term1 term2  &key (eq-test #'keim~equal))
  (declare (edited  "26-JAN-1992 13:01")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "A substitution that is a most general matcher for TERM1 to TERM2,"
		    "or NIL if it does not exist."))
  (uni=match-free-terms term1 term2 :eq-test eq-test))

(defun uni~match-list (termlist1 termlist2)
  (declare (edited  "26-JAN-1992 13:02")
	   (authors RICHTS)
	   (input   "Two termlists of equal length.")
	   (effect  "None.")
	   (value   "A substitution that is a most general matcher for the terms in TERMLIST1"
		    "to the corresponding terms in TERMLIST2, or NIL if it does not exist.")))

(defun uni~renamed (term1 term2)
  (declare (edited  "26-JAN-1992 13:03")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "A substitution that is a variable renaming for TERM1 to TERM2,"
		    "or NIL if it does not exist.")))

(defun uni~renamed-list (termlist1 termlist2)
  (declare (edited  "26-JAN-1992 13:04")
	   (authors RICHTS)
	   (input   "Two termlists of equal length.")
	   (effect  "None.")
	   (value   "A substitution that is a variable renaming for the terms in TERMLIST1"
		    "and the corresponding terms in TERMLIST2, or NIL if it does not exist.")))

;;;begin{latex}
;;;\section{Unification problems}
;;; A unification problem is an object with two slots, the initial-problem
;;; and previously-found solutions.
;;; It would be good to have a class which represented a generator approach,
;;; that is, one which had a slot for previously-found solutions, and a slot
;;; for a continuation that contained the current state of the unification
;;; process and could be called to obtain the ``next'' solution.
;;;end{latex}

(eval-when (load compile eval)
(defclass uni+unification-problem (keim+object)
  ((initial-problem :initarg :initial-problem :reader uni=initial-problem)
   (previous-solutions :initarg :previous-solutions :reader uni=previous-solutions))
  (:documentation "The class of unification problems.")))

(defun uni~up-create (term1 term2)
  (declare (edited  "26-JAN-1992 13:08")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "A unification-problem representing the unifiers of TERM1 and TERM2.")))



;;;begin{latex}
;;;\subsection{Functions for creating unification problems}
;;;end{latex}


(defun uni~up-list-create (termlist1 termlist2)
  (declare (edited  "26-JAN-1992 13:08")
	   (authors RICHTS)
	   (input   "Two termlists of equal length.")
	   (effect  "None.")
	   (value   "A unification-problem representing the unifiers of the terms in TERMLIST1"
		    "and the corresponding terms in TERMLIST2.")))

(defun uni~up-multi-create (termlists)
  (declare (edited  "26-JAN-1992 13:08")
	   (authors RICHTS)
	   (input   "A list of lists of terms.")
	   (effect  "None.")
	   (value   "A unification-problem representing the unifiers of all terms in each termlist in TERMLISTS.")))

(defun uni~mp-create (term1 term2)
  (declare (edited  "26-JAN-1992 13:08")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "A unification-problem representing the matchers of TERM1 to TERM2.")))

(defun uni~mp-list-create (termlist1 termlist2)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "Two termlists of equal length.")
	   (effect  "None.")
	   (value   "A unification-problem representing the matchers of the terms in TERMLIST1"
		    "to the corresponding terms in TERMLIST2.")))

;;;\subsection{Functions for operating on unification problems}

(defgeneric uni~pop-solution (up)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "A unification-problem.")
	   (effect  "The actual solution, a unifier for the unification-problem UP, is added to the previous solutions of UP"
		    "and a new solution becomes the actual solution.")
	   (value   "The old actual substitution (perhaps NIL).")))

(defgeneric uni~top-solution (up)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "A unification-problem.")
	   (effect  "None.")
	   (value   "The actual substitution, a unifier for the unification-problem UP or NIL.")))

(defgeneric uni~remaining-solutions (up)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "A unification-problem.")
	   (effect  "All remaining (not yet popped) solutions for UP (including the actual one) are computed"
		    "and added to the previous solutions of UP, the actual solution becomes NIL.")
	   (value   "All new substitutions (including the old actual one).")))

(defgeneric uni~previous-solutions (up)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "A unification-problem.")
	   (effect  "None.")
	   (value   "All substitutions that have been popped with UNI~POP-SOLUTION or
UNI~REMAINING-SOLUTIONS."))
  (:method ((up uni+unification-problem))
	   (uni=previous-solutions up)))

(defgeneric uni~initial-problem (up)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "A unification-problem.")
	   (effect  "None.")
	   (value   "A list of lists of terms, the unification-problem was created with."))
  (:method ((up uni+unification-problem))
	   (uni=initial-problem up)))


(defgeneric uni~merge (up1 up2)
  (declare (edited  "26-JAN-1992 13:09")
	   (authors RICHTS)
	   (input   "Two unification-problems.")
	   (effect  "None.")
	   (value   "A unification-problem representing unifiers of the merge of UP1 and UP2.")))

;; Unification Algorithms

(defun uni=unify-free-terms (term1 term2  &key (eq-test #'keim~equal))
  (unwind-protect
       (progn
	 (top~new-binding-list!)
	 (if (if (listp term1)
		 (uni=unify-free-terms-rec (car term1) (car term2) (cdr term1) (cdr term2) :eq-test eq-test)
		 (uni=unify-free-terms-rec term1 term2 nil nil :eq-test eq-test))
	     (subst~create-binding-substitution)))
    (top~unbind-binding-list!)))

(defgeneric uni=unify-free-terms-rec (term1 term2 termlist1 termlist2  &key eq-test )
  (:method ((t1 (eql nil)) (t2 (eql nil)) termlist1 termlist2 &key (eq-test #'keim~equal))
   (when (or termlist1 termlist2)
     (error "Programmierfehler: Es sind noch Terme da."))
   t)
  (:method ((variable1 sym+var) (variable2 sym+var) termlist1 termlist2  &key (eq-test #'keim~equal))
   (let ((binding1 (term~binding variable1))
	 (binding2 (term~binding variable2)))
     (cond ((funcall eq-test variable1 variable2)
	    (uni=unify-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2) :eq-test eq-test))
	   ((and binding1 binding2)
	    (uni=unify-free-terms-rec binding1 binding2 termlist1 termlist2  :eq-test eq-test))
	   (binding1
	    (uni=unify-free-terms-rec binding1 variable2 termlist1 termlist2  :eq-test eq-test))
	   (binding2
	    (uni=unify-free-terms-rec variable1 binding2 termlist1 termlist2  :eq-test eq-test))
	   (t (top~bind-term! variable1 variable2)
	      (uni=unify-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2)  :eq-test eq-test)))))
  (:method ((variable sym+var) (term term+term) termlist1 termlist2  &key (eq-test #'keim~equal))
   (cond ((term~binding variable)
	  (uni=unify-free-terms-rec (term~binding variable) term termlist1 termlist2 :eq-test eq-test))
	 ((uni=occurs-p variable term :eq-test eq-test) nil)
	 (t
	  (top~bind-term! variable term)
	  (uni=unify-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2)  :eq-test eq-test))))
  (:method ((term term+term) (variable sym+var) termlist1 termlist2  &key (eq-test #'keim~equal))
   (uni=unify-free-terms-rec variable term termlist1 termlist2 :eq-test eq-test))
  (:method ((constant1 sym+const) (constant2 sym+const) termlist1 termlist2  &key (eq-test #'keim~equal))
   (if (funcall eq-test constant1 constant2)
       (uni=unify-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2) :eq-test eq-test)
       nil))
  (:method ((application1 appl+appl) (application2 appl+appl) termlist1 termlist2  &key (eq-test #'keim~equal))
   (if (funcall eq-test (appl~function application1) (appl~function application2))
       (let ((arguments1 (appl~arguments application1))
	     (arguments2 (appl~arguments application2)))
	 (when (/= (length arguments1) (length arguments2))
	   (error "~A and ~A have different numbers of arguments are not first order terms"))
	 (uni=unify-free-terms-rec (car arguments1) (car arguments2) (append (cdr arguments1) termlist1) (append (cdr arguments2) termlist2) :eq-test eq-test))
       nil))
  (:method ((term1 term+term) (term2 term+term) termlist1 termlist2  &key (eq-test #'keim~equal))
   (declare (ignore termlist1 termlist2))
   (if (funcall eq-test (term~top term1) (term~top term2))
       (error "Programmierfehler: Methoden falsch definiert; Topsymbole sind gleich."))
   nil))


(defgeneric uni=occurs-p (variable obj   &key eq-test)
  (:method (variable1 (variable2 sym+var)   &key (eq-test #'keim~equal))
	   (cond ((funcall eq-test variable1 variable2) t)
		 ((term~binding variable2) (uni=occurs-p variable1 (term~binding variable2) :eq-test eq-test))
		 (t nil)))
  (:method (variable (constant sym+const)  &key (eq-test #'keim~equal))
	   (declare (ignore variable))
	   nil)
  (:method (variable (application appl+appl) &key (eq-test #'keim~equal))
	   (some #'(lambda (argument)
		     (uni=occurs-p variable argument :eq-test eq-test))
		 (appl~arguments application))))


(defun uni=match-free-terms (term1 term2  &key (eq-test #'keim~equal))
  (unwind-protect
      (progn
	(top~new-binding-list!)
	(if (if (listp term1)
		(uni=match-free-terms-rec (car term1) (car term2) (cdr term1) (cdr term2) :eq-test eq-test)
		(uni=match-free-terms-rec term1 term2 nil nil :eq-test eq-test))
	    (subst~create-binding-substitution nil)))
    (top~unbind-binding-list!)))

(defgeneric uni=match-free-terms-rec (term1 term2 termlist1 termlist2  &key eq-test)
  (:method ((t1 (eql nil)) (t2 (eql nil)) termlist1 termlist2  &key (eq-test #'keim~equal))
   (when (or termlist1 termlist2)
     (error "Programmierfehler: Es sind noch Terme da."))
   t)
  (:method ((variable sym+var) term termlist1 termlist2  &key (eq-test #'keim~equal))
   (let ((binding (term~binding variable)))
     (cond ((term~binding variable)
	    (if (funcall eq-test binding term)
		(uni=match-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2) :eq-test eq-test)
		nil))
	   (t (top~bind-term! variable term)
	      (uni=match-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2) :eq-test eq-test)))))
  (:method ((constant1 sym+const) (constant2 sym+const) termlist1 termlist2  &key (eq-test #'keim~equal))
   (if (funcall eq-test constant1 constant2)
       (uni=match-free-terms-rec (car termlist1) (car termlist2) (cdr termlist1) (cdr termlist2) :eq-test eq-test)
       nil))
  (:method ((application1 appl+appl) (application2 appl+appl) termlist1 termlist2  &key (eq-test #'keim~equal))
   (if (funcall eq-test (appl~function application1) (appl~function application2))
       (let ((arguments1 (appl~arguments application1))
	     (arguments2 (appl~arguments application2)))
	 (when (/= (length arguments1) (length arguments2))
	   (error "~A and ~A have different numbers of arguments are not first order terms"))
	 (uni=match-free-terms-rec (car arguments1) (car arguments2) (append (cdr arguments1) termlist1) (append (cdr arguments2) termlist2) :eq-test eq-test))
       nil))
  (:method ((term1 term+term) (term2 term+term) termlist1 termlist2  &key (eq-test #'keim~equal))
   (declare (ignore termlist1 termlist2))
   (if (funcall eq-test (term~top term1) (term~top term2))
       (error "Programmierfehler: Methoden falsch definiert; Topsymbole sind gleich."))
   nil)) 



