;;; -*- Syntax: Common-Lisp; Package: KEIM; Base: 10; Mode: LISP -*-
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

(mod~defmod lit :uses (appl env fo keim  mod pos post term termc top )
	    :documentation "Abstract datatype for literals."
	    :exports (
		      lit+literal
		      lit~literal-create
		      lit~literal-p
		      lit~clause
		      lit~set-clause!
		      lit~positive-p
		      lit~set-polarity!
		      lit~atom
		      lit~set-atom!
		      lit~read
		      )
	    )

#{
\section{ Literals}
\label{mod:lit}

This module provides basic datastructures and algorithms for literals,
as they would be used in clauses.

\subsection{ General functions}

A literal is an object with the following slots: the clause in which
it occurs, its atom and its polarity. To make available a mapping from literals
to their clauses literals have an additional slot CLAUSE that
contains the clause in which the literal occurs. This slot is set when
a clause is created (see module clause, section \ref{mod:cl}).
#}
 
(defvar lit*literal-counter 0)

(eval-when (load compile eval)
(defclass lit+literal (keim+name keim+object)
    ((clause :initarg :clause :reader lit=clause :writer lit=write-clause!)
     (atom :initarg :atom :reader lit=atom :writer lit=write-atom!)
     (polarity :initarg :polarity :reader lit=polarity :writer lit=write-polarity!
	       :documentation "This slot is t if the literal is positive, nil if it is negated."))
    (:documentation "The class of literals, i.e. atoms or negated atoms.")))


(defun lit~literal-create (term polarity &key name (name-prefix "L-"))
  (declare (edited  "1-7-1992")
	   (authors RICHTS)
	   (input   "A term with type o, a flag, a string or a symbol and a string.")
	   (effect  "None.")
	   (value   "A newly created literal with TERM as its atom, the polarity POLARITY and"
		    "the name NAME if NAME was supplied or the name '<NAME-PREFIX>-<number>' else."))
  (when (lit~literal-p term) (break  "KEIM: ~A which should be an atom is already a literal" term))
  (make-instance 'lit+literal :atom term :polarity polarity :clause nil
		 :name (if name (string name) (format nil "~A~A" name-prefix (incf lit*literal-counter)))))

(defun lit~literal-p (object)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if OBJECT is a literal, otherwise NIL."))
  (typep object 'lit+literal))

(defgeneric lit~clause (literal)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "The clause LITERAL belongs to (may be NIL)"))  
  (:method ((literal lit+literal))
	   (lit=clause literal)))

(defgeneric lit~set-clause! (literal clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal and a clause it belongs to.")
	   (effect  "The clause of the literal is changed to CLAUSE.")
	   (value   "Undefined."))
  (:method ((literal lit+literal) clause)
	   (lit=write-clause! clause literal)))

(defgeneric lit~positive-p (literal)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "T if  LITERAL is positive (not negated), otherwise NIL."))
  (:method ((literal lit+literal))  (lit=polarity literal)))

(defgeneric lit~set-polarity! (literal polarity)
  (declare (edited  "17-JAN-1993 12:46")
	   (authors nesmith)
	   (input   "A literal and its new polarity, non-nil for positive, NIL for negative.")
	   (effect  "The polarity of the literal is changed to POLARITY.")
	   (value   "Undefined."))
  (:method ((literal lit+literal) polarity)
	   (lit=write-polarity! (if polarity t nil) literal)))

(defgeneric lit~atom (literal)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal")
	   (effect  "None.")
	   (value   "The logical atom."))
  (:method ((literal lit+literal))
	   (lit=atom literal)))

(defgeneric lit~set-atom! (literal atom)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A literal LITERAL and a term.")
	   (effect  "The atom of the literal is changed to ATOM, if ATOM is an atom,"
		    "i.e. a term of type O that is not negated' else error.")
	   (value   "Undefined."))
  (:method ((literal lit+literal) (atom term+term))
	   (if (termc~atom-p atom)
	       (lit=write-atom! atom literal)
	       (error "~A is not an atom" atom))))


(defmethod keim~copy ((literal lit+literal))
  (lit~literal-create (lit~atom literal) (lit~positive-p literal)))

(defmethod keim~equal ((literal1 lit+literal) (literal2 lit+literal))
  (and (or (and (lit~positive-p literal1) (lit~positive-p literal2))
	   (and (not (lit~positive-p literal1)) (not (lit~positive-p literal2))))
       (keim~equal (lit~atom literal1) (lit~atom literal2))))

(defmethod print-object ((literal lit+literal) stream)
  (format stream "~A~A" (if (lit~positive-p literal) "+" "-") (lit~atom literal)))



;term selectors on literals

#{\subsection{Term selectors and operations on literals}

To make the handling of literals easier the following functions for
terms and applications are defined for literals too:
{\vb TERM~TOP}, {\vb APPL~ARGUMENTS}, {\vb APPL~SET-ARGUMENTS!}, {\vb
TERM~SUBTERMS}, {\vb FO~ARGUMENTS}, {\vb FO~SET-ARGUMENTS!}.#}


(defmethod term~top ((literal lit+literal))
  (term~top (lit~atom literal)))

(defmethod appl~arguments ((literal lit+literal))
  (appl~arguments (lit~atom literal)))

(defmethod appl~set-arguments! ((literal lit+literal) new-arguments)
  (appl~set-arguments! (lit~atom literal) new-arguments))

(defmethod term~subterms ((literal lit+literal))
  (term~subterms (lit~atom literal)))


;Term operations on literals

#{The following term operations are defined for literals:
{\vb TERM~COPY}, {\vb TOP~FREE-VARIABLES}, {\vb TERMC~ALL-BOUND-VARIABLES}, {\vb TERMC~GROUND-P}, {\vb
TOP~TERM-LINEAR-P}, {\vb TERM~POSITIONS}, {\vb TERM~POSITION}, {\vb TERM~AT-POSITION}, {\vb
TOP~REPLACE-TERMS(!!)}, {\vb TOP~REPLACE-AT-POSITION(!!)}, {\vb
TOP~REPLACE-FREE-VARIABLES-AND-RENAME(!!)}, {\vb TERM~UNSHARED-P}, {\vb
UNI~RENAMED}.#}


(defmethod term~copy ((literal lit+literal))
  (lit~literal-create (term~copy (lit~atom literal)) (lit~positive-p literal)))

(defmethod top~=free-variables ((literal lit+literal))
  (top~=free-variables (lit~atom literal)))

(defmethod termc~all-bound-variables ((literal lit+literal))
  (termc~all-bound-variables (lit~atom literal)))

(defmethod termc~ground-p ((literal lit+literal))
  (termc~ground-p (lit~atom literal)))

(defmethod top=term-linear-p ((literal lit+literal))
  (top~term-linear-p (lit~atom literal)))


(defmethod term~positions ((literal lit+literal) test)
  (let ((subpositions (term~positions (lit~atom literal) test)))
    (mapl #'(lambda (positions-tail)
	      (setf (car positions-tail) (pos~add-front 1 (car positions-tail))))
	  subpositions)
    (if (funcall test literal)
	(cons (pos~empty) subpositions)
	subpositions)))

(defmethod term~position ((literal lit+literal) test)
  (if (funcall test literal)
      (pos~empty)
      (let ((position (term~position (lit~atom literal) test)))
	(if position (pos~add-front 1 position) nil))))

(defmethod term~at-position ((literal lit+literal) position)
  (cond (( pos~empty-p position) literal)
	((= 1 (pos~first position)) (term~at-position (lit~atom literal) (pos~rest position)))
	(t (error "Position ~A not in literal ~A." position literal))))

(defmethod top~replace-terms ((literal lit+literal) old-terms new-terms &key (test #'keim~equal))
  (lit~literal-create (top~replace-terms (lit~atom literal) old-terms new-terms :test test)
		      (lit~positive-p literal)))

(defmethod top~replace-terms! ((literal lit+literal) old-terms new-terms &key (test #'keim~equal))
  (lit~set-atom! literal (top~replace-terms! (lit~atom literal) old-terms new-terms :test test))
  literal)

(defmethod top~replace-terms!! ((literal lit+literal) old-terms new-terms &key (test #'keim~equal))
  (lit~set-atom! literal (top~replace-terms!! (lit~atom literal) old-terms new-terms :test test))
  literal)


(defmethod top~replace-at-position ((literal lit+literal) position new-term)
  (cond ((pos~empty-p position) (keim~copy new-term))
	((= 1 (pos~first position))
	 (lit~literal-create (top~replace-at-position (lit~atom literal) (pos~rest position) new-term)
			     (lit~positive-p literal)))
	(t (error "Position ~A not in literal ~A." position literal))))

(defmethod top~replace-at-position! ((literal lit+literal) position new-term)
  (cond ((pos~empty-p position) (keim~copy new-term))
	((= 1 (pos~first position))
	 (lit~set-atom! literal (top~replace-at-position! (lit~atom literal) (pos~rest position) new-term))
	 literal)
	(t (error "Position ~A not in literal ~A." position literal))))

(defmethod top~replace-at-position!! ((literal lit+literal) position new-term)
  (cond ((pos~empty-p position) (keim~copy new-term))
	((= 1 (pos~first position))
	 (lit~set-atom! literal (top~replace-at-position!! (lit~atom literal) (pos~rest position) new-term))
	 literal)
	(t (error "Position ~A not in literal ~A." position literal))))


(defmethod top=replace-free-variables-and-rename ((literal lit+literal) variables)
  (lit~literal-create (top=replace-free-variables-and-rename (lit~atom literal) variables)
		      (lit~positive-p literal)))

(defmethod top=replace-free-variables-and-rename! ((literal lit+literal) variables)
  (lit~set-atom! literal (top=replace-free-variables-and-rename! (lit~atom literal) variables))
  literal)

(defmethod top=replace-free-variables-and-rename!! ((literal lit+literal) variables)
  (top=replace-free-variables-and-rename!! (lit~atom literal) variables)
  literal)


(defmethod term=unshared-p ((literal lit+literal))
  (term=unshared-p (lit~atom literal)))



(defmethod uni=renamed-free-terms-rec ((literal1 lit+literal) (literal2 lit+literal) termlist1 termlist2)
  (uni=renamed-free-terms-rec (lit~atom literal1) (lit~atom literal2) termlist1 termlist2))


(defmethod fo~arguments ((literal lit+literal))
  (fo~arguments (lit~atom literal)))

(defmethod fo~set-arguments! ((literal lit+literal) arguments)
  (fo~set-arguments! (lit~atom literal) arguments))


#{\section{\post interface}

The \post\ syntax for literals is \\
\begin{postsyntax}
\syntax{\nt{literal}  ::= (+ \nt{term} ) | (- \nt{term}).}
\end{postsyntax}
The term should be an atom, i.~e. a term with type O. The function
{\vb POST~PRINT} prints this representation of an literal and {\vb LIT~READ}
can read it.#}



(defun lit~read (lit env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A literal in \\POST format and an environment.")
	   (effect  "The literal is constructed.")
	   (value   "The new literal is returned.")
	   (example "\\vb (lit~read '(+ (P a)) env)."
		    "\\vb (lit~read '(- (Q y)) env)."))
  (post~read-object lit env :literal))

(defmethod post~read-object (lit (env env+environment) 
				 (indicator (eql :literal)))
  (cond ((not (listp lit)) (post~error "~A which should be a post literal is not a list." lit))
	((not (= (length lit) 2)) (post~error "~A which should be a post literal has not two components." lit))
	((string= "+" (first lit))
	 (lit~literal-create (termc~read-atom (second lit) env) t))
	((string= "-" (first lit))
	 (lit~literal-create (termc~read-atom (second lit) env) nil))
	(t (post~error "~A which should be a post literal does not start with + or -." lit)))
#+weg(let ((pos t)
	(atom lit))
    (cond ((not (listp lit)) (setq atom lit))
	  ((string= "+" (car lit))
	   (setq atom (cadr lit)))
	  ((string= "-" (car lit))
	   (setq atom (cadr lit) pos nil))
	  (t ))
    (lit~literal-create (termc~read-atom atom env) pos)))


(defmethod post~print ((literal lit+literal) stream)
  (format stream "(~A " (if (lit~positive-p literal) '+ '-))
  (post~print (lit~atom literal) stream)
  (format stream ")"))


