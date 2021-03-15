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

(mod~defmod cl :uses ( env keim lit mod node pos post term termc top )
	    :documentation "Abstract datatype for clauses, i.e. sets of literals."
	    :exports (
		      cl+clause
		      cl~create
		      cl~clause-p
		      cl~literals
		      cl~set-literals!
		      cl~pos-literals
		      cl~neg-literals
		      cl~clause-set
		      cl~set-clause-set!
		      cl~empty-p
		      cl~true-p
		      cl~unit-p
		      cl~length
		      cl~insert-literal
		      cl~insert-literal!
		      cl~remove-literal
		      cl~remove-literal!
		      cl~read
		      cl~env-lookup
		      cl~with-context
		      )
	    )


#{\section{Clauses}
\label{mod:cl}

This module provides basic datastructures and algorithms for clauses,
as they would be used in resolution. It is their most important function 
to store a list of literals, which are logically connected conjunctive.

For a more efficent computation the clause class also contains two slots
for separated lists of the positive and negative literals. It also has
a slot for a clause set which can serve to divide the whole set of
clauses during the resolution process.

Because clauses should also be the nodes in a proof, they are a
subclass of {\vb NODE+NODE}.

\subsection{General Functions}

A clause is an object with the following slots: the list of literals
it contains, a list of its positive literals, a list of its negative literals
and the clause set it belongs to. Because a clause is also a proof
node it also inherits all slots of {\vb NODE+NODE} and the formula slot of
of {\vb NODE+NODE} is identical with the literal slot of {\vb
CL+CLAUSE}.

For representing the true clause,
the literal slot of an clause can be T. The empty clause is represented
by an empty list of literals, i.e. the literal slot is {\vb NIL}.
#}

(defvar cl*clause-counter 0 "a counter for creating unique clause names")

(eval-when (load compile eval)
(defclass cl+clause (node+node)
  ((formula :initarg :literals :reader cl=literals :writer cl=write-literals!)  ;new methods for the slot in node+node
   (pos-literals :initarg :pos-literals :reader cl=pos-literals :writer cl=write-pos-literals!)
   (neg-literals :initarg :neg-literals :reader cl=neg-literals :writer cl=write-neg-literals!)
   (clause-set :initarg :clause-set :reader cl=clause-set :writer cl=write-clause-set!))
  (:documentation "The class of clauses, i.e. sets of instances of CL+LITERAL.")))


(defun cl~create (literals &key justification name (name-prefix "C-"))
  (declare (edited  "1-7-1988")
	   (authors ohlbach)
	   (input   "A list of literals. Optional a justification and two parameters declaring the name:"
		    "something that can be coerced into a string and a string.")
	   (effect  "The clause slots of the literals are set to the new clause.")
	   (value   "The newly created clause."))
  (multiple-value-bind (pos-literals neg-literals)
      (cl=split-literals literals)
    (let ((clause (make-instance 'cl+clause
				 :literals literals :pos-literals pos-literals :neg-literals neg-literals
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf cl*clause-counter)))
					     (t (string name)))
				 :justification justification)))
      (mapc #'(lambda (literal)
		(lit~set-clause! literal clause))
	    literals)
      clause)))

#{The justification for a clause can store the way the clause was
constructed and represent its place in the proof tree. For further information
about nodes and justifications see the corresponding modules (section
\ref{mod:node} and \ref{mod:just} in chapter \ref{sys:problem}). Justifications for resolution, factoring and
paramodulation are defined in the resolution module (section \ref{mod:res}).

All functions in this module update the clause slot in a literal if its appearance
in a clause changes. Because a literal can only store one clause in its clause slot,
this mechanism only works correctly if every literal object appears in at most one clause.#}


(defun cl~clause-p (object)
  (declare (edited  "26-Jan-93")
	   (authors nesmith)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T if the object is a CL+CLAUSE, NIL otherwise."))
  (typep object 'cl+clause))

(defgeneric cl~literals (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The list of literals in CLAUSE."))
  (:method ((clause cl+clause))
	   (cl=literals clause)))

(defgeneric cl~set-literals! (clause literals)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause and a list of literals")
	   (effect  "The literals slot of CLAUSE is set to LITERALS (without copying the list)."
		    "The lists of positive and negative literals are updated."
		    "The clause slots of all involved literals are updated:"
		    "the clause slots of the literals in CLAUSE is set to NIL and the"
		    "the clause slots of the new LITERALS are set to CLAUSE.")
	   (value   "The changed CLAUSE."))
  (:method ((clause cl+clause) (literals (eql t)))
       (mapc #'(lambda (literal)
		(lit~set-clause! literal nil))
	    (cl~literals clause))
       (cl=write-pos-literals! nil clause)
       (cl=write-neg-literals! nil clause)
       (cl=write-literals! t clause))
  (:method ((clause cl+clause) literals)
     (mapc #'(lambda (literal)
		(lit~set-clause! literal nil))
	    (cl~literals clause))
     (multiple-value-bind (pos-literals neg-literals)
	 (cl=split-literals literals)
       (cl=write-pos-literals! pos-literals clause)
       (cl=write-neg-literals! neg-literals clause)
       (mapc #'(lambda (literal)
		(lit~set-clause! literal clause))
	    literals)
       (cl=write-literals! literals clause))))

(defgeneric cl~pos-literals (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The list of positive literals in CLAUSE.")) 
  (:method ((clause cl+clause))
	   (cl=pos-literals clause)))

(defgeneric cl~neg-literals (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The list of negative literals in CLAUSE.")) 
  (:method ((clause cl+clause))
	   (cl=neg-literals clause)))



(defgeneric cl~clause-set (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The clause set CLAUSE belongs to (may be nil).")) 
  (:method ((clause cl+clause))
	   (cl=clause-set clause)))

(defgeneric cl~set-clause-set! (clause clause-set)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause and a clause set.")
	   (effect  "The slot for the clause set of CLAUSE is set to CLAUSE-SET.")
	   (value   "The changed CLAUSE.")) 
  (:method ((clause cl+clause) clause-set)
	   (cl=write-clause-set! clause-set clause)))


(defmethod keim~copy ((clause cl+clause))
  (cl~create (cl~literals clause)
		    :justification (node~justification clause)))

(defmethod print-object ((clause cl+clause) stream)
  (declare (special *print-length*))
  (cond ((cl~empty-p clause) (format stream "[]"))
	((cl~true-p clause) (format stream "{T}"))
	(t (format stream "{[~A] ~A" (keim~name clause) (car (cl~literals clause)))
	   (do ((i (1- (or *print-length* 0)) (1- i))
		(literals (cdr (cl~literals clause)) (cdr literals)))
	       ((or (zerop i) (null literals))
		(if literals
		    (format stream " ...}")
		    (format stream "}")))
	     (format stream " ~A" (car literals))))))


(defun cl=split-literals (literals)
  (if literals
      (multiple-value-bind (pos-literals neg-literals)
	  (cl=split-literals (cdr literals))
	(if (lit~positive-p (car literals))
	    (values (cons (car literals) pos-literals) neg-literals)
	    (values pos-literals (cons (car literals) neg-literals))))
      (values nil nil)))


#{\subsection{Simple operations and predicates}#}

(defun cl~empty-p (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "True, iff CLAUSE is empty, i.e. it has zero literals."))
  (null (cl~literals clause)))

(defun cl~true-p (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "True, iff CLAUSE represents the true clause, i.e. the literals slot is T."))
  (eq t (cl~literals clause)))

(defun cl~unit-p (clause)
  (declare (edited  "29-NOV-1991 12:46")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "True, iff CLAUSE is a unit clause, i.e. it has exactly one literal."))
  (= 1 (cl~length clause)))

(defun cl~length (clause)
  (declare (edited  "29-NOV-1991 12:47")
	   (authors RICHTS)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The number of the literals in CLAUSE."))
  (length (cl~literals clause)))

(defgeneric cl~insert-literal (literal clause)
  (declare (edited  "28-NOV-1991 12:54")
	   (authors RICHTS ohlbach)
	   (input   "A literal and a clause.")
	   (effect  "None.")
	   (value   "A new clause with LITERAL added to the literals of CLAUSE."))
  (:method (literal (clause cl+clause))
   (cl~create (cons literal (cl~literals clause)))))

(defgeneric cl~insert-literal! (literal clause)
  (declare (edited  "28-NOV-1991 12:55")
	   (authors RICHTS ohlbach)
	   (input   "A literal and a clause.")
	   (effect  "The literal is inserted into the clause's literal lists.")
	   (value   "The changed CLAUSE."))
  (:method (literal (clause cl+clause))
	   (let ((new-literals (cons literal (cl=literals clause)))
		 (new-pos-literals (cons literal (cl=pos-literals clause)))
		 (new-neg-literals (cons literal (cl=neg-literals clause))))	 
	     (cl=write-literals! new-literals clause)
	     (if (lit~positive-p literal)
		 (cl=write-pos-literals! new-pos-literals clause)
		 (cl=write-neg-literals! new-neg-literals clause))
	     clause)))

(defgeneric cl~remove-literal (literal clause &key test)
  (declare (edited  "28-NOV-1991 12:59")
	   (authors RICHTS ohlbach)
	   (input   "A literal and a clause.")
	   (effect  "None.")
	   (value   "A new clause containing all literals which do not
satisfy the test with LITERAL."))
  (:method (literal (clause cl+clause) &key (test #'eql))
   (cl~create (remove literal (cl~literals clause) :test test))))

(defgeneric cl~remove-literal! (literal clause &key test)
  (declare (edited  "28-NOV-1991 12:58")
	   (authors RICHTS ohlbach)
	   (input   "A literal and a clause.")
	   (effect  "All literals in CLAUSE satisfying the TEST with LITERAL"
		    "are removed from the clause's literal list.")
	   (value   "The changed CLAUSE."))
  (:method (literal (clause cl+clause) &key (test #'eql))
   (cl=write-literals! (delete literal (cl=literals clause) :test test) clause)
   (if (lit~positive-p literal)
       (cl=write-pos-literals! (delete literal (cl=pos-literals clause) :test test) clause)
       (cl=write-neg-literals! (delete literal (cl=neg-literals clause) :test test) clause))
   clause))


#{\subsection{Term operations on clauses}#}


#{The following term operations are defined for clauses:
{\vb TERM~COPY}, {\vb TOP~FREE-VARIABLES}, {\vb TERMC~ALL-BOUND-VARIABLES},
{\vb TERMC~GROUND-P}, {\vb TOP~TERM-LINEAR-P}, {\vb TERM~POSITIONS},
{\vb TERM~POSITION}, {\vb TERM~AT-POSITION}, {\vb
TOP~REPLACE-TERMS(!!)}, {\vb TOP~REPLACE-AT-POSITION(!!)}, {\vb
TOP~REPLACE-FREE-VARIABLES-AND-RENAME(!!)}, {\vb TERM~UNSHARED-P}, {\vb
UNI~RENAMED} ({\em function\tt (!!)} stands for {\em function}, {\em function\tt !}
and {\em function\tt !!}).#}

(defmethod term~copy ((clause cl+clause))
  (cl~create (term~copy (cl~literals clause))))


(defmethod top~=free-variables ((clause cl+clause))
  (top~=free-variables (cl~literals clause)))

(defmethod termc~all-bound-variables ((clause cl+clause))
  (top~free-variables (cl~literals clause)))

(defmethod termc~ground-p ((clause cl+clause))
  (termc~ground-p (cl~literals clause)))

(defmethod term=linear-p ((clause cl+clause))
  (term=linear-p (cl~literals clause)))


(defmethod term~positions ((clause cl+clause) test)
  (term~positions (cl~literals clause) test))

(defmethod term~position ((clause cl+clause) test)
  (term~position (cl~literals clause) test))

(defmethod term~at-position ((clause cl+clause) position)
  (term~at-position (cl~literals clause) position))


(defmethod top~replace-terms ((clause cl+clause) old-terms new-terms &key (test #'keim~equal))
  (cl~create (top~replace-terms (cl~literals clause) old-terms new-terms :test test)))

(defmethod top~replace-terms! ((clause cl+clause) old-terms new-terms &key (test #'keim~equal))
  (cl=write-literals! (top~replace-terms! (cl~literals clause) old-terms new-terms :test test) clause)
  clause)

(defmethod top~replace-terms!! ((clause cl+clause) old-terms new-terms &key (test #'keim~equal))
  (cl=write-literals! (top~replace-terms!! (cl~literals clause) old-terms new-terms :test test) clause)
  clause)


(defmethod top~replace-at-position ((clause cl+clause) position new-term)
  (if (pos~empty-p position) 
      new-term
      (let ((number (pos~first position))
	    (pos-rest (pos~rest position))
	    (literals (term~copy (cl~literals clause))))
	(unless (< number (length literals))
	  (error "KEIM: Position ~A not in clause ~A." position clause))
	(setf (elt literals number) (top~replace-at-position (elt literals number) pos-rest new-term))
	(cl~create literals))))

(defmethod top~replace-at-position! ((clause cl+clause) position new-term)
  (if (pos~empty-p position) 
      new-term
      (let ((number (pos~first position))
	    (pos-rest (pos~rest position))
	    (literals (cl~literals clause)))
	(unless (< number (length literals))
	  (error "KEIM: Position ~A not in clause ~A." position clause))
	(setf (elt literals number) (top~replace-at-position! (elt literals number) pos-rest new-term))
	clause)))

(defmethod top~replace-at-position!! ((clause cl+clause) position new-term)
  (if (pos~empty-p position) 
      new-term
      (let ((number (pos~first position))
	    (literals (cl~literals clause)))
	(unless (< number (length literals))
	  (error "KEIM: Position ~A not in clause ~A." position clause))
	(setf (elt literals number) (top~replace-at-position!! (elt literals number) (pos~rest position) new-term))
	clause)))


(defmethod top=replace-free-variables-and-rename ((clause cl+clause) variables)
  (cl~create (top=replace-free-variables-and-rename (cl~literals clause) variables)))

(defmethod top=replace-free-variables-and-rename! ((clause cl+clause) variables)
  (cl=write-literals! (top=replace-free-variables-and-rename (cl~literals clause) variables) clause)
  clause)

(defmethod top=replace-free-variables-and-rename!! ((clause cl+clause) variables)
  (top=replace-free-variables-and-rename!! (cl~literals clause) variables)
  clause)


(defmethod uni=renamed-free-terms-rec ((clause1 cl+clause) (clause2 cl+clause) termlist1 termlist2)
  (if (= (cl~length clause1) (cl~length clause2))
      (let ((new-termlist1 (append (cl~literals clause1) termlist1))
	    (new-termlist2 (append (cl~literals clause2) termlist2)))
	(uni=renamed-free-terms-rec (car new-termlist1) (car new-termlist2) (cdr new-termlist1) (cdr new-termlist2)))
      nil))


(defmethod term=unshared-p ((clause cl+clause))
  (term=unshared-p (cl~literals clause))) 



#{\subsection{POST interface}


The \post\ syntax for literals is \\
\begin{postsyntax}
\syntax{ \nt{clause}  ::= (clause \nt{name} (\nt{var-dec}*) \nt{literal}*).}
\end{postsyntax}
The function {\vb POST~PRINT} prints this representation of an literal and {\vb CL~READ}
can read it.

Because the variables in a clause are implicitly universal quantified,
the declarations of the variables of a clause are printed in the \post\ syntax.
If a variable with a name occurring in the declaration already exists,
it is shadowed and inside the clause the new variable is used.
The variables declared in a clause are only known inside the clause; they
are removed from the environment after the clause has been created.

#}




(defmethod post~print ((clause cl+clause) stream)
  (format stream "(clause ~A (" (keim~name clause))
  (post~print-declaration (termc~all-bound-variables clause) stream)
  (format stream ") ")
  (post~print (cl~literals clause) stream)
  (format stream ")"))

(defmethod env~post-print (key (clause cl+clause) stream)
  (declare (ignore key))
  (post~print clause stream))


(defun cl~read (clause env)
  (declare (edited  "11-JUN-1993 12:46")
	   (authors nesmith)
	   (input   "A clause CLAUSE in \\post\\ format and an environment.")
	   (effect  "The clause is constructed.")
	   (value   "The new clause is returned."))
  (post~read-object (if (listp clause) 
			(cdr clause) 
			clause) 
		    env
		    (if (listp clause) 
			(intern (car clause) (find-package "KEYWORD"))
		      :clause)))


(defmethod post~read-object ((clause list) (env env+environment) 
			     (indicator (eql :clause)))
  (let* ((name (first clause))
	 (var-decs (second clause))
	 (literals (if (car (cddr clause))
		       (cddr clause)
		       nil)))
    ;; leave the variables in the environment 
    (dolist (var var-decs)
      (post~read-object var env :variable-multiple))
    (let* ((processed-lits (mapcar #'(lambda (lit) (lit~read lit env))
				   literals))
	   (newclause (cl~create processed-lits :name name))
	   (var-keys (mapcar #'car var-decs)))
      (env~enter name newclause env)
      (cl=save-context newclause var-keys env)
      (cl=remove-context newclause env)
      newclause)))

(defmethod post~read-object ((clause symbol) (env env+environment) 
			     (indicator (eql :clause)))
  (cl~env-lookup clause env))

(defmethod post~read-object ((clause string) (env env+environment) 
			     (indicator (eql :clause)))
  (cl~env-lookup clause env))

(defun cl~env-lookup (key env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A KEY and an environment ENV.")
	   (effect  "None.")
	   (value   "KEY is looked up in ENV.  If it is associated with a clause,"
		    "the clause will be returned, otherwise post~error is signaled."))
  (let ((obj (env~lookup-object key env)))
    (unless (cl~clause-p obj)
      (post~error "~A is a ~A in environment, not a clause" 
		  key (class-of obj)))
    obj))



#{\subsection{Operations for the context of a clause}

Because the variables defined by a clause are needed later
(for reading substitutions for this clause), the association
between the \post-Syntax of the variables and the \keim-objects
is saved in the clause.  If the \post\ syntax of an object, which needs
the variables of a clause, is read, the variables can be restored to
the environment.
#}

(defun cl=save-context (clause var-keys env)
  (declare (edited  "31-MAR-1993 15:41")
	   (authors RICHTS)
	   (input   "A clause, a list of symbols (POST-keys for variables defined in the clause) and an environment.")
	   (effect  "The association between VAR-KEYS and their variables in ENV is saved in CLAUSE.")
	   (value   "Undefined."))
  (let ((variables (mapcar #'(lambda (key) (term~env-lookup key env))
			   var-keys)))
    (keim~put clause 'cl=context (cons var-keys variables))))

(defun cl=restore-context (clause env)
  (declare (edited  "31-MAR-1993 15:45")
	   (authors RICHTS)
	   (input   "A clause and an environment. The clause contains the association"
		    "between its variables and their POST-syntax.")
	   (effect  "The association between POST-syntax (some keys) and the variables is restored into ENV.")
	   (value   "Undefined."))
  (let* ((context (keim~get clause 'cl=context))
	 (var-keys (car context))
	 (variables (cdr context)))
    (mapc #'(lambda (key variable)
	      (env~enter key variable env))
	  var-keys variables))
  env)

(defun cl=remove-context (clause env)
  (declare (edited  "31-MAR-1993 15:45")
	   (authors RICHTS)
	   (input   "A clause and an environment. The clause contains the association"
		    "between its variables and their POST-syntax.")
	   (effect  "The association between POST-syntax (some keys) and the variables is removed from ENV.")
	   (value   "Undefined."))
  (let* ((context (keim~get clause 'cl=context))
	 (var-keys (car context)))
    (env~remove-plural var-keys env)))

(defmacro cl~with-context (clause env &body body)
  (declare (edited  "18-AUG-1993 15:39")
	   (authors RICHTS)
	   (input   "A clause, an environment and a body.")
	   (effect  "The context of CLAUSE, i.e. the declaration of the variables in it, are added to ENV."
		    "The variables are removed after the body has been executed.")
	   (value   "The value of BODY executed during ENV is changed."))
  `(prog1 (progn (cl=restore-context ,clause ,env)
		 ,@body)
	  (cl=remove-context ,clause ,env)))
