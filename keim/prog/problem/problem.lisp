;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(mod~defmod prob :uses (assum conc env keim mod node post sym term type )
	    :documentation "Datastructures and basic functionality for deduction problems."
	    :exports (
		      prob+problem
		      prob~create
		      prob~find-problem
		      prob~print
		      prob~read
		      prob~p
		      prob~status
		      prob~set-status!
		      prob~environment
		      prob~set-environment!
		      prob~conclusion
		      prob~set-conclusion!
		      prob~assumptions
		      prob~set-assumptions!
		      )
	    )

#{\section{\keim\ deduction problems}\label{mod:prob}

\keim-deduction problems are the basic datastructures for protocolling deduction systems based on \keim.  A
problem consists of the formulas of the theorem that is to be proved, divided into {\em assumptions} and a
{\em conclusion}.  The key intention of a \keim-deduction problem is to represent the current state the
manipulation of a deduction problem is in.  Therefore it keeps a record of all results obtained so far.  These
results will mainly consist of a list of proofs in various formats.#}


(eval-when (load compile eval)
(defclass prob+problem (keim+name)
  ((status :initarg :status
	   :initform 'initialized
	   :accessor prob=status
	   :documentation "Status of the proof: Initialized, Incomplete, complete, ...")
   (conclusion :initarg :conclusion
	       :accessor prob=conclusion
	       :documentation "What the proof is allegedly proving.");doc
   (assumptions :initarg :assumptions
	 :accessor prob=assumptions
	 :documentation "Allowed assumptions (a list of proof nodes) for this proof.");doc
   (environment :initarg :environment
		:accessor prob=environment
		:documentation "An environment (constants, variables, etc.)")
   )
  (:documentation "A KEIM deduction problem object.")))

;; We'll use this hash table to keep track of all the problems that
;; exist at any given time 
(defvar prob*problem-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by problem name, that holds all existing problems.
This way we can refer to problems by name.")


(defun prob~create (name status env assumptions conclusions)
  (declare (edited  " 3-MAY-1993 10:45" )
	   (authors KOHLHASE )
	   (input   "A name, status, an environment and lists of assumptions and conclusions."
		    "Here ASSUMPTIONS and CONCLUSIONS must be lists of ASSUM+ASSUMPTION and CONC+CONCLUSION"
		    "respectively.")
	   (effect  "None.")
	   (value   "The respective problem."))
  (let ((newproblem 
	 (make-instance 'prob+problem :name name 
			:status status :environment env
			:assumptions assumptions 
			:conclusion
			(if (= (length conclusions) 1)
			    (car conclusions)
			  (post~error "Wrong number of conclusions ~A: must be exactly one."
				      conclusions))
			)))
    newproblem))


(defun prob~find-problem (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a problem (symbol or string).")
           (effect  "None.")
           (value   "The problem with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   prob*problem-hash-table))

;; make sure the new problem is in the hash table
(defmethod initialize-instance :after ((obj prob+problem) &rest initargs)
  (declare (ignore initargs))
  (when (eq (find-class 'prob+problem) (class-of obj))
    (setf (gethash (symbol-name (keim~name obj)) prob*problem-hash-table)
	  obj))
  obj)


#{\subsection{\post\ interface}

\keim\ problems have the following \post\ syntax:

\begin{postsyntax}
\syntax{
\nt{problem} ::= (problem \nt{name} \nt{status} (\nt{decl}*)).
\nt{proof}   ::= (problem \nt{name} \nt{status} (\nt{decl}*) (\nt{proof-class} \nt{proof-slots} \nt{proof-step}*)).
\nt{status}  ::= initialized | proven | conjectured | refuted.}
\syntaxcomment{Where \nt{decl} is declaration of the following form}
\syntax{
\nt{decl}    ::= (type-variables \{\nt{name}\}+).
\nt{decl}    ::= (type-contants \{\nt{name}\}+).
\nt{decl}    ::= (constants \{(\nt{name} \nt{type})\}+).
\nt{decl}    ::= (variables \{(\nt{name} \nt{type})\}+).
\nt{decl}    ::= \nt{assumption} | \nt{conclusion}.}
\end{postsyntax}
#}

(defmethod print-object ((problem prob+problem) stream)
  (format stream "(Problem ~S ~S)" (keim~name problem) (prob~status problem)))


(defgeneric prob~print (object &optional stream)
  (declare (edited  "11-NOV-1992 12:20")
	   (authors RICHTS)
	   (input   "A problem (which can also be a proof) and a stream.")
	   (effect  "This around-method prints a readable form of PROBLEM (the assertions) on STREAM."
		    "Then before closing the final parenthesis it calls the next method, which then prints the proof-slots"
		    "if there are any or does nothing if PROBLEM is a simple problem.")
	   (value   "Undefined."))
  (:method :around ((problem  prob+problem) &optional (stream t))
   (format stream "~%(Problem ~S ~S ~{~%  ~S~}~%  ~S" (keim~name problem) (prob~status problem)
	   (prob~assumptions problem) (prob~conclusion problem))
   (call-next-method)
   (format stream ")"))
  (:method ((problem  prob+problem) &optional (stream t))
   (declare (ignore stream)))
  (:method ((node  node+node) &optional (stream t))
   (format stream "~%~S:~%   ~S" (node~justification node) node)))


(defmethod post~print :around ((problem prob+problem) stream)
  (declare (edited  "27-MAR-1993 13:04")
	   (authors RICHTS)
	   (input   "A problem (which can also be a proof) and a stream.")
	   (effect  "This around-method prints the POST-representation of PROBLEM (the declarations) on STREAM."
		    "Then before closing the final parenthesis it calls the next method, which then prints the proof-slots"
		    "if there are any or does nothing if PROBLEM is a simple problem.")
	   (value   "Undefined."))
  (cond ((null stream) (with-output-to-string (string-stream)
			 (post~print problem string-stream)))
	((eq stream t) (post~print problem *standard-output*))
	(t
	 (format stream "~%(Problem ~S ~S ~%" (keim~name problem)
		 (prob~status problem))
	 (let* ((env (prob~environment problem))
		(type-vars  (env~class-keys env 'type+variable))
		(type-constants (env~class-keys env 'type+constant))
		(constants (env~class-keys env 'sym+const))
		(variables (env~class-keys env 'sym+var)))
	   (princ "(" stream)
	   (mapc #'(lambda (x) (env~post-print x 
					       (env~lookup-object x env)
					       stream))
		 (append type-vars type-constants constants variables))
	   (mapc #'(lambda (x) (post~print x stream) (terpri stream))
		 (append (prob~assumptions problem)
			 (list (prob~conclusion problem))))
	   (princ ")" stream)
	   (call-next-method)		;print the proof-slots if there are any
	   (format stream ")")))))

(defmethod post~print ((problem prob+problem) stream)
  (declare (edited  "27-MAR-1993 13:15")
	   (authors RICHTS)
	   (input   "A problem and a stream.")
	   (effect  "Because the declarations of the problem are allready printed by the"
		    "around-method (see above) this primary-method has nothing left to do.")
	   (value   "Undefined."))
  (declare (ignore stream)))

(defun prob~read (post-expr &optional (env (env~create)))
  (declare (edited  "17-FEB-1993 17:27")
	   (authors RICHTS@JS-SFBSUN)
	   (input  "A Post expression for a problem, and an environment ENV.")
	   (effect "ENV is changed.")
	   (value  "The POST expression will be parsed as a PROBLEM object in the environment ENV, and returned."))
  (post~read-object (cdr post-expr) env :problem))


(defmethod post~read-object ((cmd list) (env env+environment) 
			     (indicator (eql :problem)))
  (let* ((name (post~read-symbol (first cmd) env))
	 (status (post~read-symbol (second cmd) env))
	 (decls (post~read-object-list (third cmd) env))
	 (post-proof (fourth cmd))
	 (prover (fifth cmd))
	 (problem
	  (cond (post-proof 
		 (let ((proof (post~read-object (rest post-proof) env
						(intern (symbol-name
							 (first post-proof))
							(find-package "KEYWORD"))))
		       (conclusions (remove-if-not #'conc~p decls)))
		   (when (> (length conclusions) 1)
		     (error "POST: There is more than one conclusion in the problem declaration ~A." cmd))
		   (keim~set-name! proof name)
		   (prob~set-status! proof status)
		   (prob~set-environment! proof env)
		   (prob~set-assumptions! proof (remove-if-not #'assum~p decls))
		   (if conclusions
		       (prob~set-conclusion! proof (first conclusions))
		       (prob~set-conclusion! proof (conc~create 'conc (term~env-lookup 'true env))))
		   proof))
		(t (prob~create name status env 
				(remove-if-not #'assum~p decls)
				(remove-if-not #'conc~p decls))))))
    (declare (ignore prover))
    (env~enter name problem env)
    problem))


#{\subsection{Auxiliary functions}#}

(defun prob~p (thing)
  (declare
   (authors nesmith)
   (input  "A lisp object THING" )
   (effect "none" )
   (value  "T if THING is a prob+problem, nil otherwise."))
  (typep thing 'prob+problem)
  )

(defgeneric prob~status (proof)
  (declare (edited  " 5-JUN-1992 07:57" )
	   (authors KOHLHASE )
	   (input   "A KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "The status of the proof: A subset of Initialized, Incomplete, Complete,"
		     "RP-refuted, CNF, Transformed, Presented,..."))
  (:method ((problem prob+problem))
	   (prob=status problem)))

(defgeneric prob~set-status! (problem status)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "A KEIM deduction problem and a status.")
	   (effect   "The status of PROBLEM is changed to STATUS.")
	   (value    "Undefined."))
  (:method ((problem prob+problem) status)
	   (setf (prob=status problem) status)))

(defgeneric prob~environment (proof)
  (declare (edited  " 5-JUN-1992 07:57" )
	   (authors KOHLHASE )
	   (input   "An KEIM-deduction problem.")
	   (effect   "None.")
	   (value    "The environment of the proof."))
  (:method ((problem prob+problem))
	   (prob=environment problem)))

(defgeneric prob~set-environment! (problem environment)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "An KEIM deduction problem and an environment.")
	   (effect   "The environment of PROBLEM is changed to ENVIRONMENT.")
	   (value    "Undefined."))
  (:method ((problem prob+problem) environment)
	   (setf (prob=environment problem) environment)))

(defgeneric prob~conclusion (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "An KEIM deduction problem.")
	   (effect   "None.")
	   (value    "The conlusions to be proven in PROBLEM."))
  (:method ((problem prob+problem))
	   (prob=conclusion problem)))


(defgeneric prob~set-conclusion! (problem conclusion)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "An KEIM deduction problem and a formula, that is to be the new conclusion of the problem.")
	   (effect   " If CONCLUSION is a formula of type O, then the conclusion of PROBLEM is changed to CONCLUSION.")
	   (value    "Undefined."))
  (:method ((problem prob+problem) (conclusion conc+conclusion))
	   (if (type~o-p (term~type conclusion))
	       (setf (prob=conclusion problem) conclusion)
	       (error "The term ~A has type ~A and is therefore not a valid conclusion of the problem ~A"
		      conclusion (term~type conclusion) problem)))
  (:method ((problem prob+problem) (conclusion term+term))
      (if (type~o-p (term~type conclusion))
	  (setf (prob=conclusion problem) conclusion)
	(error "The term ~A has type ~A and is therefore not a valid conclusion of the problem ~A"
	       conclusion (term~type conclusion) problem))))

(defgeneric prob~assumptions (problem)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "An KEIM deduction problem.")
	   (effect   "None.")
	   (value    "The assumptions of PROBLEM."))
  (:method ((problem prob+problem))
	   (prob=assumptions problem)))

(defgeneric prob~set-assumptions! (problem assumptions)
  (declare (edited  " 2-JUN-1992 10:53" )
	   (authors KOHLHASE )
	   (input    "An KEIM deduction problem and a list of terms.")
	   (effect   "If ASSUMPTIONS is a list of formulae of type O,"
		     "then the list of assumptions of PROBLEM is changed to ASSUMPTIONS.")
	   (value    "Undefined."))
  (:method ((problem prob+problem) assumptions)
	   (setf (prob=assumptions problem) assumptions)))


