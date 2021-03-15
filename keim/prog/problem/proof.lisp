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

(mod~defmod proof :uses (env keim mod node post prob term )
	    :documentation "Datastructures and basic functionality for generic proofs."
	    :exports (
		      proof+abstract
		      proof~steps
		      proof~set-steps!
		      proof+multi
		      proof~multi-create
		      proof~linearize-proof-tree
		      )
	    )


#{\section{Proof nodes}\label{mod:proof}

All of the intended proof formats have in common, that they can be viewed to consist of steps of a special
class.  These steps can be simple proof steps like resolution steps, natural deduction steps etc. or proofs
themselves.  The steps can be regarded as a conjunction (dividing a proof in subproofs) or a disjunction
(having many proofs for one problem).  Because everys proof belongs to a problem, proofs are a subclasses of
problems so they contain their slots (environment, assumptions etc.).  To have a common superclass for all
proof classes an abstract class {\vb PROOF+ABSTRACT} is defined. It is a subclass of {\vb  PROB+PROBLEM} and defines the
new slot for steps. It is abstract because it has not any instances; all proofs are instances of one of its
subclasses.

Since proofs are subclasses of problem, their \post\-suntax should be an simple extension of the syntax of
problems.

In the second rule \nt{proof-class} is a keyword (like many-proofs, resolution-proof etc.) which determines
what kind of proof follows. \nt{proof-slots>} contains additional information for this kind of proof (like
the cnf for resolution proofs).  Note that this is only the \post\-syntax and not the structure of the object:
There is {\em NOT} a slot in the problem which contains the proof; the result of a \post\-expression of this rule is a
proof which is also a problem.


To have a datastructure which can contain many proofs for one problem the class {\vb PROOF+MULTI} is defined.
In this class the steps slot is filled with objects which are proofs themselves. This class can be useful in a
deduction system application, where e.g. a resolution theorem prover finds a proof that is then transformed to
natural deduction style.#}

(eval-when (load compile eval)
(defclass proof+abstract (prob+problem)
  ((steps :initarg :steps
	  :initform ()
	  :reader proof=steps
	  :writer proof=write-steps!
	  :documentation "Steps in the problem.")
   )
  (:documentation "This is the abstract superclass of all KEIM-proofs. It has not own instances.")))




(defgeneric proof~steps (proof)
  (declare (edited  "24-FEB-1993 17:49")
	   (authors RICHTS nesmith )
	   (input    "A generic proof object.")
	   (effect   "None.")
	   (value    "The list of the steps of PROOF."))
  (:method ((proof proof+abstract))
	   (proof=steps proof)))

(defgeneric proof~set-steps! (proof steps)
  (declare (edited  "24-FEB-1993 17:50")
	   (authors RICHTS nesmith)
	   (input   "A generic proof and a list of steps.")
	   (effect   "The steps of PROOF is set to STEPS.")
	   (value    "Undefined."))
  (:method ((proof proof+abstract) steps)
	   (proof=write-steps! steps proof)))

#{\subsection{ Multi proofs}

The class {\vb proof+multi} is a \keim\ deduction problem that can possibly contain several proofs. This class
is useful for formalizing examples that can have multiple proof or to have proofs in vairous formats for a
single \keim\ deduction problem.#}

(eval-when (load compile eval)
(defclass proof+multi (proof+abstract)
  ()
  (:documentation "This class contains a list of proofs.")))



(defun proof~multi-create (name status env assumptions conclusions subproofs)
  (declare (edited  "14-APR-1993 11:24")
	   (authors RICHTS)
	   (input   "A name, a proof status, an environment, a list of assumptions, a list of one conclusion, a list of proofs.")
	   (effect  "None.")
	   (value   "A multi proof with SUBPROOFS as subproofs in the steps slot."))
  (let ((proof (change-class (prob~create name status env assumptions conclusions) 'proof+multi)))
    (proof~set-steps! proof subproofs)
    proof))

(defmethod post~read-object ((cmd list) (env env+environment) 
			     (indicator (eql :multi-proofs)))
  "cmd should be (<proof>*)"
  (let* ((proofs (mapcar #'(lambda (proof) (prob~read proof env))
			 cmd)))
    (proof~multi-create nil nil nil nil (list (term~env-lookup 'false env)) proofs)))






#{\subsection{Algorithms}
Here we provide a set of algorithms for \keim\ proofs. {\vb PROB~LINEARIZE-PROOF-TREE} is needed for instance
whenever a proof is written to a stream.#}


(term~warning-rename-fn  prob~linearize-proof-tree  proof~linearize-proof-tree)


(defun proof~linearize-proof-tree (root-node)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A proof node.")
	   (effect  "None.")
	   (value   "A linear list of all proof nodes in the DAG obtained through the justification of ROOT-TREE in correct order."
		    "This function performs a depth-up-to-joins search by first counting the occurrences of all nodes in the tree"
		    "and then taking the first one with a 0 and decrementing is successors."))
  (let ((property-key (gentemp "prob=number")))
    (prob=count-nodes root-node property-key)
    (keim~put root-node property-key 0)
    (nreverse (prob=linearize (list root-node) property-key))))

(defun prob=count-nodes (node property-key)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A proof node and a symbol.")
	   (effect  "The property PROPERTY-KEY in all nodes in the tree is incremented by 1 or initialized with 1.")
	   (value   "Undefined."))
  (let ((number (keim~get node property-key)))
    (cond ((null number)
	   (keim~put node property-key 1)
	   (mapc #'(lambda (node) (prob=count-nodes node property-key))
		 (node~just-nodes node)))
	  (t (keim~put node property-key (1+ number))))))

(defun prob=linearize (node-list property-key)
  (declare (edited  " 6-Jan-1993 14:57" )
	   (authors richts)
	   (input   "A list of proof nodes and a symbol.")
	   (effect  "The property PROPERTY-KEY in all nodes is decremented step by step and removed finally.")
	   (value   "A list of nodes."))
  (if (null node-list)
      nil
      (let* ((next-node (find-if #'(lambda (node)
				     (zerop (keim~get node property-key)))
				 node-list))
	     (counter (count next-node node-list))
	     (next-list (append (mapcan #'(lambda (node)
					    (let ((number (- (keim~get node property-key) counter)))
					      (keim~put node property-key number)
					      (if (zerop number) (list node) nil)))
					  (if next-node
					      (node~just-nodes next-node)
					    nil))
				(delete next-node node-list))))
	(keim~remprop next-node property-key)
	(cons next-node (prob=linearize next-list property-key)))))





