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

(mod~defmod node :uses (just keim mod term type )
	    :documentation "Datastructures and basic functionality for generic proof nodes."
	    :exports (
		      node+node
		      node~create
		      node~formula
		      node~set-formula!
		      node~justification
		      node~set-justification!
		      node~just-nodes
		      node~make-new-name
		      node~post-print-step
		      node~post-print-steps
		      )
	    )

#{\section{Proof Nodes}\label{mod:node}

Proof nodes are abstract objects which contain a formula, a name, and a justification. They could for instance
be used as lines in a natural deduction proof or as clauses in a resolution proof.#}

(eval-when (load compile eval)
(defclass node+node (keim+name)
  ((formula :reader node=formula
	    :writer node=write-formula!
	    :initarg :formula
	    :documentation "Formula asserted by a proof node.")
   (justification :reader node=justification
		  :writer node=write-justification!
		  :initarg :justification
		  :type just+justification
		  :documentation "The justification for a proof node (rule name, ~
                                  nodes used, any required terms."))
  (:documentation "A node in a generic proof.")))

(defun node~create (label wff just)
  (declare
   (authors nesmith)
   (input "A label, a formula and a justification")
   (value "An instance of class NODE+NODE with corresponding slot values"))
  (make-instance 'node+node :label label :conclusion wff 
		 :justification just))

#{\subsection{Accessors}#}

(defgeneric node~formula (node)
  (declare (edited  " 2-JUN-1992 10:22" )
	   (authors KOHLHASE )
	   (input    "A proof node.")
	   (effect   "None.")
	   (value    "The formula of NODE."))
  (:method ((node node+node))
	   (node=formula node)))

(defgeneric node~set-formula! (node formula)
  (declare (edited  " 2-JUN-1992 10:22" )
	   (authors KOHLHASE )
	   (input    "A proof node and a term.")
	   (effect   "If FORMULA is of type O, then the formula in NODE is set to FORMULA, else error.")
	   (value    "Undefined."))
  (:method ((node node+node) (formula term+term))
	   (if t ;(keim~equal (term~type formula) (type~o)) ;; Fix later! DAN
	       (node=write-formula! formula node)
	       (error "~A is not a formual of type O" formula))))

(defgeneric node~justification (node)
  (declare (edited  " 2-JUN-1992 10:24" )
	   (authors KOHLHASE )
	   (input    "A proof node.")
	   (effect   "None.")
	   (value    "The justification of NODE."))
  (:method ((node node+node))
	   (node=justification node)))

(defgeneric node~set-justification! (node justification)
  (declare (edited  " 2-JUN-1992 10:22" )
	   (authors KOHLHASE )
	   (input    "A proof node and a justification.")
	   (effect   "The justification in NODE is set to JUSTIFICATION.")
	   (value    "Undefined."))
  (:method ((node node+node) (just just+justification))
	   (node=write-justification! just node)))


(defgeneric node~just-nodes (node)
  (declare (edited  "06-Jan-1992 10:24" )
	   (authors richts)
	   (input    "A proof node.")
	   (effect   "None.")
	   (value    "The supporting nodes of the justification of NODE."))
  (:method ((node node+node))
   (just~nodes (node~justification node))))

#{For deduction system applications it is a recurring situation that the system needs a new (that does not
occur anywhere else) name for a proof node. The function {\vb node~make-new-name} dreams one up according
to the status of the system.#}

(defun node~make-new-name ()
  (declare
   (authors nesmith)
   (input "none")
   (value "A new symbol for use as a name"))
  (gensym "NODE")
  ;; fill in with some complicated way of naming nodes, including a way
  ;; to reset a counter when beginning a new proof, making sure node
  ;; names are distinct, etc.
  )

#{\subsection{POST interface for resolution nodes} 

The following functions form a peculiarity of \keim\ from the history of implementation, they may vanish in
future versions. In the resolution system the nodes for resolution proofs and the clauses are equated
therefore a clause has 2 \post\ representations, one as a clause, and anther as a proof step. The former can
be obtained with the function {\vb POST~PRINT}, the latter is supplied by the following functions. #}

(defgeneric node~post-print-step (justification node stream)
  (declare (edited  "06-FEB-1993 12:45")
	   (authors RICHTS)
	   (input   "A justification, the proof node it is leading to and a stream.")
	   (effect  "The POST representation of the step leading to NODE"
                     "(consisting of JUSTIFICATION and NODEs formula)"
		    "is printed on STREAM"
                    "(e.g. (resolution step1 <resolvent> (<cl-name1> <pos1>) (<cl-name2> <pos2>)"
                                       "<subst> <renaming>)).")
	   (value   "Undefined.")))

(defun node~post-print-steps (nodes stream)
  (declare (edited  "06-FEB-1993 14:34")
	   (authors RICHTS)
	   (input "a list of nodes and a stream"  )
	   (effect "the nodes are printed  in POST representation to the stream"))
  (when nodes
     (node~post-print-step (node~justification (first nodes)) (first nodes) stream)
     (mapc #'(lambda (node)
	       (format stream " ")
	       (node~post-print-step (node~justification node) node stream))
	   (rest nodes))))



(defmethod print-object ((node node+node) stream)
  (let ((just (node~justification node)))
    (format stream "#<~A : ~A with ~A from ~{~A~}>"
	    (type-of node)
	    (node~formula node)
	    (just~rule just)
	    (keim~name (just~nodes just)))))

