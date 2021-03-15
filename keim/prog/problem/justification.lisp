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

(mod~defmod just :uses ( keim mod )
	    :documentation  "Datastructures and basic functionality for generic justifications."
	    :exports (
		      just+justification
		      just~create
		      just~rule
		      just~set-rule!
		      just~nodes
		      just~set-nodes!
		      )
	    )


#{\section{Justifications}\label{mod:just}

All justifications represent a tree (or DAG) structure which encodes the dependency of the current nodes in
the calculus upon each other, as well as the rule with which each line was grounded. At this level of
abstraction the rule can be an arbitrary \lisp\ object where as the dependency is represented by the list of
nodes that the node to be justified depends on. #}

(eval-when (load compile eval)
(defclass just+justification (keim+object)
  ((rule :reader just=rule
	 :writer just=write-rule!
	 :initform "Unspecified"
	 :initarg :rule
	 :documentation "The rule that justifies this proof node.");doc
   (nodes :reader just=nodes
	  :writer just=write-nodes!
	  :initform nil
	  :initarg :nodes
	  :documentation "The proof-nodes from which this line is justified."));doc
  (:default-initargs :rule "Unspecified" :nodes nil)
  (:documentation "The justification for a proof node.")))

(defun just~create (rule nodes)
  (declare (edited  "24-JUN-1992 12:05" )
	   (authors KOHLHASE )
	   (input   "A derivation-rule and a list of proof-nodes.")
	   (effect  "A new justification is created.")
	   (value   "The new justification."))
  (make-instance 'just+justification :rule rule :nodes nodes))

#{\subsection{Accessors}#}

(defgeneric just~rule (justification)
  (declare (edited  " 2-JUN-1992 10:17" )
	   (authors KOHLHASE )
	   (input    "A justification.")
	   (effect   "None.")
	   (value    "The rule used in JUSTIFICATION."))
  (:method ((justification just+justification))
	   (just=rule justification)))

(defgeneric just~set-rule! (justification rule)
  (declare (edited  " 2-JUN-1992 10:17" )
	   (authors KOHLHASE )
	   (input    "A justification and a specifier of a deduction rule.")
	   (effect   "The rule in JUSTIFICAIION set to RULE.")
	   (value    "Undefined."))
  (:method ((justification just+justification) rule)
	   (just=write-rule! rule justification)))

(defgeneric just~nodes (justification)
  (declare (edited  " 2-JUN-1992 10:20" )
	   (authors KOHLHASE )
	   (input    "A justification.")
	   (effect   "None.")
	   (value    "A list of the proof nodes directly supporting the proof node justified by JUSTIFICATION."))
  (:method ((justification just+justification))
	   (just=nodes justification)))

(defgeneric just~set-nodes! (justification nodes)
  (declare (edited  " 2-JUN-1992 10:17" )
	   (authors KOHLHASE )
	   (input    "A justification and a list of proof-nodes.")
	   (effect   "The rule in JUSTIFICAIION set to RULE.")
	   (value    "Undefined."))
  (:method ((justification just+justification) nodes)
	   (just=write-nodes! nodes justification)))


(defmethod print-object ((just just+justification) stream)
	(format stream "#<~A with ~A from ~{~A~}>" (type-of just) (just~rule just) (mapcar #'keim~name (just~nodes just))))

