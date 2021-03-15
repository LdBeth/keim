;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(mod~defmod res :uses (cl delta env just keim mod node pos post prob proof subst)
	    :documentation "The datastructure and I/O for resolution proof-results."
	    :exports (
		      res+proof
		      res~proof-p
		      res~proof-create
		      res~find-proof
		      res~proof-clauses
		      res~set-proof-clauses!
		      res~proof-delta-relation
		      res~set-proof-delta-relation!
		      res~proof-empty-clause
		      res~set-proof-empty-clause!
		      res+justification
		      res~step-p
		      res~just-unifier
		      res~just-renaming
		      res~read-step
		      res+initial
		      res~initial-create
		      res~initial-p
		      res+resolution
		      res~resolution-create
		      res~resolution-p
		      res~resolution-clauses
		      res~resolution-positions
		      res+factoring
		      res~factoring-create
		      res~factoring-p
		      res~factoring-clause
		      res~factoring-positions
		      res+paramodulation
		      res~paramod-create
		      res~paramodulation-p
		      res~paramod-mother
		      res~paramod-mother-position
		      res~paramod-father
		      res~paramod-father-position
		      res~paramod-direction
		      res+instance
		      res~instance-create
		      res~instance-p
		      res~instance-clause
		      res~step-p
		      res~just-unifier
		      res~just-renaming
		      res~read-step
		      )
	    )


#{
\section{Resolution}
\label{mod:res}

Resolution is a well-known concept, and this module defines resolution
proofs and steps in a straightforward manner. The definitions are based on
the proof module and the justification
module (sections \ref{mod:proof} and \ref{mod:just} in chapter \ref{sys:problem}).


Here is the POST input form for
resolution proofs:
\begin{postsyntax}
\syntax{

\nt{proof-step}        ::=  \nt{resolution-step} | \nt{factoring-step} | \nt{paramodulation-step}
                               | \nt{instance-step}.

%\bigskip

\nt{resolution-step}   ::=   (resolution \nt{name} \nt{resolvent}
                                \nt{parent} \nt{parent} \nt{unifier} \nt{renaming}).

%\medskip

\nt{resolvent}         ::=   \nt{clause}.
\nt{resolution-parent} ::=   (\nt{clause-name} \nt{position}).
%\nt{parent-2}         ::=   ( \nt{clause-name} \nt{position} ).
\nt{unifier}           ::=   \nt{substitution}.
\nt{renaming}          ::=   \nt{substitution}.

%\bigskip

\nt{factoring-step}    ::=   (factoring \nt{name} \nt{resolvent} \nt{parent} \nt{unifier} \nt{renaming}).
\nt{factoring-parent}  ::=   ( \nt{clause-name} \nt{position} \nt{position} ).

%\bigskip

\nt{instance-step}     ::=   (instance \nt{name} \nt{resolvent} \nt{parent} \nt{substitution}).
\nt{instance-parent}   ::=   (\nt{clause-name}).

%\bigskip

\nt{paramodulation-step}   ::=    (paramodulation \nt{name} \nt{resolvent}
                                     \nt{parent} \nt{equation} \nt{unifier} \nt{renaming} ).

%\medskip

\nt{parent}     ::=   (\nt{clause-name} \nt{position}).
\nt{equation}   ::=   (\nt{clause-name} \nt{position} \nt{direction}).
\nt{direction}  ::=   LR | RL.

}

\end{postsyntax}
#}

#{\subsection{Resolution proofs}

Resolution proofs are a subclass of {\vb proof+abstract}. From this
class and from its superclass {\vb prob+problem} they inherit some
slots and some generic functions (see section \ref{mod:prob} and \ref{mod:proof}).
A resolution proof object needs not to be a complete proofs. Also the
startup for a resolution proof and the proving itself can be managed with this class.

The class {\vb res+proof} defines the following slots: a list of the
initial clauses, the empty clause, and the delta relation.
The initial clauses are the conjunctive normal form of the assumptions
and the conclusion slot (inherited from {\vb prob+problem}). To store
the origins of the literals in these clauses the delta relation slot
can contain a mapping between literals and positions in formulae. 
Normalization is provided by the cnf module (section \ref{mod:cnf})
and delta relations are defined in the delta module (section \ref{mod:delta}).

Since the empty clause in a resolution proof is the root of the proof
tree, the resolution proof class provides a slot for it.
The steps of a resolution proof are stored in the steps slot which is inherited
from {\vb proof+abstract}.

#}


(eval-when (load compile eval)
(defclass res+proof (proof+abstract)
  ((clauses :initarg :clauses :reader res=proof-clauses :writer res=proof-write-clauses!
	    :documentation "The initial set of clauses.")
   (delta-relation :initarg :delta-relation :reader res=proof-delta-relation :writer res=proof-write-delta-relation!
		   :documentation "The delta-relation.")
;   (skolem-relation :initarg :skolem-relation :reader res=proof-skolem-relation :writer res=proof-write-skolem-relation!
;		    :documentation "A list of named formulas.")
   (empty-clause :initarg :empty-clause :reader res=proof-empty-clause :writer res=proof-write-empty-clause!
		 :initform NIL
		 :documentation "The empty clause which is a member in steps."))
  (:documentation "The datastructure for resolution proofs.")))

(defun res~proof-p (thing)
  (declare
   (authors nesmith)
   (input   "Any lisp object")
   (effect  "None.")
   (value   "T if THING is a resolution proof, otherwise nil."))
  (typep thing 'res+proof))


(defmethod print-object ((resolution res+proof) stream)
    (format stream "(Resolution-proof ~A ~A)"
	     (keim~name resolution) (prob~status resolution)))


(defun res~proof-create (clauses delta-relation steps empty-clause env)
  (declare (edited  "03-NOV-1992 15:17")
	   (authors RICHTS)
	   (input   "A list of clauses, a delta-relation object, a list of steps, a (possibly nil) empty clause,"
		    "and the environment in which these things were defined.")
	   (effect  "None.")
	   (value   "A resolution proof-result."))
  (make-instance 'res+proof 
		 :clauses clauses :delta-relation delta-relation    ; :skolem-relation skolem-relation 
		 :steps steps :empty-clause empty-clause
		 :environment env))

(defvar res*proof-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by proof name, that holds all existing resolution proofs.
This way we can refer to proofs by name.")

(defun res~find-proof (name)
  (declare
   (authors NESMITH)
   (input   "A name of a resolution proof (symbol or string).")
   (effect  "None.")
   (value   "The resolution proof with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   res*proof-hash-table))

;; make sure the new proof is in the hash table
(defmethod initialize-instance :after ((obj res+proof) &rest initargs)
  (declare (ignore initargs))
  (when (eq (find-class 'res+proof) (class-of obj))
    (setf (gethash (symbol-name (keim~name obj)) res*proof-hash-table)
	  obj))
  obj)


(defgeneric res~proof-clauses (proof)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of initial clauses of PROOF."))
  (:method ((proof res+proof))
   (res=proof-clauses proof)))

(defgeneric res~set-proof-clauses! (proof clauses)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof and a list of clauses.")
	   (effect  "The initial clauses of PROOF are set to CLAUSES")
	   (value   "The changed PROOF."))
  (:method ((proof res+proof) clauses)
   (res=proof-write-clauses! clauses proof)))


(defgeneric res~proof-delta-relation (proof)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The delta relation of PROOF."))
  (:method ((proof res+proof))
   (res=proof-delta-relation proof)))

(defgeneric res~set-proof-delta-relation! (proof delta-relation)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof and a delta-relation.")
	   (effect  "The delta relation of PROOF is set to DELTA-RELATION")
	   (value   "The changed PROOF."))
  (:method ((proof res+proof) delta-relation)
   (res=proof-write-delta-relation! delta-relation proof)))

(defgeneric res~proof-empty-clause (proof)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The empty clause of PROOF or nil."))
  (:method ((proof res+proof))
   (res=proof-empty-clause proof)))

(defgeneric res~set-proof-empty-clause! (proof empty-clause)
  (declare (edited  "01-FEB-1993 17:00")
	   (authors RICHTS)
	   (input   "A resolution proof and an empty clause.")
	   (effect  "The empty clause of PROOF is set to EMPTY-CLAUSE")
	   (value   "The changed PROOF."))
  (:method ((proof res+proof) empty-clause)
   (res=proof-write-empty-clause! empty-clause proof)))


#{\subsection{\post\ syntax for resolution proofs}

Since resolution proofs are a subclass of {\vb prob+problem} in this
module we only define the syntax for {\tt \nt{proof-result}} which
contains rules for the new slots and for the steps.  Resolution proofs
can be read in with {\vb prob~read} if the problem declaration is added.
There is an example at the end of this section.

\begin{postsyntax}
\syntax{
\nt{proof-result}    ::=   (resolution-proof \nt{cnf} \nt{proof-step}*).
\nt{cnf}             ::=   (cnf (\nt{clause}*) \nt{delta-relation}).}
\end{postsyntax}

If the \post\ syntax of a resolution proof is read in via {\vb
prob~read}, a resolution proof is created and the clauses slot and the
delta relation slot is set to the objects read from the {\tt \nt{cnf}} form.
The steps slot is set to the {\tt \nt{proof-steps}}. If an empty clause
is found amoung the steps it is stored in the empty clause slot.
#}


#|(defmethod post~print ((proof res+proof) stream)
  (format stream "(resolution-proof ")
  (post~print (prob~environment proof) stream)
  (format stream "(cnf (")
  (post~print (res~proof-clauses proof) stream)
  (format stream ") ")
  (post~print (res~proof-delta-relation proof) stream)
  (format stream ")")
  (let ((steps (remove-if #'res~initial-p
			  (if (res~proof-empty-clause proof)
			      (proof~linearize-proof-tree (res~proof-empty-clause proof))
			      (proof~steps proof)))))
    (dolist (step steps)
      (node~post-print-steps step stream)
      ))
  (format stream ")"))|#


(defmethod post~print ((proof res+proof) stream)
  (declare (edited  "27-MAR-1993 13:15")
	   (authors RICHTS)
	   (input   "A resolution proof and a stream.")
	   (effect  "Because the declarations of the problem are already printed by the"
		    "around-method for prob+problem this primary-method only has print the new slots.")
	   (value   "Undefined."))
  (format stream "(resolution-proof ")
  (format stream "(cnf (")
  (post~print (res~proof-clauses proof) stream)
  (format stream ") ")
  (post~print (res~proof-delta-relation proof) stream)
  (format stream ")")
  (node~post-print-steps (proof~steps proof) stream)
  (format stream ")"))

;;  version with keyword
(defmethod post~read-object ((cmd list) (env env+environment) 
			     (indicator (eql :resolution-proof)))
  "cmd should be ((cnf (\nt{clause}*) \nt{delta-relation}) \nt{step}*)"
  (let* ((cnf-result (post~read-object (rest (first cmd)) env :cnf))
	 (clauses (first cnf-result))
	 (delta (second cnf-result))
	 (steps (mapcar #'(lambda (step) (res~read-step step env))
			(rest cmd)))
	 (empty-clause (find-if #'cl~empty-p steps)))
    (dolist (initial clauses)
      (node~set-justification! initial (res~initial-create 
					 (keim~name initial))))
    (res~proof-create clauses delta steps empty-clause env)))


(defmethod post~read-object (cmd (env env+environment) 
				 (indicator (eql :cnf)))
  (declare (edited  "24-MAR-1993 13:41")
	   (authors RICHTS)
	   (input   "cmd should be (\nt{clauses} \nt{delta-relation})")
	   (effect  "The clauses are entered into the environment")
	   (value   "A list of two values, first being the clauses, the second being the delta-relation."))  
  (unless (and (listp cmd)
	       (= 2 (length cmd))
	       (listp (first cmd)))
    (post~error "~A does not match specification for a ~A declaration"
		cmd indicator))
  (let ((clauses (mapcar #'(lambda (clause) (cl~read clause env))
			 (first cmd)))
	(delta-relation (delta~read (second cmd) env)))
					;(dolist (clause clauses) (env~enter (keim~name clause) clause env))   ;Klauseln tragen sich selbst ins Env. ein. joern
    (list clauses delta-relation)))



#{\subsection{Steps for resolution proofs}

A step in a resolution proof consists of a clause and a
justification for this clause. The clause is the result of the step and the
justification contains all information how the clause was
constructed. To avoid a new datastructure which
would make the handling of resolution proofs more difficult, a
step of a resolution proof in \keim\ is simply a clause with a
justification in its justification slot, i.e. a clause together with
its justification forms a step. All generic function defined for
justifications are also defined for instances of {\vb node+node}
(including clauses) so that clauses really can be viewed as steps.

This module provides datastructures and algorithms for the following
steps used in a resolution proof: resolution, factoring,
paramodulation, instantiation and giving an initial clause. For this purpose some
justification classes are defined.

\subsubsection{Abstract steps in resolution proofs}

To make the handling of steps of different kinds easier, they are as
uniformly represented as much as possible. Therefore an abstract
superclass {\vb res+justification} is defined which provides slots for
almost all information needed by the subclasses. This class is a
subclass of {\vb just+justification} from which it inherits the nodes
slot. This slot is used to store all the parent clauses, i.e. the set of
clauses from which the new clause was inferred.

The new slots in {\vb res+justification} are:
\begin{description}
\item[positions] determine the literal or term in a parent clause
which was used in the step. To which clause a position belongs is
defined in the subclasses of {\vb res+justification}.
\item[unifier] is a substitution which unifies two terms or literals
of the parent clauses.
\item[renaming] is a substitution which renames the variables of the
parent clauses so that the infered clause has new variables.
\end{description}

{\vb res+justification} is an abstract class, i.e. it has no
instances. Instances can be created for one of its subclasses. These
subclasses define the meaning of the slots of {\vb res+justification}.

For slots with a general meaning there are functions for selecting and
setting these slots defined on {\vb res+justification}: for the parent
clauses {\vb just~nodes} and {\vb just~set-nodes!} are inherited from {\vb
just+justification} and {\vb node+node}; {\vb res~just-unifier} and
{\vb res~just-renaming} are defined.

There is also a common structure for the \post\ syntax of resolution steps:

{\vb \nt{proof-step}  ::= ({\rm step name} \nt{name} \nt{resolvent}
				{\rm ... parent information ...} \nt{unifier} \nt{renaming}).}

The resolvent is the resulting clause of the step, the unifier is a
substitution which unifies two terms in the parent clauses and the renaming
is a substitution which renames all variables of the parent clauses so
that the resulting clause has new variables.
\begin{postsyntax}
\syntax{
\nt{resolvent}         ::=   \nt{clause}.
\nt{unifier}           ::=   \nt{substitution}.
\nt{renaming}          ::=   \nt{substitution}.
}
\end{postsyntax}
In the parent information the parent clauses are referenced by their name:
\begin{postsyntax}
\syntax{
\nt{parent}           ::=    \nt{clause-name}.
}
\end{postsyntax}
#}


#|
\nt{step}  ::=  (resolution \nt{name} \nt{resolvent} (\nt{parent} \nt{literal-position})
                                            (\nt{parent} \nt{literal-position})
                                            \nt{unifier} \nt{renaming})
\nt{step}  ::=  (factoring  \nt{name} \nt{resolvent} (\nt{parent} \nt{literal-position} \nt{literal-position})
                                            \nt{unifier} \nt{renaming})
\nt{step}  ::=  (instance  \nt{name} \nt{resolvent} (\nt{parent}) \nt{unifier})
\nt{step}  ::=  (paramodulation \nt{name} \nt{resolvent}
                             (\nt{parent} \nt{literal-position})
                             (\nt{parent} \nt{literal-position} \nt{direction})
                             \nt{unifier} \nt{renaming})
|#

(eval-when (load compile eval)
(defclass res+justification (just+justification keim+name)
    ((nodes :initarg :parents :reader res=justification-parents :writer res=justification-write-parents!
	    :initform nil
	    :documentation "The two parent clauses that were resolved.")
     (positions :initarg :positions :reader res=justification-positions :writer res=justification-write-positions!
		:initform nil
		:documentation "The two literal positions in the parent clauses that were resolved.")
     (unifier :initarg :unifier :reader res=justification-unifier :writer res=justification-write-unifier!
	      :initform nil
	      :documentation "The unifier.")
     (renaming :initarg :renaming :reader res=justification-renaming :writer res=justification-write-renaming!
	      :initform nil
	      :documentation "The renaming."))
  (:documentation "This is the superclass to all justifications used in resolution proofs.")))


(defgeneric res~step-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for resolution proofs or a node with such a justification."))
  (:method ((node node+node))
   (res~step-p (node~justification node)))
  (:method ((res-just res+justification))
   t)
  (:method ((object t))
   nil))

(defgeneric res~just-unifier (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The unifier of this step (a substitution)"))
  (:method ((node node+node))
   (res~just-unifier (node~justification node)))
  (:method ((res-just res+justification))
   (res=justification-unifier res-just)))

(defgeneric res~just-renaming (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The renaming of this step (a substitution)"))
  (:method ((node node+node))
   (res~just-renaming (node~justification node)))
  (:method ((res-just res+justification))
   (res=justification-renaming res-just)))

(defmethod node~post-print-step ((just res+justification) node stream)
  (format stream "(~A " (keim~name just))
  (post~print node stream)
  (format stream " () () ")
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renaming just) stream)
  (format stream ")"))

(defun res~read-step (step env)
  (declare
   (input "a POST representation of a resolution step and an environment")
   (effect "The resolution step is parsed in using the environment"))
  (unless (and (listp step)
	       (symbolp (car step)))
    (post~error "~A does not match specification for a RESOLUTION STEP."
		step))
  (post~read-object (cdr step) env 
		    (intern (string (car step)) (find-package :keyword))))




#{\subsubsection{Initial clauses}

For the initial clause, the clauses that are returned by the
normalization, here we define a justification. All slots are {\vb
NIL}, because these clauses are the leave nodes of a proof graph.
To get the origins of the literals in a clause one can use the delta
relation.

This justification has no \post\ syntax since it is created for the
clauses in the cnf form of a \post\ resolution proof.

#}

(eval-when (load compile eval)
(defclass res+initial (res+justification)
    ()
  (:documentation 
  "Justification for initial clauses."))) 


(defun res~initial-create (name)
  (declare (authors nesmith)
	   (input "A name")
	   (value "A new initial justification."))
  (make-instance 'res+initial :name name :rule 'initial))


(defgeneric res~initial-p (obj)
  (:method ((node t))
      nil)
  (:method ((node node+node))
      (res~initial-p (node~justification node)))
  (:method ((res-initial res+initial))
      t))

#{\subsubsection{Resolution}

This justification is the representation of binary resolution on two literals.
The nodes slot contains the two parent clauses and the
positions slot contains exactly two positions. Each position identifies
a literal in the corresponding clause in the nodes slot. The literals
have different polarity and the unifier slot contains a substitution which
unifies the atoms of these literals.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (resolution \nt{name} \nt{resolvent} (\nt{parent} \nt{literal-position})
                                            (\nt{parent} \nt{literal-position})
                                            \nt{unifier} \nt{renaming}).
}
\end{postsyntax}
Note that the \post\ representation differs from the representation of
the \keim\ objects: In \keim\ a clause with a justification in its
justification slot is a step and there is no step object which
contains a clause. Furthermore the representation of the parent
information is different. A \keim\ jsutification has a list of parent
clauses and a list of positions and not two lists with a clause and
position. Also the \keim\ justification contains the clause object and
not just its name.
#}


(eval-when (load compile eval)
(defclass res+resolution (res+justification)
    ()
  (:documentation 
  "Justification for resolution steps: UNIFIER is a unifier for the literals at POSITIONS in the clauses in NODES.")))


(defun res~resolution-create (parents positions unifier renaming name)
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A list of two clauses, a list of two positions of two literals in the clauses"
		    "and a unifier for these literals containing a renaming for all variables in the two clauses.")
	   (effect  "None.")
	   (value   "A justification object for the resolution of the two literals."))
  (make-instance 'res+resolution :parents parents :positions positions :unifier unifier :renaming renaming :name name :rule 'resolution))


(defgeneric res~resolution-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a resolution step or a node with such a justification."))
  (:method ((node node+node))
   (res~resolution-p (node~justification node)))
  (:method ((res-just res+resolution))
   t)
  (:method ((object t))
   nil))


(defgeneric res~resolution-clauses (resolution-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a resolution step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parents of this step (a list of clauses)"))
  (:method ((node node+node))
   (res~resolution-clauses (node~justification node)))
  (:method ((res-just res+resolution))
   (res=justification-parents res-just)))

(defgeneric res~resolution-positions (resolution-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a resolution step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of this step (two positions identifying
literals in clauses)."))
  (:method ((node node+node))
   (res~resolution-positions (node~justification node)))
  (:method ((res-just res+resolution))
   (res=justification-positions res-just)))

(defmethod print-object ((resolution-just res+resolution) stream)
  (let ((parents (res~resolution-clauses resolution-just))
	(positions (res~resolution-positions resolution-just))
	(unifier (res~just-unifier resolution-just))
	(renaming (res~just-renaming resolution-just)))
    (format stream "Resolution of (~S ~S) and (~S ~S) with unifier ~S and renaming ~S:"
	    (keim~name (first parents)) (first positions)
	    (keim~name (second parents)) (second positions)
	    unifier renaming)))

(defmethod node~post-print-step ((just res+resolution) node stream)
  (format stream "(resolution ~A " (keim~name just))
  (post~print node stream)
  (mapc #'(lambda (clause pos)
	    (format stream "(~A " (keim~name clause))
	    (post~print pos stream)
	    (format stream ") "))
	(res~resolution-clauses just)
	(res~resolution-positions just))
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renaming just) stream)
  (format stream ")"))


(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :resolution)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (parent1 (cl~env-lookup (car (third step)) env))
	 (pos1 (pos~read (cadr (third step)) env))
	 (parent2 (cl~env-lookup (first (fourth step)) env))
	 (pos2 (pos~read (cadr (fourth step)) env))
	 (unifier (cl~with-context resolvent env
				   (cl~with-context parent1 env
						    (cl~with-context parent2 env
								     (subst~read (fifth step) env)))))
	 (renaming (cl~with-context resolvent env
				    (cl~with-context parent1 env
						     (cl~with-context parent2 env
								      (subst~read (sixth step) env))))))
    (node~set-justification! 
     resolvent 
     (res~resolution-create (list parent1 parent2)
			    (list pos1 pos2) unifier renaming name))
    resolvent))

#{\subsubsection{Factoring}

This justification represents the factoring of two literals of a clause.
The nodes slot contains a list of the parent clause and the
positions slot contains exactly two positions. Each position identifies
a literal in the clause in the nodes slot. The literals
have the same polarity and the unifier slot contains a substitution which
unifies the atoms of these literals.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (factoring  \nt{name} \nt{resolvent}
				  (\nt{parent} \nt{literal-position} \nt{literal-position})
				  \nt{unifier} \nt{renaming}).
}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.

#}

(eval-when (load compile eval)
(defclass res+factoring (res+justification)
    ()
  (:documentation 
  "Justification for factoring steps: UNIFIER is a unifier for the two literals at POSITIONS in the clause in NODES.")))

(defun res~factoring-create (parent positions unifier renaming name)
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause, a list of two positions of two literals in the clause"
		    "and a unifier for this literals containing a renaming for all variables in the two clauses.")
	   (effect  "None.")
	   (value   "A justification object for the factoring of the two literals."))
  (make-instance 'res+factoring :parents (list parent) :positions positions :unifier unifier :renaming renaming :name name :rule 'factoring))



(defgeneric res~factoring-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a factoring step or a node with such a justification."))
  (:method ((node node+node))
   (res~factoring-p (node~justification node)))
  (:method ((fact-just res+factoring))
   t)
  (:method ((object t))
   nil))


(defgeneric res~factoring-clause (factoring-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a factoring step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parent of this step (a clause)"))
  (:method ((node node+node))
   (res~factoring-clause (node~justification node)))
  (:method ((res-just res+factoring))
   (first (res=justification-parents res-just))))


(defgeneric res~factoring-positions (factoring-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a factoring step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of this step"))
  (:method ((node node+node))
   (res~factoring-positions (node~justification node)))
  (:method ((res-just res+factoring))
   (res=justification-positions res-just)))




(defmethod print-object ((factoring-just res+factoring) stream)
  (let ((parent (res~factoring-clause factoring-just))
	(positions (res~factoring-positions factoring-just))
	(unifier (res~just-unifier factoring-just))
	(renaming (res~just-renaming factoring-just)))
    (format stream "Factoring of ~S at ~S and ~S with unifier ~S: and renaming ~S"
	    (keim~name parent) (first positions) (second positions)
	    unifier renaming)))



(defmethod node~post-print-step ((just res+factoring) node stream)
  (format stream "(factoring ~A " (keim~name just))
  (post~print node stream)
  (format stream "(~A " (keim~name (res~factoring-clause just)))
  (mapcar #'(lambda (x) (post~print x stream))
	  (res~factoring-positions just))
  (format stream ") ")
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renaming just) stream)
  (format stream ")"))


(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :factoring)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (parent (cl~read (first (third step)) env))
	 (pos1 (pos~read (second (third step)) env))
	 (pos2 (pos~read (third (third step)) env))
	 (unifier (cl~with-context resolvent env
				   (cl~with-context parent env
						    (subst~read (fifth step) env))))
	 (renaming (cl~with-context resolvent env
				    (cl~with-context parent env
						     (subst~read (sixth step) env)))))
    (node~set-justification! 
     resolvent 
     (res~factoring-create parent (list pos1 pos2) unifier renaming name))
    resolvent))


#{\subsubsection{Paramodulation}

This justification represents the paramodulation of a clause with a
clause containing an equation.
The nodes slot contains a the two parent clauses and the
positions slot contains exactly two positions. The first position identifies
a term in the first clause in the nodes slot. The second position
identifies a literal in the second clause. This literal
has positive polarity and an atom which is an equation (that is an
term with the predefined predicate {\vb =} as function symbol). The class {\vb
res+paramodulation} provides an additional slot containing the
direction in which the equation is to the term in the first clause.
The possible values are {\vb LR} for left-to-right and {\vb RL} for right-to-left.
If the direction is {\vb LR}, the substitution in the unifier slot
unifies the term in the first clause and the first argument of the
equational atom of the literal in the second clause; if the direction
is {\vb RL} it unifies the term with the second argument.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (paramodulation \nt{name} \nt{resolvent}
                             (\nt{parent} \nt{literal-position})
                             (\nt{parent} \nt{literal-position} \nt{direction})
                             \nt{unifier} \nt{renaming}).
\nt{direction}  ::=   LR | RL.}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.
#}

(eval-when (load compile eval)
(defclass res+paramodulation (res+justification)
    ((direction :initarg :direction :reader res=paramodulation-direction :writer res=paramodulation-write-direction!
	        :documentation "The direction in which the second literal is applied: 'LR or 'RL."))
  (:documentation 
  "Justification for paramodulation steps: UNIFIER is a unifier for the position (first POSITIONS) in (first NODES) ~
   and one side of the equality literal at position (second POSITIONS) in the clause (second NODES).")))



(defun res~paramod-create (mother mother-position father father-position direction unifier renaming name)
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause and a position of a term in a literal in this clause,"
		    "another clause, a position of an equality literal in this clause and the symbol 'LR or RL,"
		    "a unifier for the term in the first clause and one side of the equation in the second clause"
		    "containing a renaming for all variables in the two clauses.")
	   (effect  "None.")
	   (value   "A justification object for the paramodulation of the 
first clause with the equality literals in the second clause."))
  (make-instance 'res+paramodulation 
		 :parents (list mother father)
		 :positions (list mother-position father-position)
		 :direction direction
		 :unifier unifier
		 :renaming renaming
		 :name name
		 :rule 'paramod))

(defgeneric res~paramodulation-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a paramodulation step or a node with such a justification."))
  (:method ((node node+node))
   (res~paramodulation-p (node~justification node)))
  (:method ((paramod-just res+paramodulation))
   t)
  (:method ((object t))
   nil))


(defgeneric res~paramod-mother (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The clause in which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-mother (node~justification node)))
  (:method ((res-just res+paramodulation))
   (first (res=justification-parents res-just))))

(defgeneric res~paramod-mother-position (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The position at which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-mother-position (node~justification node)))
  (:method ((res-just res+paramodulation))
   (first (res=justification-positions res-just))))

(defgeneric res~paramod-father (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The clause containing the equality literal with which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-father (node~justification node)))
  (:method ((res-just res+paramodulation))
   (second (res=justification-parents res-just))))

(defgeneric res~paramod-father-position (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The positions of the equality literal with which the replacement was done."))
  (:method ((node node+node))
   (res~paramod-father-position (node~justification node)))
  (:method ((res-just res+paramodulation))
   (second (res=justification-positions res-just))))

(defgeneric res~paramod-direction (paramod-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a paramodulation step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The direction in which the equality literal is applied: 'LR or 'RL."))
  (:method ((node node+node))
   (res~paramod-direction (node~justification node)))
  (:method ((res-just res+paramodulation))
   (res=paramodulation-direction res-just)))

(defmethod print-object ((paramod-just res+paramodulation) stream)
  (let ((mother (res~paramod-mother paramod-just))
	(mother-position (res~paramod-mother-position paramod-just))
	(father (res~paramod-father paramod-just))
	(father-position (res~paramod-father-position paramod-just))
	(direction (res~paramod-direction paramod-just))
	(unifier (res~just-unifier paramod-just))
	(renaming (res~just-renaming paramod-just)))
    (format stream "Paramodulation of (~S ~S) and (~S ~S ~S) with unifier ~S: and renaming ~S"
	    (keim~name mother) mother-position
	    (keim~name father) father-position direction
	    unifier renaming)))

(defmethod node~post-print-step ((just res+paramodulation) node stream)
  (format stream "(paramodulation ~A " (keim~name just))
  (post~print node stream)
  (format stream "(~A " (keim~name (res~paramod-mother just)))
  (post~print (res~paramod-mother-position just) stream)
  (format stream ") ")
  (format stream "(~A " (keim~name (res~paramod-father just)))
  (post~print (res~paramod-father-position just) stream)
  (post~print (res~paramod-direction just) stream)
  (format stream ") ")
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renaming just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :paramodulation)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (parent1 (cl~read (first (third step)) env))
	 (pos1 (pos~read (second (third step)) env))
	 (parent2 (cl~read (first (fourth step)) env))
	 (pos2 (pos~read (second (fourth step)) env))
	 (direction (third (fourth step)))
	 (unifier (cl~with-context resolvent env
				   (cl~with-context parent1 env
						    (cl~with-context parent2 env
								     (subst~read (fifth step) env)))))
	 (renaming (cl~with-context resolvent env
				   (cl~with-context parent1 env
						    (cl~with-context parent2 env
								     (subst~read (sixth step) env))))))
    (node~set-justification! 
     resolvent 
     (res~paramod-create parent1 pos1 parent2 pos2 direction unifier renaming name))
    resolvent))



#{\subsubsection{Instantiation}


This justification is the representation of an instantiation of a clause.
Though this step is not explicitly stated in most resolution calculi
it is usefull during programming for protocolling renamings etc.
The nodes slot contains one parent clause and the
positions slot is {\vb NIL}. The unifier slot contains a substitution which
instantiates the parent clause.
\begin{postsyntax}
\syntax{
\nt{proof-step}  ::=  (instance  \nt{name} \nt{resolvent} (\nt{parent}) \nt{unifier}).
}
\end{postsyntax}
Note the differences between \post\ representation and the representation of
\keim\ objects mentioned at resolution steps.

#}

(eval-when (load compile eval)
(defclass res+instance (res+justification)
    ()
  (:documentation 
  "Justification for instance steps: UNIFIER is a instanciates the clause in NODES.")))


(defun res~instance-create (parent unifier name)
  (declare (edited  "03-NOV-1992 15:09")
	   (authors RICHTS)
	   (input   "A clause, and substitution.")
	   (effect  "None.")
	   (value   "A justification object for the instance of parent."))
  (make-instance 'res+factoring :parents (list parent) :unifier unifier :name name :rule 'instance))

(defgeneric res~instance-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for a instance step or a node with such a justification."))
  (:method ((node node+node))
   (res~instance-p (node~justification node)))
  (:method ((fact-just res+instance))
   t)
  (:method ((object t))
   nil))

(defgeneric res~instance-clause (instance-just)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for a instance step or a node with such a justification.")
	   (effect  "None.")
	   (value   "The parent of this step (a clause)"))
  (:method ((node node+node))
   (res~instance-clause (node~justification node)))
  (:method ((res-just res+instance))
   (first (res=justification-parents res-just))))

(defmethod print-object ((instance-just res+instance) stream)
  (let ((parent (res~instance-clause instance-just))
	(unifier (res~just-unifier instance-just)))
    (format stream "Instance of ~S with substitution ~S:"
	    (keim~name parent) unifier)))

(defmethod node~post-print-step ((just res+instance) node stream)
  (format stream "(instance ~A " (keim~name just))
  (post~print node stream)
  (format stream "(~A) " (keim~name (res~factoring-clause just)))
  (post~print (res~just-unifier just) stream)
  (format stream ")"))

(defmethod post~read-object ((step list) (env env+environment)
			     (indicator (eql :instance)))
  (let* ((name (post~read-symbol (first step) env))
	 (resolvent (cl~read (second step) env))
	 (parent (cl~read (first (third step)) env))
	 (unifier (cl~with-context resolvent env
				   (cl~with-context parent env
						    (subst~read (fifth step) env)))))
    (node~set-justification! 
     resolvent 
     (res~instance-create parent unifier name))
    resolvent))


;xxxxx


(defgeneric res~step-p (object)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "T iff object is a justification for resolution proofs or a node with such a justification."))
  (:method ((node node+node))
   (res~step-p (node~justification node)))
  (:method ((res-just res+justification))
   t)
  (:method ((object t))
   nil))

(defgeneric res~just-unifier (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The unifier of this step (a substitution)"))
  (:method ((node node+node))
   (res~just-unifier (node~justification node)))
  (:method ((res-just res+justification))
   (res=justification-unifier res-just)))

(defgeneric res~just-renaming (justification)
  (declare (edited  "11-NOV-1992 12:07")
	   (authors RICHTS)
	   (input   "A justification for resolution proofs or a node with such a justification.")
	   (effect  "None.")
	   (value   "The renaming of this step (a substitution)"))
  (:method ((node node+node))
   (res~just-renaming (node~justification node)))
  (:method ((res-just res+justification))
   (res=justification-renaming res-just)))

(defmethod node~post-print-step ((just res+justification) node stream)
  (format stream "(~A " (keim~name just))
  (post~print node stream)
  (format stream " () () ")
  (post~print (res~just-unifier just) stream)
  (post~print (res~just-renaming just) stream)
  (format stream ")"))

(defun res~read-step (step env)
  (declare
   (input "a POST representation of a resolution step and an environment")
   (effect "The resolution step is parsed in using the environment"))
  (unless (and (listp step)
	       (symbolp (car step)))
    (post~error "~A does not match specification for a RESOLUTION STEP."
		step))
  (post~read-object (cdr step) env 
		    (intern (string (car step)) (find-package :keyword))))









#|
\nt{proof}    ::=   (proof \nt{name} \nt{status} \nt{problem} \nt{proof-result} \nt{prover}).
\nt{status}   ::=   proven | refuted | conjectured.

\nt{proof-result}    ::=   (resolution-proof \nt{cnf} \nt{proof-step}*).
\nt{prover}          ::=   (prover \nt{prover-name} \nt{options}).

\nt{cnf}       ::=   (cnf (\nt{clause}*) \nt{delta-relation}).

\nt{delta-relation}   ::=   (delta-relation \nt{relation-pair}*).
\nt{relation-pair}    ::=   (\nt{formula-name}  \{(\nt{position} \nt{literal-pos})\}+).
\nt{literal-pos}      ::=   (\nt{clause-name} \nt{position}).

\begin{postsyntax}
\syntax{
\nt{resolvent}         ::=   \nt{clause}.
\nt{parent}           ::=    \nt{clause-name}.
\nt{unifier}           ::=   \nt{substitution}.
\nt{renaming}          ::=   \nt{substitution}.


\nt{proof-step}  ::=  (resolution \nt{name} \nt{resolvent} (\nt{parent} \nt{literal-position})
                                            (\nt{parent} \nt{literal-position})
                                            \nt{unifier} \nt{renaming})
\nt{proof-step}  ::=  (factoring  \nt{name} \nt{resolvent} (\nt{parent} \nt{literal-position} \nt{literal-position})
                                            \nt{unifier} \nt{renaming})
\nt{proof-step}  ::=  (instance  \nt{name} \nt{resolvent} (\nt{parent}) \nt{unifier})
\nt{proof-step}  ::=  (paramodulation \nt{name} \nt{resolvent}
                             (\nt{parent} \nt{literal-position})
                             (\nt{parent} \nt{literal-position} \nt{direction})
                             \nt{unifier} \nt{renaming})
\nt{direction}  ::=   LR | RL.}
\end{postsyntax}
|#
