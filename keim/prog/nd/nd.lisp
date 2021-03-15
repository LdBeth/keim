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

(in-package "KEIM")
(mod~defmod nd
	    :uses (abstr appl assum conc env hop just keim mod node post
			 prob proof sym term termc top type )
	    :documentation "Natural deduction functions."
	    :exports (
		      nd+justification
		      nd+planned-justification
		      nd+verified-justification
		      nd~just-rule
		      nd~just-lines
		      nd~just-terms
		      nd~set-just-terms!
		      nd~make-planned-justification
		      nd~make-verified-justification
		      nd+line
		      nd~line-hyps
		      nd~set-line-hyps!
		      nd~line-supports
		      nd~set-line-supports!
		      nd~line-label
		      nd~set-line-label!
		      nd~line-p
		      nd~make-new-line-name
		      nd~line-just-rule
		      nd~line-just-terms
		      nd~line-just-lines
		      nd+proof
		      nd~proof-lines
		      nd~set-proof-lines!
		      nd~proof-planned-lines
		      nd~set-proof-planned-lines!
		      nd~proof-final-line
		      nd~set-proof-final-line!
		      nd~proof-label-line-hashtable
		      nd~set-proof-label-line-hashtable!
		      nd~proof-p
		      nd~find-proof
		      nd~make-proof
		      nd~def-nd-proof
		      nd~label-2-line
		      nd~existent-p
		      nd~copy-line
		      nd~delete-line
		      nd~cleanup
		      nd~planify
		      nd~plan-p
		      nd~sponsor
		      nd~unsponsor
		      nd~check-structure
		      nd~insert-line
		      nd~insert-line-after-before
		      nd~proof-done-p
		      nd~make-hypothesis
		      nd~hypothesis-line-p
		      nd~make-planned-line
		      nd~make-line
		      nd~start-proof
		      nd~focus
		      nd~proof-status
		      nd~not-free-in-lines-or-hyps-p
		      nd~not-free-in-terms-p
		      nd~change-top-var-quantor
		      nd~not-atom-p
		      nd~pushneg
		      nd~replace-and-contract
		      nd~make-problem-from-line
		      nd+output-style
		      nd~print-subproof
		      nd~print-line
		      nd~print-line-label
		      nd~print-line-list-label
		      nd~show-line
		      nd~print-label
		      nd~print-hyps
		      nd~print-turnstile
		      nd~print-formula
		      nd~print-justification
		      nd~print-space
		      nd~print-newline
		      nd~print-proof
		      nd~show-proof
		      nd~show-subproof
		      nd~print-line-just
		      nd~print-list-of-things-in-style
		      nd~print-ellipsis
		      nd+simple-output-style
		      nd+readable-output-style
		      nd~print-wff
		      nd~print-type
		      nd~pprint-proof
		      )

	    )

#{
\section{Natural Deduction Definitions } 
\label{mod:nd}
In this module, we define the things we need in order to carry out
natural deduction proofs.  Natural deduction proofs are defined as
a subclass of abstract proofs (class {\vb PROOF+ABSTRACT}).  We have to
add some additional slots and specialize on the components of the
proof structure.  

In the text below, we will often use ND as an abbreviation for
natural deduction.

\subsection{Natural Deduction Justifications}

First we define a new class of justifications.  These differ from their
parent class by adding the slot TERMS.  Sometimes we want to store a
term or terms when justifying a line in a natural deduction proof; for
example, we'll want to keep around the instantiation term used when
eliminating a universal quantifier. 

Because every line will have some kind of justification, two subclasses 
of {\vb ND+JUSTIFICATION},\\ {\vb ND+VERIFIED-JUSTIFICATION} and 
{\vb ND+PLANNED-JUSTIFICATION}, 
will allow us to distinguish, respectively, between
lines of the proof that have {\em really\/} been justified and those that
are still being planned.

Our justifications have three slots: a rule, a list of terms used in
applying the rule (if any), and a list of other lines that were used in
justifying this line.  We define some accessors for these slots.

Note: many of the ideas and even names of proof operations are taken
from Peter Andrews's TPS system.
#}

(eval-when (load compile eval)
(defclass nd+justification (just+justification)
  ((keim::nodes 
    :initarg :lines
    :documentation "The lines from which this line is justified.")
   (terms :reader nd=just-terms
	  :writer nd=set-just-terms!
	  :initarg :terms
	  :documentation "Any special terms used to justify this line."))
  (:documentation "The justification for a line."))

(defclass nd+planned-justification (nd+justification)
  nil
  (:default-initargs :rule "Planned" :lines nil :terms nil)
  (:documentation "A justification for a line which is not yet completely 
verified."))

(defclass nd+verified-justification (nd+justification)
  nil
  (:documentation "A justification for a line which is completely verified."))
)
(defgeneric nd~just-rule (just)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A justification")
           (effect  "None.")
           (value   "The value of the justification's rule slot."))
  (:method  ((just nd+justification))
	    (just~rule just))
  (:documentation "The rule that justifies this line."))

(defmethod print-object ((just nd+justification) stream)
  (format stream 
	  (if *print-escape*
	      "#<nd+justification (~S ~S ~S)>" 
	    "(~S ~S ~S)") 
	  (nd~just-rule just) (nd~just-terms just) (nd~just-lines just)))

(defgeneric nd~just-lines (just)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A justification")
           (effect  "None.")
           (value   "The value of the justification's lines slot."))
  (:method  ((just nd+justification))
	    (just~nodes just))
  (:documentation "The lines that justify this line.")
  )

(defgeneric nd~just-terms (just)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A justification")
           (effect  "None.")
           (value   "The value of the justification's terms slot."))
  (:method ((just nd+justification)) (nd=just-terms just))
  (:documentation "The terms that help justify this line.")
  )

(defgeneric nd~set-just-terms! (just newterms)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A justification")
           (effect  "Sets the terms slot of JUST to NEWTERMS.")
           (value   "Undefined."))
  (:method ((just nd+justification) newterms) 
    (nd=set-just-terms! newterms just))
  (:documentation "Sets the terms slot of a justification.")
  )

(eval-when (load compile eval)
(defun nd~make-planned-justification (rule terms lines)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule, a list of terms, a list of lines.")
           (effect  "None.")
           (value   "Constructs a planned justification using the inputs."))
  (make-instance 'nd+planned-justification
    :rule rule :terms terms :lines lines)))

(eval-when (load compile eval)
(defun nd~make-verified-justification (rule terms lines)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule, a list of terms, a list of lines.")
           (effect  "None.")
           (value   "Constructs a verified justification using the inputs."))
  (make-instance 'nd+verified-justification 
    :rule rule :terms terms :lines lines)))

#{\subsection{Natural Deduction Lines}

Natural deduction lines are based on the NODE+NODE class, but with 
added slots for the hypotheses of the line (lines upon which the
line is {\em logically\/} dependent) as well as for the supports of
this line.  A line's supports are lines which are believed to be useful
in proving the line.  Whereas a line's hypotheses are fixed and may not
be changed (except under very controlled circumstances), its supports
do not affect the logical correctness of the proof in any way, and can
be manipulated by the application of rules.
#}

(eval-when (load compile eval) 
(defclass nd+line (node+node)
  ((keim::name :initarg :label
	       :reader nd=line-label
	       :writer nd=set-line-label!)
   (hyps :reader nd=line-hyps
	 :writer nd=set-line-hyps!
	 :initarg :hyps
	 :documentation "Hypotheses upon which a line depends.")
   (supports :reader nd=line-supports
	     :writer nd=set-line-supports!
	     :initform nil
	     :initarg :supports
	     :documentation "List of lines from which this line is intended to
be proven.")
   (keim::justification :type nd+justification
	     :documentation "The justification for a line (rule name,
lines used, any required terms."))
  (:documentation "A line in a natural deduction proof.")))

(defmethod print-object ((line nd+line) stream)
  (format stream 
	  (if *print-escape* "#<nd+line ~S>" "~S")
	  (nd~line-label line)))



(defgeneric nd~line-hyps (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction line.")
           (effect  "None.")
           (value   "Value of hyps slot of the line."))
  (:method ((line nd+line)) (nd=line-hyps line))
  (:documentation "Return the hyps of a line."))

(defgeneric nd~set-line-hyps! (line newhyps)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of lines and a line.")
           (effect  "Sets hyps slot of LINE to NEWHYPS.")
           (value   "Undefined."))
  (:method ((line nd+line) newhyps) (nd=set-line-hyps! newhyps line)))

(defgeneric nd~line-supports (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "The support lines of LINE."))
  (:method ((line nd+line)) (nd=line-supports line))
  (:documentation "Supports for a line."))

(defgeneric nd~set-line-supports! (line newsupports)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of lines and a line.")
           (effect  "Sets supports slot of LINE to NEWSUPPORTS.")
           (value   "Undefined."))
  (:method ((line nd+line) newsupports) 
	   (nd=set-line-supports! newsupports line))
  (:documentation "Set the supports slot of a line.")
  )

(defgeneric nd~line-label (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "The line's label."))
  (:method ((line nd+line)) (nd=line-label line))
  (:documentation "The label of a line.")
)

(defgeneric nd~set-line-label! (line newlabel)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A label (symbol) and a line.")
           (effect  "None.")
           (value   "Sets LINE's label slot to NEWLABEL."))
  (:documentation "Set the label slot of a line.")
  (:method ((line nd+line) (newlabel symbol)) 
	   (nd=set-line-label! newlabel line)))


(defun nd=make-line (label hyps wff just supports)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name, list of hypotheses (other lines), a formula,
a justification, and a list of support lines.")
           (effect  "None.")
           (value   "A new line created with input values."))
  (make-instance 'nd+line :label label :hyps hyps :formula wff 
		 :justification just :supports supports))


(defun nd~line-p (thing)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type ND+LINE, else NIL."))
  (typep thing 'nd+line))



#{
Every time we create a line, we need a name for it, and if one isn't supplied,
we have to think of it on our own here.  We'll use the following function
to make new lines, basically of the form ``L1'', ``L2''.  To keep track
of which names we've already used in this proof, there's a counter kept
in each proof.
#}

(defvar nd*current-proof nil
  "The current proof.  Most proof functions will refer to this variable
as a default proof.")

(defun nd~make-new-line-name ()
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "None.")
           (effect  "None.")
           (value   "A new symbol."))
  (loop 
   (let ((count (incf (nd=proof-label-counter nd*current-proof))))
     (if (nd~label-2-line (format nil "L~A" count))
	 (incf (nd=proof-label-counter nd*current-proof))
	 (return-from nd~make-new-line-name 
	   (make-symbol (format nil "L~A" count))))))
  )

(eval-when (load eval compile)
(defmacro nd~line-just-rule (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "The line's justification rule."))
  `(nd~just-rule (node~justification ,line)))

(defmacro nd~line-just-terms (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "The line's justification terms."))
  `(nd~just-terms (node~justification ,line)))

(defmacro nd~line-just-lines (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "The line's justification lines."))
  `(nd~just-lines (node~justification ,line))))



#{\subsection{Natural Deduction Proofs}
We define our proofs with some extra slots.  In order to quickly find
the lines that not yet justified (on which we will of course concentrate
our efforts), we keep a list of them current.  We also keep track of what
the final line of the proof is, that is, just what the ultimate goal of
this proof is.  We also, in order to be able to quickly translate from
user inputs referring to the symbol names of lines to the lines themselves,
we keep a hashtable with this association.   We also keep a integer counter
for use in generating new line names (as noted above).
#}

#{We'll use this hash table to keep track of all the proofs that
exist at any given time. Note that the names of proofs are
case-insensitive.
#}

(defvar nd*proof-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by proof name, that holds all existing proofs.
This way we can jump back and forth between proofs.")

#{Since most of our functions deal with only one proof at a time, 
it is useful to keep this variable around to refer to the current proof.
#}

(defvar nd*current-proof nil
  "The current proof.  Most proof functions will refer to this variable
as a default proof.")

(defvar nd*current-proof-environment nil
  "The environment of the current proof, nd*current-proof.  Some functions 
will refer to this variable as a default environment.")


(eval-when (load compile eval) 
(defclass nd+proof (proof+abstract) 
  ((keim::steps
    :initarg :lines
    :documentation "Lines in the proof.")
   (planned-lines :initarg :planned-lines
		  :reader nd=proof-planned-lines
		  :writer nd=set-proof-planned-lines!
		  :documentation "The lines which still are unjustified.")
   (final-line :initarg :final-line
	       :reader nd=proof-final-line
	       :writer nd=set-proof-final-line!
	       :documentation "The last line of the proof.")
   (label-line-hashtable
    :reader nd=proof-label-line-hashtable
    :writer nd=set-proof-label-line-hashtable! ;aaaaaah!!!!
    :initarg :label-line-hashtable
    :initform (make-hash-table :test #'equal)
    :documentation "A mapping from labels to lines.")
   (label-counter
    :accessor nd=proof-label-counter
    :initarg :label-counter
    :initform 0)
   )
   (:documentation "A natural deduction proof object.")))

(defmethod print-object ((prf nd+proof) stream)
  (format stream 
	  (if *print-escape* "#<nd+proof ~S>" "~S")
	  (keim~name prf)))

(defgeneric nd~proof-lines (ndproof)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction proof.")
           (effect  "None.")
           (value   "The lines of the proof."))
  (:method ((ndproof nd+proof)) (proof~steps ndproof))
  (:documentation "The lines of a nd proof.")
)

(defgeneric nd~set-proof-lines! (ndproof newlines)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of lines and an nd-proof. ")
           (effect  "None.")
           (value   "Sets NDPROOF's label slot to NEWLINES."))
  (:documentation "Set the lines slot of an NDPROOF.")
  (:method ((ndproof nd+proof) newlines) (proof~set-steps! ndproof newlines)))

(defgeneric nd~proof-planned-lines (ndproof)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction proof.")
           (effect  "None.")
           (value   "The planned lines of the proof."))
  (:method ((ndproof nd+proof)) (nd=proof-planned-lines ndproof)))

(defgeneric nd~set-proof-planned-lines! (ndproof newlines)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of lines and an ndproof.")
           (effect  "None.")
           (value   "Sets NDPROOF's planned-lines slot to NEWLINES."))
  (:documentation "Set the planned-lines slot of an ndproof.")
  (:method ((ndproof nd+proof) newlines) (nd=set-proof-planned-lines! newlines
								      ndproof)))

(defgeneric nd~proof-final-line (ndproof)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction proof.")
           (effect  "None.")
           (value   "The final line of the proof."))
  (:documentation "The final line of the proof.")
  (:method ((ndproof nd+proof)) (nd=proof-final-line ndproof)))

(defgeneric nd~set-proof-final-line! (ndproof line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line and an ndproof.")
           (effect  "None.")
           (value   "Sets NDPROOF's final-line slot to LINE."))
  (:documentation "Set the final-line slot of an ndproof .")
  (:method ((ndproof nd+proof) line) (nd=set-proof-final-line! line
							       ndproof)))

(defgeneric nd~proof-label-line-hashtable (ndproof)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction proof.")
           (effect  "None.")
           (value   "The label-line-hashtable of the proof."))
  (:documentation "The label-line-hashtable of the proof.")
  (:method ((ndproof nd+proof)) (nd=proof-label-line-hashtable ndproof)))

(defgeneric nd~set-proof-label-line-hashtable! (ndproof hashtable)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A hashtable and a ndproof.")
           (effect  "None.")
           (value   "Sets NDPROOF's label-line-hashtable to HASHTABLE."))
  (:documentation "Set the label-line-hashtable slot of a line.")
  (:method ((ndproof nd+proof) hashtable) 
	   (nd=set-proof-label-line-hashtable! hashtable
					       ndproof)))

(defun nd~proof-p (thing)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "Any THING.")
           (effect "None")
           (value   "T if THING is of type ND+PROOF, else NIL."))
  (typep thing 'nd+proof))




#{
Given the name of a proof, we want to find the proof itself.  Note that
these names are case insensitive.
#}

(defun nd~find-proof (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a proof (symbol or string).")
           (effect  "None.")
           (value   "The proof object with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   nd*proof-hash-table))


(defun nd~make-proof (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A symbol NAME to be used as name for the proof.")
           (effect  "A new proof is made, and put in hash table of proofs.")
           (value   "The new proof with name NAME."))
  (let ((new-proof (make-instance 'nd+proof :name name)))
    (setf (gethash (symbol-name name) nd*proof-hash-table)
	  new-proof)
    new-proof))


#{\subsection{Reading a Natural Deduction Proof}

It is of course useful to have a way to store natural deduction proofs
and be able to read them back in. The macro nd~def-nd-proof allows you
to do that.

First let's give an example of this syntax.
\begin{code}
(nd~def-nd-proof example
 (declarations 
  (type-variables AA)
  (type-constants O I)
  (constants
   (exists (O (O AA)))
   (P (O I))
   (A I)))
  (assumptions (L1 (P A)))
  (conclusion L100 (exists (lam (Y I) (P Y))))
  (plans (L100))
  (lines
   (L1   (L1) (P A)                      ("Hyp"     () ()) ())
   (L100 (L1) (exists (lam (Y I) (P Y))) ("Planned" () ()) (L1))))
\end{code}

Now here's a description of the syntax:

\begin{code}
(nd~def-nd-proof {\it proofname}
  (declarations {\it post-declaration}* )
  (assumptions ({\it linename} {\it formula})* )
  (conclusion {\it linename} {\it formula})
  (plans ({\it linename}*))
  (lines {\it line}*)
   ).

{\it line} ::== 
 ({\it linename} ({\it hypname}*) {\it formula} {\it justification} ({\it support}*)).

{\it justification} ::== ({\it rule} ({\it term}*) ({\it justifying-linename}*)).
\end{code}

Above, {\it proofname}, as well as {\it linename}, {\it hypname}, {\it support}
and 
{\it justifying-linename}, is a symbol.  {\it post-declaration} is any
declaration suitable for adding to an environment. {\it formula} is the
POST representation of a formula, suitable for reading, and each {\it term}
must be a POST term. 

Here's basically what happens when you read something like this in.
A new ND proof is created with the name {\it proofname}, and with an
environment that consists only of the symbols declared in the declaration
part of the form.  The lines are created using the {\it lines} given.  Their
order doesn't matter, because we make sure they are created in the proper
order. For each line, we read in the list of {\it hypname}s as the
hypotheses of the line, the {\it formula} as its (of course) formula, 
and we create a justification from the rule name (a string), the list of
terms (which we first parse), and justifying-lines.  When the string
``Planned'' is given as justifying rule, we assume the line to be unjustified.
The lines indicated as {\it support}s are added as support lines.

There is some redundancy in this syntax and it is used to check for 
consistency.  The linename and formula given in the conclusion field must
match with those given in the line's description.  The assumptions are
the only lines that may be used as hypotheses of the conclusion. 
The planned lines should be exactly those specified in the plans
field.
#}

(defmacro nd~def-nd-proof (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an nd-proof.")
           (effect  "Read the proof, construct a real nd-proof.")
           (value   "The new proof."))
  (let ((decl-list nil)
	(hyp-list nil)
	conc
	(conc-p nil)
	(plan-list nil)
	line-list
	(line-list-p nil))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (cond
	    ((string-equal (car attrib) :declarations)
	     (setq decl-list (cdr attrib)))
	    ((string-equal (car attrib) :proof-hyps)
	     (setq hyp-list (cdr attrib)))
	    ((string-equal (car attrib) :assumptions)
	     (setq hyp-list (cdr attrib)))
	    ((string-equal (car attrib) :conclusion)
	     (setq conc (cdr attrib) ; (cadr attrib)
		   conc-p t))
	    ((string-equal (car attrib) :plans)
	     (setq plan-list (cadr attrib)))
	    ((string-equal (car attrib) :lines)
	     (setq line-list (cdr attrib)
		   line-list-p t))
	    (t
	     (error ";;;nd~~def-nd-proof: Not expecting ~S" attrib)))
	(error ";;;nd~~def-nd-proof: Not expecting ~S" attrib)))
    (unless conc-p
      (error ";;;nd~~def-nd-proof: Must specify conclusion."))
    (unless line-list-p
      (error ";;;nd~~def-nd-proof: Must specify line list."))
;;; do some error-checking here before beginning.
  `(block nil
     ;; in this when, we could ask if the user wants to write over
     ;; a proof that already exists.  
     (when (nd~find-proof ',name)
       (warn "Redefining ND-PROOF ~A" ',name))
     (let* ((new-proof (make-instance 'nd+proof 
			 :name ',name :lines nil 
			 :planned-lines nil
			 :environment 
			 (env~create)))
	    (nd*current-ndproof new-proof)
	    (nd*current-proof-environment 
	     (prob~environment nd*current-ndproof))
	    (line-label-mapping nil))
       (labels ((look-up-line (label)
		  (let ((line (cdr (assoc label line-label-mapping))))
		    (if line 
			line
		      (error "nd~~def-nd-proof: ~S is not a line." label))))
		(get-just (just)
		  (funcall (if (string-equal (first just) "Planned")
			       #'nd~make-planned-justification
			     #'nd~make-verified-justification)
		   (first just)
		   (mapcar #'get-wff (second just))
		   (mapcar #'look-up-line (third just))))
		;; the following have to be fleshed out
		(process-declarations (decl-list) 
		  (post~read-object-list decl-list 
					 nd*current-proof-environment))
		(get-wff (wff)
		  (term~read wff nd*current-proof-environment))
		(check-just (line just)
		  (if (member line (just~nodes just))
		      (error "nd~~def-nd-proof: Illegal justification: ~
                              Line ~A justifies itself." (nd~line-label line))
		    just))
		(check-proof (proof)
		  (let* ((proof-hyps (prob~assumptions proof))
			 (final-line (nd~proof-final-line proof))
			 (final-line-hyps
			  (nd~line-hyps final-line))
			 (conc (prob~conclusion proof)))
		    (unless (and (term~equal-p (conc~formula conc)
					       (node~formula final-line))
				 (string-equal (keim~name conc)
					       (nd~line-label final-line)))
		      (error "nd~~def-nd-proof: Conclusion does not match ~
                              formula on final line."))
		    (when (or 
			   (not (= (length proof-hyps) 
				   (length final-line-hyps)))
			   (dolist (proof-hyp proof-hyps final-line-hyps)
			     (let ((match 
				    (find-if 
				     #'(lambda (x)
					 (and (string-equal 
					       (keim~name proof-hyp)
					       (nd~line-label x))
					      (term~equal-p
					       (assum~formula 
						proof-hyp)
					       (node~formula x))))
				     final-line-hyps)))
			       (if match
				   (setq final-line-hyps 
					 (remove match final-line-hyps))
				   (return t)))))
		      (error "nd~~def-nd-proof: Final line's hypotheses ~
                              and the proof hypotheses do not match up."))
		    proof)))
	 (process-declarations ',decl-list)
	 (prob~set-conclusion! new-proof 
			       (conc~create 
				(car ',conc) (get-wff ',(cadr conc))
				))
	 (prob~set-assumptions! new-proof 
				(mapcar #'(lambda (x) 
					    (assum~create 
					     (car x) 
					     (get-wff (cadr x))))
					',hyp-list))
	 (dolist (line ',line-list)
	   (let ((label (first line)))
	     (push (cons label (make-instance 'nd+line :name label))
		   line-label-mapping)))
	 (dolist (line ',line-list
		   (let ((hashtable (nd~proof-label-line-hashtable new-proof)))
		     (dolist (line (nd~proof-lines new-proof))
		       (setf (gethash (symbol-name (nd~line-label line)) 
				      hashtable)
			 line))))
	   (let ((new-line nil)
		 (label (first line)))
	     (setq new-line (look-up-line label))
	     (let ((hyps (mapcar #'look-up-line (second line)))
		   (wff (get-wff (third line)))
		   (just (get-just (fourth line)))
		   (supports (mapcar #'look-up-line (fifth line))))
	       (nd~set-line-hyps! new-line hyps)
	       (node~set-formula! new-line wff)
	       (node~set-justification! new-line 
					(check-just new-line just))
	       (nd~set-line-supports! new-line supports)
	       (nd~set-proof-lines! 
		new-proof
		(nconc (nd~proof-lines new-proof) (list new-line)))
	       )))

	 (nd~set-proof-final-line! 
	  new-proof
	  (car (last (nd~proof-lines new-proof))))
	 (nd~set-proof-planned-lines! new-proof
				      (mapcar #'look-up-line ',plan-list))
	 (check-proof new-proof))
       (setf (gethash (symbol-name ',name) nd*proof-hash-table)
	 new-proof)))))


#{
\subsection{Proof Operations} 
#}

(defun nd~label-2-line (label)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a line.")
           (effect  "None.")
           (value   "The line in nd*current-proof with this name."))
  (gethash 
   (etypecase label
     (symbol (symbol-name label))
     (string label))
   (nd~proof-label-line-hashtable nd*current-proof)))

(defun nd~existent-p (label)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a line.")
           (effect  "None.")
           (value   "Non-nil iff a line with this name exists in 
nd*current-proof."))
  (nd~label-2-line label))


(defun nd~copy-line (line)
   (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "None.")
           (value   "A copy of the line."))
  (with-slots (name hyps formula justification supports) line
    (nd=make-line name (copy-list hyps) 
		  (term~copy formula) 
		  justification
		  (copy-list supports))))

(defun nd~delete-line (line &optional (verbose t))
   (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "Delete the line from nd*current-proof, along with 
                     lines that were inferred from it.")
           (value   "True if succeeds, nil otherwise."))
  (when (eq line (nd~proof-final-line nd*current-proof))
    (warn "ND~~DELETE-LINE: Won't delete final line of proof.")
    (return-from nd~delete-line nil))
  (when (member line (nd~line-hyps (nd~proof-final-line nd*current-proof)))
    (warn "ND~~DELETE-LINE: Won't delete hypothesis of final line of proof.")
    (return-from nd~delete-line nil))
  (dolist (x (nd~proof-lines nd*current-proof))
    (let ((y (nd~line-just-lines x))
	  (z (nd~line-hyps x)))
      (cond 
       ;; if x is the line we want to delete, change its justification.
       ((eq line x)
	(node~set-justification! x     
				 (nd~make-planned-justification 
				  "Deleted" nil nil))
	(when verbose
	  (nd~show-line x)))
       ;; if the line is one of x's hypotheses, delete x itself.
       ((member line z :test #'eq) 
	(nd~delete-line x verbose))
       ;; if the line is used to justify x, then change x to a planned line.
       (y (when (member line y)
	    (nd~planify x)
	    (when verbose
	      (nd~show-line x)))))))
  ;; here really delete the line itself
  (nd~set-proof-lines! nd*current-proof
		       (delete line (nd~proof-lines nd*current-proof)))
  ;; get it out of the plans as well.
  (nd~set-proof-planned-lines! nd*current-proof
			       (delete line 
				       (nd~proof-planned-lines 
					nd*current-proof)))
  ;; if line was used as a support anywhere, get rid of it.
  (dolist (x (nd~proof-lines nd*current-proof))
    (nd~set-line-supports! x (delete line (nd~line-supports x)))
    )
  t)


(defun nd~cleanup (nd*current-proof)
  (declare
   (authors nesmith)
   (input "An nd+proof.")
   (effect "Removes lines not used in the proof and removes redundant steps, 
where a line is justified by a line exactly like it.  Proof must have no
remaining planned lines, otherwise an error is signaled.")
   (value "Undefined"))
  (if (nd~proof-planned-lines nd*current-proof)
      (error "There are still planned lines. Cleanup not applicable.")
      (progn
	(nd=cleanup-hypotheses nd*current-proof)
	(nd=cleanup-same nd*current-proof)
	(nd=cleanup-justifying nd*current-proof))))


(defun nd=cleanup-justifying (nd*current-proof)
  (declare
   (authors nesmith)
   (input "An nd+proof.")
   (effect "Removes lines not used in nd*current-proof, that is, those who
are not used in justifying the final line of the proof."))
  (let ((lines (set-difference
		 (nd~proof-lines nd*current-proof) 
		 (nd=justifying-lines 
		  (list (nd~proof-final-line nd*current-proof))))))
    (when lines
      (dolist (line lines) (nd~delete-line line)))))


(defun nd=justifying-lines (justified-lines)
  (declare
   (authors nesmith)
   (input "A list of nd lines.")
   (value "The lines which justify the input lines, as well as the lines which
justify them, ad infinitum, recursively.  That is, all lines upon which the 
input lines depend."))
  (do ((jlines justified-lines (append just-lines (cdr jlines)))
       (curr-jline)
       (just-lines)
       (justifying-lines justified-lines
			 (pushnew (car jlines) justifying-lines)))
      ((endp jlines) justifying-lines)
    (setq curr-jline (car jlines))
    (setq just-lines 
      (nd~line-just-lines curr-jline))))


(defun nd=sort-lines (lines)
  ;; put lines in order such that a line comes after its hypotheses and
  ;; justifying lines
  (declare
   (authors nesmith)
   (input "A list of nd lines.")
   (value "Sorted version of the list in which a line's hypotheses and 
justifying-lines all occur before it.  Destructive."))
  (stable-sort lines
	       #'(lambda (x y)
		   (or (member x (nd~line-hyps y))
		       (member x (nd~line-just-lines y))))))

(defun nd=cleanup-hypotheses (nd*current-proof)
  (declare
   (authors nesmith)
   (input "An nd+proof.")
   (effect "Minimizes the hypotheses of each line to those actually used in
its proof."))
  (dolist (line (nd=sort-lines (copy-list (nd~proof-lines nd*current-proof))))
    (let ((hyps (nd~line-hyps line)))
      (nd~set-line-hyps! line
	    (if (nd~hypothesis-line-p line)    ;; a hypothesis
		(list line)
	      (intersection (nd=justifying-hyps line) hyps))))))


(defun nd=justifying-hyps (line)
  (declare
   (authors nesmith)
   (input "An nd line.")
   (value "A list of hypotheses of the justifying lines of the input line."))
  (remove-duplicates 
   (apply #'append 
	  (mapcar #'nd~line-hyps 
		  (nd~line-just-lines line)))))


(defun nd=cleanup-same (nd*current-proof)
  (declare
   (authors nesmith)
   (input "An nd proof.")
   (effect "Lines in the proof are checked one by one. If a line is justified
by another line whose formula is identical to the first line, and whose
hypotheses are a subset of those of the first line, then the first line is
replaced by the line that justifies it in the justifying-lines and hypotheses
of all other lines in the proof."))
  (dolist (line (nd=sort-lines (copy-list (nd~proof-lines nd*current-proof))))
    (let ((jlines (nd~line-just-lines line))
	  (hyps (nd~line-hyps line))
	  (formula (node~formula line)))
      (dolist (jline jlines)
	(when (and (subsetp (nd~line-hyps jline) hyps)
		   (term~equal-p formula
				 (node~formula jline)))
	  (nd=change-refs line jline nd*current-proof)
	  (return))))))

(defun nd=change-refs (from-line to-line nd*current-proof)
  (declare
   (authors nesmith)
   (input "Two nd lines and an nd proof in which they occur.")
   (effect "The first nd line will be replaced by the second one everywhere it
occurs in the given nd proof as a hypothesis or justifying line."))
  (dolist (line (nd~proof-lines nd*current-proof)
	    (nd~delete-line from-line))
    (nd~set-line-hyps! line 
		      (nsubst to-line from-line (nd~line-hyps line)))
    (just~set-nodes! 
     (node~justification line)
     (nsubst to-line from-line (nd~line-just-lines line)))))



(defun nd~planify (line)
   (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect   "Make this line a planned line.")
           (value  "Undefined."))
   (node~set-justification! line 
			    (nd~make-planned-justification "Planned" nil nil))
   (nd~set-proof-planned-lines! 
    nd*current-proof
    (cons line (nd~proof-planned-lines nd*current-proof))))

(defun nd~plan-p (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect   "None.")
           (value  "True if line is planned line, nil otherwise."))
  (typep (node~justification line) 'nd+planned-justification)
  )

(defun nd~sponsor (planline newsupports)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A planned line and a list of new support lines.")
           (effect   "Adds new support lines to the support lines of 
the planned line.")
           (value  "The new list of supports."))
  (nd~set-line-supports! planline 
			 (remove-duplicates 
			  (append (nd~line-supports planline)
				  newsupports))))

(defun nd~unsponsor (planline oldsupports)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A planned line and a list of support lines to be removed.")
           (effect   "Removes given support lines from the support lines of the planned line.")
           (value  "The new list of supports."))
  (nd~set-line-supports! planline 
			 (remove-if #'(lambda (x) (member x oldsupports))
				    (nd~line-supports planline))))

; Next section we check the proof to make sure things look kosher.

(defun nd~check-structure ()
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "None")
           (effect   "Checks nd*current-proof to see if its 
structure of plans, supports and hypotheses makes sense. 
Informative and messages should result. Probably doesn't
work.")
           (value  "Undefined."))
  (let ((lines (nd~proof-lines nd*current-proof))
	(planned-lines (nd~proof-planned-lines nd*current-proof)))
    (if (null lines)
	(format t "~%There are no lines.~%")
      (if (null planned-lines)
	  (format t "~%Your proof is complete.~%")
	(progn
	  (warn
	   "Now checking for lines not connected to any planned line ...")
	  (nd=dangle-main planned-lines lines)
	  (warn "Now checking for extra hypotheses in the support lines ...")
	  (nd=check-hyps-main planned-lines))))))

(defun nd=dangle-main (plans all-lines)
  (let ((dangling-lines
	 (nd=dangling-lines plans all-lines)))
    (if dangling-lines
	(progn (warn "You have the following dangling lines: ")
	       (nd~print-line-list-label dangling-lines)
	       (warn  "Perhaps you need to use ND~~SPONSOR to show me where they could be used."))
      (warn "There are no dangling lines."))))

(defun nd=dangling-lines (plines all-lines)
  (let ((slines (delete-duplicates 
		 (apply #'append (mapcar #'nd~line-supports plines)))))
    ;; Now to the slines we also must add those lines which appear
    ;; in a justfication of a depending-on-pline.  For example, after
    ;; Exists-E, the existentially quantified line does not occur in the
    ;; justification of the new hypothesis, but in only in the
    ;; justification of the line justified by Exists-E.
    ;; We do this by looking through the depending-on-pline and
    ;; collecting those lines which are used to justify them, but do
    ;; not themselves depend on a pline.
    (let* ((depending-on-pline (nd=dependent-lines plines all-lines))
	   (justifying-support-line (nd=justifying-lines
				     (nd=add-special-lines depending-on-pline
							slines))))
      (let ((all-dangling-lines
	     (remove-if-not #'(lambda (x) (or (member x depending-on-pline)
					      (member x justifying-support-line)))
			    all-lines)))
	all-dangling-lines))))


(defun nd=dependent-lines (start-lines all-lines)
  ;; Hypotheses are ignored for now.
  ;; We are using the fact that all-lines are sorted in ascending order
  (do ((all-lines (cdr all-lines) (cdr all-lines))
       (curr-line (car all-lines) (car all-lines))
       (dependent-lines start-lines))
      ((null curr-line) dependent-lines)
    (when (some #'(lambda (x) (member x dependent-lines))
		(nd~line-just-lines curr-line))
      (push curr-line dependent-lines))))

(defun nd=add-special-lines (depend-on-pline slines)
  (do ((dplines depend-on-pline (cdr dplines))
       (jlines))
      ((null dplines) slines)
    (setq jlines 
      (remove-if-not #'(lambda (jline) (member jline depend-on-pline))
		     (nd~line-just-lines (car dplines))))
    (setq slines (append jlines slines))))

(defun nd=check-hyps-main (planned-lines)
  (do ((ps-pairs (mapcar #'(lambda (x) 
			     (cons x (copy-list (nd~line-just-lines x))))
			 planned-lines)
		 (cdr ps-pairs))
       (found-p nil)
       (supp-hyps) (plan-hyps) (extra-hyps))
      ((null ps-pairs)
       (if found-p
	  (progn
	    (warn "You have some extra hypotheses."))
	 (warn "The structure of your hypotheses looks fine.")))
    (setq supp-hyps (nd=joint-hyps (cdar ps-pairs)))
    (setq plan-hyps (nd~line-hyps (caar ps-pairs)))
    (setq extra-hyps (set-difference supp-hyps plan-hyps))
    (when extra-hyps
	  (warn "The hypotheses ")
	  (nd~print-line-list-label extra-hyps)
	  (warn " are extraneous in the support of planned line ")
	  (nd~print-line-label (caar ps-pairs))
	  (setq found-p t))))

(defun nd=joint-hyps (lines)
  (do ((lines lines (cdr lines))
       (all-hyps nil 
		   (union all-hyps (nd~line-hyps (car lines)))))
      ((null lines) all-hyps)))

;; simplistic, can have better ones that take advantage of knowing
;; where this line comes from..
(defun nd~insert-line (line &optional (proof nd*current-proof))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A new line and a proof.")
           (effect  "Insert the line into the proof before lines it supports
or justifies and after all its hypotheses.")
           (value  "Undefined."))
  (let ((hashtable (nd~proof-label-line-hashtable proof)))
    (setf (gethash (symbol-name (nd~line-label line)) hashtable)
      line))
  (when (nd~plan-p line)
    (nd~set-proof-planned-lines! 
     proof 
     (union (list line) (nd~proof-planned-lines proof))))
  (let* ((supports (nd~line-supports line))
	 (hyps (nd~line-hyps line))
	 (hyps-and-supports (append (copy-list supports)
				    (copy-list hyps)))
	 (proof-lines* (nd~proof-lines proof)))
       ;; walk down proof-lines, just stick this guy in after all his
       ;; hyps and lines that justify him 
    (let ((next-line-pos 
	   (or (position-if
		#'(lambda (pline)
		    (null (intersection (member pline proof-lines*)
					hyps-and-supports)))
		proof-lines*)
	       (length proof-lines*))))
      (nd~set-proof-lines! proof 
			   (nconc
			    (subseq proof-lines* 0 next-line-pos)
			    (cons line (subseq proof-lines* next-line-pos)))))
      ))

; insert new-line after the earlier lines but before the later lines
(defun nd~insert-line-after-before (line earlier-lines later-lines
				    &optional (proof nd*current-proof))
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A LINE, a list of EARLIER-LINES, and a list of 
LATER-LINES.  PROOF is an optional argument which defaults to 
ND*CURRENT-PROOF.")
	   (effect  "The LINE is inserted into the proof steps so that is
comes after all the EARLIER-LINES and before all the LATER-LINES.")
	   (value   "Undefined."))
  (let ((hashtable (nd~proof-label-line-hashtable proof)))
    (setf (gethash (symbol-name (nd~line-label line)) hashtable)
      line))
  (when (nd~plan-p line)
    (nd~set-proof-planned-lines! 
     proof 
     (union (list line) (nd~proof-planned-lines proof))))
  (let* ((proof-lines* (nd~proof-lines proof))
	 (next-line-pos 
	  (or (if later-lines 
		  (apply #'min
			 (mapcar #'(lambda (x) (position x proof-lines*))
				 later-lines))
		(position-if
		 #'(lambda (pline)
		     (null (intersection (member pline proof-lines*)
					 earlier-lines)))
		 proof-lines*))
	      (length proof-lines*))))
    (nd~set-proof-lines! proof 
			 (nconc
			  (subseq proof-lines* 0 next-line-pos)
			  (cons line (subseq proof-lines* next-line-pos)))))
  )



; here need updating plan stuff
; assume that the proof is well-constructed, i.e., that other
; proof-modifying functions are okay.  Only reason this might not be
; a proof is if some required line is still planned.

(defun nd~proof-done-p (&optional (nd*current-proof nd*current-proof))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "An optional proof (defaults to nd*current-proof).")
           (effect  "none.")
           (value  "True if proof is complete, nil otherwise."))
  (cond ((nd~proof-planned-lines nd*current-proof)
	 (warn "There are still planned lines: ~A ."
		(mapcar #'nd~line-label (nd~proof-planned-lines 
					 nd*current-proof)))
	 (return-from nd~proof-done-p nil))
	((not (nd~proof-lines nd*current-proof))
	 (warn "The proof doesn't contain any lines!")
	 (return-from nd~proof-done-p nil)))
  (labels ((ck-just (line)
	   (when (nd~plan-p line)
	     (warn "Line ~A is still planned."  (nd~print-line-label line))
	     (return-from nd~proof-done-p nil))
	   (mapc #'ck-just (nd~line-just-lines line))))
    (if (ck-just (nd~proof-final-line nd*current-proof)) t)))


(defun nd~make-hypothesis (formula label)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A formula and a label.")
           (effect  "none")
           (value    "A new hypothesis line with label LABEL and assertion FORMULA."))
  (let ((new-line
	 (make-instance 'nd+line :name label
			:formula formula
			:hyps nil
			:justification 
			(nd~make-verified-justification "Hyp" nil nil))))
    (nd~set-line-hyps! new-line (list new-line))
    (nd~set-line-supports! new-line (list new-line))
    new-line))

(defun nd~hypothesis-line-p (line)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A line.")
           (effect  "none")
           (value  "True if line is a hypothesis, nil otherwise."))
  (and (member line (nd~line-hyps line))
       t))

(defun nd~make-planned-line (formula hyps supports label)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A formula, a list of hypothesis lines, a list of support
lines, and a label.")
           (effect  "none.")
           (value   "A new planned line with label LABEL, hypotheses HYPS, suports SUPPORTS,
and assertion FORMULA."))
  (make-instance 'nd+line :name label
			:formula formula
			:hyps hyps
			:supports supports
			:justification
			(nd~make-planned-justification "Planned" nil nil)))

(defun nd~make-line (formula hyps supports label justification)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A formula, a list of hypothesis lines, a list of support
lines, a label and a justification.")
           (effect  "none.")
           (value   "A new planned line with label LABEL, hypotheses HYPS, suports SUPPORTS,
assertion FORMULA and the JUSTIFICATION."))
  (make-instance 'nd+line :name label
		 :formula formula
		 :hyps hyps
		 :supports supports
		 :justification
		 justification))

(defun nd~start-proof (conclusion hyp-list name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A formula, a list of hypothesis formulas and a name.")
           (effect "Start a new proof with name NAME of the formula CONCLUSION, allowing
use of the formulas in HYP-LIST.  CONCLUSION should be a CONC+CONCLUSION,
and HYP-LIST a list of ASSUM+ASSUMPTIONS; NAME is a symbol.  Sets nd*current-proof to new proof and returns 
it.")
           (value   "The new proof."))
  (let ((new-proof (nd~make-proof name))
	(hyps nil)
	(conc-line nil))
    (prob~set-assumptions! new-proof (copy-list hyp-list))
    ; make hyps
    (setq hyps
      (let ((hyps nil))
	(dotimes (i (length hyp-list) (nreverse hyps))
	  (push (nd~make-hypothesis 
		 (assum~formula (nth i hyp-list))
		 (keim~name (nth i hyp-list)))
		hyps))))
    ; make conclusion, using hyps
    (setq conc-line
      (nd~make-planned-line (conc~formula conclusion)
			    hyps hyps (keim~name conclusion)))
    ; put all stuff in right places
    (nd~set-proof-lines! new-proof (append hyps (list conc-line)))
    (dolist (line (nd~proof-lines new-proof))
      (let ((hashtable (nd~proof-label-line-hashtable new-proof)))
	(setf (gethash (symbol-name (nd~line-label line)) hashtable)
	  line)))
    (nd~set-proof-final-line! new-proof conc-line)
    (prob~set-conclusion! new-proof conclusion)
    (nd~set-proof-planned-lines! new-proof (list conc-line))
    (setq nd*current-proof new-proof)
    nd*current-proof))




(defun nd~focus (line &optional (nd*current-proof nd*current-proof))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A LINE, and an optional 
PROOF (defaults to ND*CURRENT-PROOF)")
           (effect "Make this LINE (if a planned line) the current focus
of attention (i.e., the first planned line).")
           (value   "T if carried out, NIL if not."))
  (when (and (nd~proof-p nd*current-proof)
	     (nd~line-p line)
	     (nd~plan-p line))
    (nd~set-proof-planned-lines! nd*current-proof
     (cons line (delete line (nd~proof-planned-lines nd*current-proof))))
    t))


	     
(defun nd~proof-status (&optional (nd*current-proof nd*current-proof))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "An optional PROOF (defaults to ND*CURRENT-PROOF).")
           (effect "For each planned line, prints the line name and names
of its supporting lines.")
           (value   "Undefined"))
  (let ((plines (nd~proof-planned-lines nd*current-proof)))
    (if plines
	(progn
	  (format t "~%Planned lines    Supports")
	  (dolist (pline plines t)
	    (format t "~%~A~17T~{~A ~}~%" (nd~line-label pline)
		    (mapcar #'nd~line-label (nd~line-supports pline)))))
      (format t "~%No planned lines in proof ~A.~%" 
	      (keim~name nd*current-proof)))))

#{\subsection{Functions used in rule definitions}

We will want, in defining some rules, to describe some properties or
operations on ND proofs.  Here are a few of these operations.
#}

(defun nd~not-free-in-lines-or-hyps-p (term &rest lines)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A TERM and  as &rest argument a list of LINES.")
	   (effect  "None.")
	   (value   "T if TERM is not free in (occurs un-lambda-bound in)
LINES' formulas nor free in any of their hypotheses, otherwise nil."))
  (let ((hyps (delete-duplicates
	       (mapcan #'copy-list (mapcar #'nd~line-hyps lines)))))
    (dolist (line 
		(remove-duplicates (append lines hyps)) 
	      t)
      (let ((unbound-symbols
	     (top~all-unbound-symbols (node~formula line))))
	(if (member term unbound-symbols :test #'term~equal-p)
	    (return-from nd~not-free-in-lines-or-hyps-p nil))))))

(defun nd~not-free-in-terms-p (term &rest others)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A TERM and as &rest argument a list of OTHERS terms.")
	   (effect  "None.")
	   (value   "T if TERM is not free in the given terms 
(occurs un-lambda-bound in) in the OTHERS, otherwise nil."))
  (dolist (other others t)
    (when (member term (top~all-unbound-symbols other))
      (return-from nd~not-free-in-terms-p nil))))
 

(defun nd~change-top-var-quantor (var formula)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A variable VAR and a FORMULA, which must be a quantified
formula whose first argument is an abstraction.  VAR should not appear free
in FORMULA.")
	   (effect  "None.")
	   (value   "A new quantified formula just like FORMULA, but where
the bound variable is changed to VAR."))
  (let ((quantor (appl~function formula))
	(arg (car (appl~arguments formula))))
    (if (abstr~p arg)
	(termc~quantification-create 
	 quantor var 
	 (hop~beta-contract (appl~create arg (list var))))
      (error "Can't apply nd~~change-top-var-quantor because the first 
argument is not a lambda abstraction."))))
    
(defun nd~not-atom-p (formula)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A FORMULA.")
	   (effect  "None.")
	   (value   "T if formula is not an atom, otherwise NIL."))
  (not (termc~atom-p formula)))


(defun nd~pushneg (formula &optional 
			   (env
			    (let ((newenv (env~create)))
			      (post~read-object-list 
			       '((type-constants o)
				 (type-variables aa)
				 (constants
				  (and (o o o))
				  (or (o o o))
				  (implies (o o o))
				  (equiv (o o o))
				  (not (o o))
				  (forall (o (o aa)))
				  (exists (o (o aa))))) newenv)
			      newenv)))
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A FORMULA and an environment.")
	   (effect  "None.")
	   (value   "If formula is negated, tries to push the negation through
a quantifier or connective, returning a new formula. Otherwise returns the
original formula."))
  (when (env~empty-p env)
    (warn "nd~~pushneg is called with an empty environment! consider using the optional argument ENV")) 
  (unless (termc~negation-p formula)
    (return-from nd~pushneg formula))
  (let* ((the-not (appl~function formula))
	 (scope (car (appl~arguments formula))))
    (cond ((or (termc~existential-quantification-p scope)
	       (termc~universal-quantification-p scope))
	   (let ((quant (appl~function scope))
		 (quant-scope (car (appl~arguments scope))))
	     (if (abstr~p quant-scope)
		 (appl~poly-create
		  (if (termc~existential-quantor-p quant)
		      (term~env-lookup 'forall env)
		      (term~env-lookup 'exists env))
		  (list (abstr~create
			 (termc~quantification-bound-variable scope)
			 (appl~create 
			  the-not 
			  (list (termc~quantification-scope scope))))))
		 formula)))
	  ((termc~negation-p scope)
	   (car (appl~arguments scope)))
	  ((termc~conjunction-p scope)
	   (appl~create 
	    (env~lookup-object 'or env)
	    (list (appl~create the-not (list (car (appl~arguments scope))))
		  (appl~create the-not (list (cadr (appl~arguments scope)))))))
	  ((termc~disjunction-p scope)
	   (appl~create 
	    (env~lookup-object 'and env)
	    (list (appl~create the-not (list (car (appl~arguments scope))))
		  (appl~create the-not (list (cadr (appl~arguments scope)))))))
	  ((termc~implication-p scope)
	   (appl~create 
	    (env~lookup-object 'and env)
	    (list (car (appl~arguments scope))
		  (appl~create the-not (list (cadr (appl~arguments scope)))))))
	  (t formula))))

(defun nd~replace-and-contract 
    (term replacement in-term 
     &optional
     (num
      (do ((num 0 (1+ num))
	   (replacement replacement
		    (abstr~scope 
		     replacement)))
	  ((not (abstr~p replacement)) num))))
  (declare
   (authors nesmith)
   (input "A TERM to be replaced, its REPLACEMENT, and the IN-TERM in which the
replacement is to occur.  Optional argument NUM is an integer, defaults to
the number of consecutive lambdas in REPLACEMENT, if it is lambda-bound.")
   (value "A term like IN-TERM, but where TERM has been replaced by REPLACEMENT
and if REPLACEMENT is lambda-bound, each replacement has then been NUM times
beta-contracted (or fewer if there are fewer than NUM arguments where the
replacement was made.  Parts of the formula that have no replacements made are
not changed, otherwise a brand new formula will be created."))
  (cond 
   ((sym~p in-term) 
    (if (term~equal-p term in-term)
	(values replacement t)
      (values in-term nil)))
   ((abstr~p in-term)
    (multiple-value-bind (newterm changed-p)
	(nd~replace-and-contract term replacement
				 (abstr~scope in-term)
				 num)
      (if changed-p
	  (values (abstr~create
		   (abstr~bound-variable in-term)
		   newterm)
		  t)
	(values in-term nil))))
   ((appl~p in-term)
    (let ((newargs nil)
	  (changed-p nil)
	  (newfun nil))
      (multiple-value-setq (newfun changed-p)
	(nd~replace-and-contract term replacement 
				 (appl~function in-term)
				 num))
      (dolist (arg (appl~arguments in-term)
		(if (eq replacement newfun)
		    (let ((newappl
			    (appl~create newfun 
						     (nreverse newargs))))
		     (dotimes (i (min num (length 
					   (appl~arguments
					    newappl)))
				(values newappl t))
		       (setq newappl (hop~beta-contract newappl))))
		  (if changed-p
		      (values (appl~create 
			       newfun
			       (nreverse newargs))
			      t)
		    (values in-term nil))))
	(multiple-value-bind (newarg changed-p*)
	    (nd~replace-and-contract term replacement arg num)
	  (setq changed-p (or changed-p changed-p*))
	  (push newarg newargs)))))
   (t in-term)))


(defun nd~make-problem-from-line (proof line)
  (declare (edited  "20-APR-1993 11:58" )
	   (authors KOHLHASE )
	   (input   "A ND proof and a line in it.")
	   (effect  "None.")
	   (value   "A new problem is made from the line, where the assumptions will be the formulas of"
		    "the supporting lines of LINE, the conclusion will be the formula of LINE, and"
		    "the environment will be that of PROBLEM."))
  (prob~create (read-from-string (format nil "~A-PROBLEM" (keim~name line)))
	       'initialized
	       (prob~environment proof)
	       (mapcar #'(lambda (supporting-line)
			   (assum~create
			    (read-from-string (format nil "ASS-~A" (keim~name supporting-line)))
			    (node~formula supporting-line)))
		       (nd~line-supports line))
	       (list (conc~create (intern (format nil "CONC-~A" (keim~name line)))
					     (node~formula line)))))



#{
\subsection{Printing of Proofs} 
There is a lot devoted here to printing.  This really doesn't belong here,
but it is planned to be replaced anyway by some more general facilities
which will use the new Common Lisp pretty printer. 
These functions allow use the standard output as the output stream.

This section is now basically obsolete, and is used only for the post~print
function.  Better is the stuff defined in the next subsection with
{\vb nd~pprint-proof}.

\subsubsection{Output Styles}

An output style is actually a CLOS class which is merely used in defining
methods.  Because of the inheritance properties of CLOS, we can easily get
a hierarchy of styles.
#}


(eval-when (load compile eval) 
(defclass nd+output-style (keim+object)
  ((name :reader nd~output-style-name :initarg :name
	 :documentation "Name of the output style.")
   (description :reader nd~output-style-description :initarg :description
		:documentation "Description of the output style."))
  (:documentation "A style of outputting natural deductions.")))

(defvar nd*current-output-style nil "Current output style.")

(defvar nd*print-ellipsis nil "If true, print an ellipsis before each
planned line.")


(defgeneric nd~print-subproof (line style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "A line and an output style.")
           (effect  "Print this line and its supports.")
           (value   "Undefined."))
 (:method ((line nd+line) style)
   (let ((nd*print-ellipsis nil))
     (dolist (supp (nd~line-supports line))
       (nd~print-line supp style)))
   (let ((nd*print-ellipsis t))
     (nd~print-line line style))))


(defgeneric nd~print-line (line style)
 (declare (edited  "21-MAY-1992 14:20")
	  (authors NESMITH)
           (input   "An nd+line and an nd+output-style.")
           (effect  "Line is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction LINE using STYLE to standard-output."))

(defun nd~print-line-label (line)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A line.")
	   (effect  "The line's label is princ'd.")
	   (value   "Undefined."))
  (princ (nd~line-label line)))

(defun nd~print-line-list-label (line-list)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A list of lines.")
	   (effect  "The lines' labels are princ'd in a list form.")
	   (value   "Undefined."))
  (princ "(")
  (dolist (line line-list (princ ")"))
    (nd~print-line-label line) (princ " ")))

(defvar nd*simple-output-style nil
  "We just want an instance of this class to be able to use as an
argument to various printing functions.")

(defun nd~show-line (line &optional (style (or nd*current-output-style
					       nd*simple-output-style)))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "An nd line and an optional style.")
           (effect  "Print the line in the style provides; style defaults to
nd*current-output-style, or to nd*simple-output-style.")
           (value   "Undefined"))
  (nd~print-line line style))

(defgeneric nd~print-label (line style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+line and an nd+output-style.")
           (effect  "Line's label is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction LINE's label using STYLE to standard-output."))

(defgeneric nd~print-hyps (line style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+line and an nd+output-style.")
           (effect  "Line's hypotheses are printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction LINE's hypotheses using STYLE to standard-output."))

(defgeneric nd~print-turnstile (style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+output-style.")
           (effect  "Turnstile for a proof line is printed using style to
standard-output.")
           (value   "Undefined."))
 (:documentation "Print a turnstile using STYLE to standard-output."))

(defgeneric nd~print-formula (line style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+line and an nd+output-style.")
           (effect  "Line's formula is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction LINE's formula using STYLE to standard-output."))

(defgeneric nd~print-justification (line style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+line and an nd+output-style.")
           (effect  "Line's label is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction LINE's label using STYLE to standard-output."))

(defgeneric nd~print-space (style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+output-style.")
           (effect  "A space is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a space using STYLE to standard-output."))

(defgeneric nd~print-newline (style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd+output-style.")
           (effect  "A newline is printed using style to standard-output.")
           (value   "Undefined."))
 (:documentation "Print a newline using STYLE to standard-output."))

(defgeneric nd~print-proof (proof style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An natural deduction proof object and an output style.")
           (effect  "The proof is printed using the style to standard
output.")
           (value   "Undefined."))
 (:documentation "Print a natural deduction PROOF using STYLE to
standard output.")
  )

(defun nd~show-proof (&optional (proof nd*current-proof)
				(style (or nd*current-output-style
					   nd*simple-output-style)))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "An optional nd-proof and optional style.")
           (effect  "Print the proof in the style provided; PROOF defaults
to value of ND*CURRENT-PROOF and STYLE defaults to ND*CURRENT-OUTPUT-STYLE,
or to ND*SIMPLE-OUTPUT-STYLE.")
           (value   "Undefined"))
  (nd~print-proof proof style))

(defun nd~show-subproof (&optional (line 
				    (car (nd~proof-planned-lines 
					  nd*current-proof)))
				   (style (or nd*current-output-style
					      nd*simple-output-style)))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "An optional line and an optional style.")
           (effect  "Print the subproof that is represented by this line (the
line and its supports.  LINE defaults to the first planned line of the current
proof; STYLE defaults to nd*current-output-style, or to nd*simple-output-style.")
           (value   "Undefined"))
  (when line (nd~print-subproof line style)))


(defgeneric nd~print-line-just (just style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An nd-justification and an output style.")
           (effect  "The justification is printed using the style to standard
output.")
           (value   "Undefined."))
 )


(defun nd~print-list-of-things-in-style (list indiv-print-function style)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A LIST of things, a print function and a STYLE.")
	   (effect  "Each of the items in the list is printed using the STYLE 
given, with parens put around the whole output.")
	   (value   "Undefined."))
  (princ "(")
  (do* ((things list (cdr things))
	(thing (car things) (car things)))
      ((null thing) (princ ")"))
    (funcall indiv-print-function thing style)
    (unless (null (cdr things)) (princ " "))))



(defgeneric nd~print-ellipsis (style)
 (declare (edited  "21 May")
           (authors nesmith)
           (input   "An output style.")
           (effect  "An ellipsis ... is printed.")
           (value   "Undefined."))
  )



#{\subsubsection{A Simple Output Style } 
Here is a very simple output style.  It is, you might say, the worst of
all possible worlds.  It can't be reread, nor does it use any reasonable
pretty-printing. 
#}

(eval-when (load compile eval) 
(defclass nd+simple-output-style (nd+output-style) nil))

(setq nd*simple-output-style
  (make-instance 'nd+simple-output-style
    :name 'nd*simple-output-style
    :description "Very simple output style, for output to a terminal."))



(defmethod nd~print-label ((line nd+line) 
			   (style nd+simple-output-style))
  (princ (nd~line-label line)))

(defmethod nd~print-hyps ((line nd+line) 
			  (style nd+simple-output-style))
  (nd~print-list-of-things-in-style (nd~line-hyps line) 
				    #'nd~print-label style))

(defmethod nd~print-turnstile ((style nd+simple-output-style))
  (princ "!"))

(defmethod nd~print-formula ((line nd+line) 
			     (style nd+simple-output-style))
  (nd~print-wff (node~formula line))
  )


(defmethod nd~print-ellipsis ((style nd+simple-output-style))
  (princ "     ...") 
  (terpri))

(defmethod nd~print-justification ((line nd+line) 
				   (style nd+simple-output-style))
  (nd~print-line-just (node~justification line) style))


(defmethod nd~print-line-just ((just nd+justification) 
			       (style nd+simple-output-style))
  (let* ((rule (nd~just-rule just))
	 (terms (nd~just-terms just))
	 (lines (nd~just-lines just)))
    (princ rule)
    (when terms
      (princ " ")
      (nd~print-list-of-things-in-style 
       terms #'(lambda (x y) (declare (ignore y))
		       (nd~print-wff x))
       style))
    (when lines
      (princ " ")
      (nd~print-list-of-things-in-style lines #'nd~print-label style))))


(defmethod nd~print-space ((style nd+simple-output-style))
  (princ " "))

(defmethod nd~print-newline ((style nd+simple-output-style))
  (terpri))


(defmethod nd~print-line ((line nd+line) 
			  (style nd+simple-output-style))
  (when (and nd*print-ellipsis (nd~plan-p line)) (nd~print-ellipsis style))
  (nd~print-label line style)
  (nd~print-space style)
  (nd~print-hyps line style)
  (nd~print-space style)
  (nd~print-turnstile style)
  (nd~print-space style)
  (nd~print-formula line style)
  (nd~print-space style)
  (nd~print-justification line style)
  (nd~print-newline style)
)


(defmethod nd~print-proof ((proof nd+proof) 
			   (style nd+simple-output-style))
  (let ((nd*print-ellipsis t))
    (dolist (line (nd~proof-lines proof))
      (nd~print-line line style))))

#{\subsubsection{A Rereadable Output Style } 
This output style is useful for printing proofs in a format that can
be parsed in again.  This is currently used by the POST~PRINT
method for natural deduction proofs.
#}

(eval-when (load compile eval) 
(defclass nd+readable-output-style (nd+simple-output-style) nil
  (:documentation "An output style intended to be used when the output must
be later re-read.  An instance of this style is nd*readable-output-style.")))

(defvar nd*readable-output-style nil
    "We just want an instance of this class to be able to use as an
argument to various printing functions.")  

(setq nd*readable-output-style  
  (make-instance 'nd+readable-output-style))

(defun nd=print-symbol-declaration (sym env)
  (declare
   (authors nesmith)
   (input "A Lisp symbol and an environment.")
   (effect "The symbol and its type are printed as in a POST declaration."))
  (let* ((term (term~env-lookup sym env))
	 (type (term~type term)))
    (when term
      (format t "(~A " sym)
      (nd~print-type type)
      (format t ")~%"))))

(defmethod post~read-object ((obj list) (env env+environment) 
			     (key (eql :nd~def-nd-proof)))
  (eval (cons 'nd~def-nd-proof obj)))

(defmethod post~print ((proof nd+proof) stream)
  (let ((*standard-output* stream))
    (nd~print-proof proof nd*readable-output-style)))

(defmethod post~print ((line nd+line) stream)
  (format stream 
	  "(~A (~{~A ~}) " 
	  (nd~line-label line) 
	  (mapcar #'nd~line-label (nd~line-hyps line)))
  (post~print (node~formula line) stream)
  (format stream " (~S (" (nd~line-just-rule line))
  (mapc #'(lambda (x) (post~print x stream))
	(nd~line-just-terms line))
  (format stream ") (~{~A ~}))" 
	  (mapcar #'nd~line-label (nd~line-just-lines line)))
    (format stream " (~{~A ~}))" 
	  (mapcar #'nd~line-label (nd~line-supports line))))


(defmethod nd~print-proof ((proof nd+proof) (style nd+readable-output-style))
  (format t "~%(nd~~def-nd-proof ~A " (keim~name proof))
  (format t "~%(declarations ~%")
  (let* ((env (prob~environment proof))
	 (type-vars (env~class-keys env 'type+variable))
	 (type-constants (env~class-keys env 'type+constant))
	 (constants (env~class-keys env 'sym+const))
	 (variables (env~class-keys env 'sym+var)))
    (when type-vars (format t "(type-variables ~{~A ~})~%" type-vars))
    (when type-constants 
      (format t "(type-constants ~{~A ~})~%" type-constants))
    (when constants 
      (format t "(constants ~%")
      (mapc #'(lambda (x) (nd=print-symbol-declaration x env)) constants)
      (format t ")~%"))
    (when variables 
      (format t "(variables ~%")
      (mapc #'(lambda (x) (nd=print-symbol-declaration x env)) variables)
      (format t ")~%")))
  (format t ")~%")
  (princ "(assumptions")
  (dolist (hyp (prob~assumptions proof) (princ ")"))
    (format t " ")
    ;; (post~print hyp *standard-output*)
    (format *standard-output* "(~A " (keim~name hyp))
    (post~print (assum~formula hyp) *standard-output*)
    (princ ")"))
  (terpri)
  (princ "(conclusion ")
  (format *standard-output* "~A " (keim~name (prob~conclusion proof))) 
  (post~print (conc~formula (prob~conclusion proof))
	      *standard-output*)
  (princ ")")
  (terpri)
  (princ "(plans (")
  (dolist (plan (nd~proof-planned-lines proof) (princ "))"))
    (nd~print-label plan nd*readable-output-style) (princ " "))
  (terpri)
  (princ "(lines ")
  (dolist (line (nd~proof-lines proof) (princ ")"))
    (terpri)
    (nd~print-line line nd*readable-output-style))
  (princ ")") (terpri))
  

(defmethod nd~print-line ((line nd+line) (style nd+readable-output-style))
  (post~print line *standard-output*)
  )

(defmethod nd~print-formula ((line nd+line) 
			     (style nd+readable-output-style))
  (nd~print-wff (node~formula line))
  )

(defmethod nd~print-supports ((line nd+line) (style nd+readable-output-style))
  (nd~print-list-of-things-in-style 
   (nd~line-supports line)
   #'(lambda (x y) (declare (ignore y))
			    (princ (nd~line-label x)))
   style))


(defmethod nd~print-ellipsis ((style nd+readable-output-style))
  )

(defmethod nd~print-justification ((line nd+line) 
				   (style nd+readable-output-style))
  (let* ((just (node~justification line))
	 (rule (just~rule just))
	 (terms (nd~just-terms just))
	 (lines (nd~just-lines just)))
    (princ "(")
    (prin1 rule)
    (princ " ")
      (nd~print-list-of-things-in-style 
       terms 
       #'(lambda (x y) (declare (ignore y)) (princ x))
       style)
      (princ " ")
      (nd~print-list-of-things-in-style lines #'nd~print-label style))
    (princ ")"))


#{
\subsubsection{Printing Terms}
This is a very simple interface for formula printing.  It should be replaced
Real Soon Now by a more general mechanism using the new pprint facilities
of Common Lisp.  For that reason, we have isolated it in this module.
#}




(eval-when (load compile eval)
(defgeneric nd~print-wff (wff)
  (declare (edited  "5-OCT-92 10:00")
           (authors NESMITH)
           (input   "A term.")
           (effect  "Print the term to standard output.")
           (value   "Undefined."))
  (:documentation "Print a wff to standard output.")))


(defmethod nd~print-wff ((wff term+term))
  (if (and (slot-boundp wff 'binding)
	     (term~term-binding wff))
	(princ (term~term-binding wff))
      (if (and (slot-boundp wff 'label)
	     (term~label wff))
	  (princ (term~label wff))
	(error ";;; nd~~print-wff: don't know how to print ~S" wff))))




(defmethod nd~print-wff ((wff sym+var))
  (if (and (slot-boundp wff 'binding)
	   (term~binding wff))
      (princ (term~binding wff))
    (if (and (slot-boundp wff 'symbol)
	     (sym~name wff))
	  (princ (sym~name wff))
      (call-next-method))))

(defmethod nd~print-wff ((wff appl+appl))
  (princ "(")
  (nd~print-wff (appl~function wff))
  (mapc #'(lambda (x) (princ " ") (nd~print-wff x))
	(appl~arguments wff))
  (princ ")"))


(defmethod nd~print-wff ((wff abstr+abstr))
  (princ "(lam ")
  (princ "(")
  (nd~print-wff (abstr~bound-variable wff))
  (princ " ")
  (nd~print-type (term~type (abstr~bound-variable wff)))
  (princ ")")
  (nd~print-wff (abstr~scope wff))
  (princ ")")
  )

(defun nd~print-type (type)
  (declare (edited  "5-OCT-92 10:00")
           (authors NESMITH)
           (input   "A type.")
           (effect  "Print the type to standard output.")
           (value   "Undefined."))
  (typecase type
    (type+primitive 
     (print-object type *standard-output*))
    (type+complex
     (format t "(")
     (nd~print-type (type~n-range type))
     (mapc #'(lambda (x) (princ " ") (nd~print-type x))
	   (reverse (type~n-domain type)))
     (format t ")"))))

(defmethod nd~print-wff ((wff sym+const))
  (if (and (slot-boundp wff 'binding)
	   (term~binding wff))
      (princ (term~binding wff))
    (if (and (slot-boundp wff 'symbol)
	     (sym~name wff))
	  (princ (sym~name wff))
      (call-next-method))))

(defmethod nd~print-wff ((wff symbol))
  (princ wff)
  )

#{\subsection{Proof pretty-printing}
Here we define functions for pretty-printing of proofs. This uses the
PP module to set up some new styles.
#}

(defvar nd*label-width 10 "default width for line labels in output")
(defvar nd*hyps-width 10 "default width for hypotheses in output")
(defvar nd*formula-width 40 "default width for formulas in output")
(defvar nd*just-width 15 "default width for justifications in output")

(defvar nd*print-entire-line t "True if we want to print an entire
line, nil if we just want to print its label.")

(defun nd=pprint-just (just)
  (let ((rule (nd~just-rule just))
	(terms (nd~just-terms just))
	(lines (nd~just-lines just)))
    (pprint-logical-block (nil nil)
      (write rule :escape nil)
      (if (or lines terms) (write-char #\:))
      (pprint-logical-block (nil terms)
        (pprint-exit-if-list-exhausted)
	(write-char #\space)
	(write-char #\()
	(unwind-protect 
	(loop (write (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\space)
	      (pprint-newline :tabular))
	(write-char #\) )))
      (pprint-logical-block (nil lines)
        (pprint-exit-if-list-exhausted)
	(write-char #\space)
	(write-char #\( )
	(let ((nd*print-entire-line nil))
	(unwind-protect 
	    (loop (write (pprint-pop) :gensym nil :escape nil)
		  (pprint-exit-if-list-exhausted)
		  (write-char #\space)
		  (pprint-newline :linear))
	(write-char #\) )))))))



#+old(defun nd=pprint-line (line)
  (let ((label (nd~line-label line))
	(hyps (nd~line-hyps line))
	(formula (node~formula line))
	(just (node~justification line)))
    (if nd*print-entire-line
	(progn
	  (terpri)
	  (pprint-logical-block
	   (nil nil)
	   (write label)
	   (write-char #\space)
	   (pprint-newline :miser)
	   (pprint-indent :current 2)
	   (let ((nd*print-entire-line nil))
	     (pprint-fill nil hyps))
	   (pprint-indent :current 2)
	   (pprint-logical-block (nil nil :prefix " ")
				 (write formula))
	   (pprint-logical-block (nil nil)
				 (pp~pprint just))))
      (write label :escape nil))
    ))
    

(defun nd=pprint-line (line)
  (if nd*print-entire-line
      (progn
	(when (and nd*print-ellipsis
		   (nd~plan-p line))
	  (format t "               ...~%"))
	(pp~pprint-table 
	 (symbol-name (nd~line-label line))
	 (list nd*label-width :l "" " ")
	 (mapcar #'symbol-name
		 (mapcar #'nd~line-label (nd~line-hyps line)))
	 (list nd*hyps-width :l nil nil  
	       #'(lambda (s o)
		   (let ((*print-escape* nil)
			 (*print-miser-width* nil))
		     (format s "~%~:/pprint-fill/" o)
		     )))
	 " ! " '(3 :c)
	 (node~formula line) 
	 (list nd*formula-width :l)
	 (node~justification line) 
	 (list nd*just-width :r " ")))
      (write (nd~line-label line))))

(defgeneric nd=pprint-type (type)
  (:method ((type type+primitive))
    (write (keim~name type) :escape nil))
  (:method ((type type+complex))
    (write-char #\()
    (write (type~n-range type))
    (pprint-logical-block (nil (reverse (type~n-domain type)))
	(loop (pprint-exit-if-list-exhausted)
	      (write-char #\space)
	      (write (pprint-pop))))
    (write-char #\))
    ))

(defvar nd*printing-bound-var nil "True if we are printing a bound variable,
nil otherwise.")

(defgeneric nd=pprint-term (term)
  (:method ((term sym+sym))
    (write (keim~name term) :escape nil)
    (when nd*printing-bound-var
      (write-char #\:)
      (write (term~type term))))
  (:method ((term appl+appl))
	   (pprint-indent :current 0)
	   (pprint-logical-block (nil nil)
	     (pprint-fill nil
			  (cons (appl~function term)
				(copy-list (appl~arguments 
					    term))))))
  (:method ((term abstr+abstr))
   (pprint-logical-block (nil nil :prefix "(" :suffix ")")
     (write-char #\[)
     #+comment(nd=pprint-term (abstr~bound-variable term))
     (write (abstr~bound-variable term))
     (write-char #\])
     (write-char #\.)
     (pprint-newline :fill)
     (pprint-indent :block 1)
     (write (abstr~scope term)))))
    
(defvar nd*quantifier-names (list "FORALL" "EXISTS") "Names of quantifiers")

(defun nd=quantified-term-p (appl)
  (declare
   (authors nesmith)
   (input "An application.")
   (value "T if the application with one argument, which is an abstraction,
and the function's name is on the list nd*quantifier-names"))
  (and (sym~p (appl~function appl))
       (let ((args (appl~arguments appl)))
	 (and (= 1 (length args))
	      (abstr~p (car args))))
       (member (keim~name (appl~function appl))
	       nd*quantifier-names :test #'string-equal)))


(deftype nd+quantified-term ()
  '(and appl+appl (satisfies nd=quantified-term-p)))



(defun nd=pprint-quantified-term (term)
  (let ((quantor (appl~function term))
	(args (appl~arguments term)))
    (let ((arg (car args)))
	(pprint-indent :current 2)
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
	  (write quantor)
	(write-char #\space)
	 (write-char #\[)
	(loop
	 (let ((var (abstr~bound-variable arg))
	       (scope (abstr~scope arg)))
	   (let ((nd*printing-bound-var t))
	     #+comment(nd=pprint-term var)
	     (write var)
	     )
	   (if (and (nd=quantified-term-p scope)
		    (term~equal quantor (appl~function scope)))
	       (progn
		 (write-char #\,)
		 (setq arg (car (appl~arguments scope))))
	     (progn
	       (write-char #\])
	       (write-char #\space)
	       (pprint-newline :fill)
	       (write scope)
	       (return)))))))))


(defun nd=pprint-proof (proof)
  (let ((nd*print-entire-line t)
	(lines (nd~proof-lines proof)))
    (multiple-value-bind (nd*label-width nd*hyps-width 
			  nd*formula-width nd*just-width)
	(nd=figure-margins lines)
      (pprint-logical-block (nil lines)
       (pprint-exit-if-list-exhausted)
       (loop (write (pprint-pop))
	       (pprint-exit-if-list-exhausted))))))

(defun nd~pprint-proof (proof style)
  (declare
   (authors nesmith)
   (input "An nd+proof PROOF, and a pp+style STYLE.")
   (effect "The proof will be printed in the style to *standard-output*."))
  (unless (pp~find-style style)
    (setq style 'nd-simple))
  (pp~pprint proof style)
  #+comment(pp~with-style style
    (pp~pprint proof))
  )

(defun nd=figure-margins (lines)
  (declare
   (authors nesmith)
   (input "A list of nd lines")
   (value "Tries to figure out the proper width of the various fields in
printing. Four values: the width of the labels field, the width of the
hypotheses field, the width of the formula field and the width of the
justification field. We always give the formulas at least 50% of the
entire line."))
  (let ((total-width (- (or *print-right-margin* 79) 5))
	(max-label 
	 (apply #'max 
		(mapcar #'length 
			(mapcar #'symbol-name 
				(mapcar #'keim~name lines)))))
	(max-just 
	 (1+ 
	  (apply #'max 
		 (mapcar #'length 
			 (mapcar #'(lambda (l)
				     (format nil "~A: ~S"
					     (nd~line-just-rule l)
					     (mapcar #'keim~name
						     (nd~line-just-lines l))))
				 lines)))))
	(max-hyps (apply #'max 
			 (mapcar #'length 
				 (mapcar #'(lambda (l) 
					     (princ-to-string
					      (mapcar #'keim~name 
						      (nd~line-hyps l))))
					 lines)))))
    (if (< (+ max-label max-just max-hyps) (/ total-width 2))
	(values max-label max-hyps (- total-width (+ max-label max-hyps
						     max-just ))
		max-just)
      (values max-label (- (/ total-width 2) (+ max-label max-just))
	      (/ total-width 2) max-just))))


#{We set up three different styles for use with {\vb nd~pprint-proof}.

The first is {\vb ND-SIMPLE}, it merely prints the lines in a relatively
readable form (but better than {\vb post~print}).

The next is {\vb ND-PRETTY}.  This style is like {\vb ND-SIMPLE}, but
prints ellipses just before planned lines, and for quantified formulas
which are defined as those applications of the right type whose function
looks like a EXISTS or an FORALL, it prints no lambda and tries to
compress multiple quantifiers into a single one. 

The last style is {\vb ND-POST}.  This corresponds to the style printed by
{\vb post~print}. 
#}

(eval-when (load eval compile)
(pp~defstyle nd-simple
	     :help "A simple style for printing nd-proofs."
	     :pprint-methods
	     ((term+term (lambda (s l)
			     (let ((*standard-output* s))
			       (nd=pprint-term l)))
			 )
              (just+justification
	       (lambda (s l)
		   (let ((*standard-output* s))
		     (nd=pprint-just l))))
	      (nd+proof 
	       (lambda (s l)
		   (let ((*standard-output* s)) 
		     (nd=pprint-proof l))))
	      (nd+line
	       (lambda (s l)
		   (let ((*standard-output* s)) 
		     (nd=pprint-line l))))
	      (type+type
	       (lambda (s l)
		   (let ((*standard-output* s)) 
		     (nd=pprint-type l))))
	      ))
)

(pp~defstyle nd-pretty :parent nd-simple
	     :pprint-methods
	     ((nd+quantified-term
	       (lambda (s l)
		   (let ((*standard-output* s))
		     (nd=pprint-quantified-term l)))
	       (1+ (length (sys~class-precedence-list
			    (find-class 'appl+appl)))))
	      (nd+proof 
	       (lambda (s l)
		   (let ((*standard-output* s)
			 (nd*print-ellipsis t)) 
		     (nd=pprint-proof l))))))

(pp~defstyle 
 nd-post :parent pp+top
 :pprint-methods
 (;(symbol (lambda (s sym) (write-string (symbol-name sym) :stream s)))
  (term+term
   (lambda (s term) (post~print term s)))
 (nd+proof 	   
  (lambda (stream proof)
    (let ((*standard-output* stream))
      (nd~print-proof proof nd*readable-output-style))))
 (nd+line
  (lambda (stream line)
    (let ((*standard-output* stream))
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
    (write (nd~line-label line))
    (write-char #\space)
    (pprint-logical-block (nil (mapcar #'nd~line-label (nd~line-hyps line))
			       :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
     (loop (write-char #\space)
	   (write (pprint-pop))
	   (pprint-exit-if-list-exhausted)))
    (write-char #\space)
    (write (node~formula line))
    (write-char #\space)
    (pprint-logical-block (nil nil :prefix "(" :suffix ")")
    (write  (string (nd~line-just-rule line)) :escape t :pretty nil)
    (write-char #\space)
    (pprint-logical-block (nil (nd~line-just-terms line)
			       :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
      (loop (write-char #\space)
	    (write (pprint-pop))
	    (pprint-exit-if-list-exhausted)))
    (write-char #\space)
    (pprint-logical-block (nil (mapcar #'nd~line-label (nd~line-just-lines line))
			       :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
     (loop (write-char #\space)
	   (write (pprint-pop))
	   (pprint-exit-if-list-exhausted))))
    (write-char #\space)
    (pprint-logical-block (nil (mapcar #'nd~line-label (nd~line-supports line))
			       :prefix "(" :suffix ")")
      (pprint-exit-if-list-exhausted)
     (loop (write-char #\space)
	   (write (pprint-pop))
	   (pprint-exit-if-list-exhausted)))
    ))))))

(defmethod post~print ((line nd+line) stream)
  (let ((*standard-output* stream))
    (pp~pprint line 'nd-post)))

(defmethod post~print ((proof nd+proof) stream)
  (let ((*standard-output* stream))
    (pp~pprint proof 'nd-post)))
    
    
#|(in-package :omega)

(defun oc=show-proof (&optional (omega*current-ndproof omega*current-ndproof))
  (if (nd~proof-p omega*current-ndproof)
      (let ((string (pp~pprint-to-string omega*current-ndproof 'nd-pretty)))
	(inter~output-object omega*current-interface string))
      (arg~signal-wrong-type 'ndproof omega*current-ndproof)))

(defun oc=show-subproof (plan)
  (let ((string (with-output-to-string (*standard-output*)
		  (let ((lines (append (nd~line-supports plan) (list plan))))
		    (oc=show-proof
		     (make-instance 'nd+proof :lines lines :name (gensym)))
		    ))))
    (inter~output-object omega*current-interface string))
  )
|#

