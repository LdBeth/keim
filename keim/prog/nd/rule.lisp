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

(in-package :keim)

(mod~defmod rule :uses (env hop just keim meta mod nd node post sym sys term )
	    :documentation "Natural deduction rule definition and use."
	    :exports (
		      rule+meta-line
		      rule+var-just
		      rule~rule-p
		      rule+rule
		      rule~args
		      rule~set-args!
		      rule~name
		      rule~set-name!
		      rule~argtypes
		      rule~set-argtypes!
		      rule~preconds
		      rule~set-preconds!
		      rule~postconds
		      rule~set-postconds!
		      rule~sideconds
		      rule~set-sideconds!
		      rule~actions
		      rule~set-actions!
		      rule~find-rule
		      rule~match-p
		      rule~def-rule
		      rule~describe
		      rule~apply
		      )
	    )

#{\section{Natural Deduction Rules} 
\label{mod:rule}

In this module, we define natural deduction rules.  Natural deduction rules
are like production rules.  They do a match on some set of lines in
a natural deduction proof, and if the match succeeds (and possibly some
other sideconditions are satisfied), carry out some action.  This action
could be to modify some lines in the proof or create new lines. 

Our rules can be thought of as having a ``left-hand'' and ``right-hand'' sides.
The left-hand side is a set of lines which must be present in the proof, and
must be matched in order for the rule to be carried out.  The right-hand
side describes new lines that will be added to the proof when the the rule
is carried out, or describes how the justifications of existing lines
(from the left-hand side) are changed.  Part of the matching procedure
may be in testing sideconditions (such as that a certain variable is
does not occur free in particular lines).

The left-hand side will be called the preconditions, the right-hand side
the postconditions, and the sideconditions, well, they're called the
sideconditions. 

We can also think of our rules as defining functions, which given certain
arguments of specified types, either produce the postconditions or return
an error.  For this reason, we include information about the arguments
(lines and terms) in the rule definition.

To round things out, we allow our rules to make changes in the
support structure of the proof, that is, to change the lines that
may be useful for proving a particular line.  For example, when a 
rule being applied means that a certain line is no longer useful, it
can then be removed as a support from other still-planned lines in the
proof.

Note: many of the ideas contained here are based on the natural deduction
rules of Andrews's TPS system.
#}

#|
rule~argtypes (rulename):  Given the symbol RULENAME, get the list of 
  argument types this rule expects.  As a theoretical example:
  (rule~argtypes 'mp) => (nd+line nd+line)

rule~describe (rulename): Describe the rule named by RULENAME.  No
  other effects.

rule~apply (rulename arg-list): RULENAME is a symbol naming the rule,
  ARG-LIST is a list of lines, formulas, whatever the rule expects, and
  types of the arguments must match what is returned by 
  (rule~argtypes RULENAME).
  Returns two values if rule was successfully carried out, T and
  a list of new and modified lines, or NIL if it failed for some
  reason (with informative output). 

rule~match-p (rulename arg-list): Arguments same as in rule~apply.
  This, however, only checks to see if the rule is applicable (the arguments
  match what the rule expects), and returns T or NIL based on this check. 
|#

#{\subsection{Meta-lines}
Our rules, in order to be general, are schematic.  That is, they describe
patterns in the input lines.  For this reason, we need what we might
call ``meta-lines'', which have metavariables for their
sets of hypotheses, as well as meta-justifications. 
#}

(eval-when (load compile eval)

(defclass rule+meta-line (nd+line)
  ((hyps :initform nil)
   (hypset :initform nil :initarg :hypset :accessor rule=hypset))
  (:documentation "An nd+line with possibly some variables for attributes."))

(defclass rule+var-just (nd+justification)
   ()
  (:default-initargs :terms nil)
  (:documentation "An nd+justification that can stand for any justification."))


)

(defun rule~rule-p (thing)
  (declare (authors nesmith)
	   (input "Anything")
	   (value "T if the argument is a rule+rule, otherwise NIL."))
  (typep thing 'rule+rule))


#|
A meta-line may have a variable standing for a set of hypotheses. If so,
we first print the variable, followed by a comma, then the remaining
hypotheses.
|#

(defmethod nd~print-hyps ((line rule+meta-line) style)
  (when (rule=hypset line)
    (princ (rule=hypset line))
    (princ ","))
  (call-next-method))

#|Don't print anything for meta-justification. |#

(defmethod nd~print-line-just ((just rule+var-just) style)
  (declare (ignore style))
  nil
  )  	  

#{\subsection{Rules}
Rules are instances of the class RULE+RULE, which has many slots, as seen
by the access functions below.  Each rule has a name (case insensitive). 
The arguments the rule expects, along with their Lisp types (for error 
checking), and a help string for each argument are also stored.
Below we'll show the syntax of a rule definition.
#}

(eval-when (load compile eval) 
(defclass rule+rule (keim+name)
  ((args :reader rule=args
	 :writer rule=set-args!
	 :initarg :args
	 :documentation "The arguments that this rule requires.")
   (argtypes :reader rule=argtypes
	     :writer rule=set-argtypes!
	     :initarg :argtypes
	     :documentation "The argument types that this rule requires (corresponding with the arguments.")
   (arghelps :reader rule=helps
	     :writer rule=set-arghelps!
	     :initarg :arghelps
	     :documentation "A list of help strings corresponding to args.")
   (preconds :reader rule=preconds
	     :writer rule=set-preconds!
	     :initarg :preconds
	     :documentation "Preconditions of a rule (lines that must be present).")
   (postconds :reader rule=postconds
	      :writer rule=set-postconds!
	      :initarg :postconds
	      :documentation "Postconditions of a rule (lines that will be created).")
   (sideconds :reader rule=sideconds
	      :writer rule=set-sideconds!
	      :initarg :sideconds
	      :documentation "Sideconditions of a rule (predicates that must be true).")
   (actions :reader rule=actions
	    :writer rule=set-actions!
	    :initarg :actions
	    :documentation "Actions of a rule (changes in support structure).")
   (metavars :reader rule=metavars
	     :writer rule=set-metavars
	     :initarg :metavars
	     :documentation "Meta variables used in this rule.")
   (description :reader rule=description
		:writer rule=set-description
		:initarg :desc
		:documentation "Short description of the rule.")
   (env :reader rule=env
	:writer rule=set-env
	:initarg :env
	:documentation "Environment used at time of definition.")
   )
  (:documentation "A natural deduction rule.")))

(defgeneric rule~args (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The arguments of the rule."))
  (:method ((ndrule rule+rule)) (rule=args ndrule)))

(defgeneric rule~set-args! (args ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of arguments (symbols) and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's args slot to args."))
  (:method (args (ndrule rule+rule)) (rule=set-args! args ndrule)))

(defgeneric rule~name (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The name of the rule."))
  (:method ((ndrule rule+rule)) (keim~name ndrule)))

(defgeneric rule~set-name! (name ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name (symbol) and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's name slot to name."))
  (:method (name (ndrule rule+rule)) (keim~set-name! name ndrule)))

(defgeneric rule~argtypes (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The argument types of the rule."))
  (:method ((ndrule symbol)) 
	   (let ((rule (rule~find-rule ndrule)))
	     (if rule
		 (rule=argtypes rule)
	     (warn " no rule ~A" ndrule))))
  (:method ((ndrule rule+rule)) (rule=argtypes ndrule)))

(defgeneric rule~set-argtypes! (argtypes ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of argument types and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's argtypes slot to argtypes."))
  (:method (argtypes (ndrule rule+rule)) (rule=set-argtypes! argtypes ndrule)))

(defgeneric rule~preconds (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The preconditions of the rule."))
  (:method ((ndrule rule+rule)) (rule=preconds ndrule)))

(defgeneric rule~set-preconds! (preconds ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of preconditions and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's preconds slot to preconds."))
  (:method (preconds (ndrule rule+rule)) (rule=set-preconds! preconds ndrule)))

(defgeneric rule~postconds (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The post conditions of the rule."))
  (:method ((ndrule rule+rule)) (rule=postconds ndrule)))

(defgeneric rule~set-postconds! (postconds ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of postconditions and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's postconds slot to postconds."))
  (:method (postconds (ndrule rule+rule)) (rule=set-postconds! postconds ndrule)))

(defgeneric rule~sideconds (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The side conditions of the rule."))
  (:method ((ndrule rule+rule)) (rule=sideconds ndrule)))

(defgeneric rule~set-sideconds! (sideconds ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of side conditions and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's sideconds slot to sideconds."))
  (:method (sideconds (ndrule rule+rule)) (rule=set-sideconds! sideconds ndrule)))

(defgeneric rule~actions (ndrule)
  (declare (edited  "2-SEP-92 10:00")
           (authors NESMITH)
           (input   "A natural deduction rule.")
           (effect  "None.")
           (value   "The actions of the rule."))
  (:method ((ndrule rule+rule)) (rule=actions ndrule)))

(defgeneric rule~set-actions! (actions ndrule)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A list of actions and an ndrule.")
           (effect  "None.")
           (value   "Sets NDRULE's actions slot to actions."))
  (:method (actions (ndrule rule+rule)) (rule=set-actions! actions ndrule)))


(defvar rule*rule-hash-table (make-hash-table :test #'equal)
  "A hash table to map rule names to the rule objects themselves.")

(defun rule~find-rule (name)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A name of a rule (symbol or string).")
           (effect  "None.")
           (value   "The rule object with this name, or NIL if none exists."))
  (gethash 
   (etypecase name
     (symbol (symbol-name name))
     (string name))
   rule*rule-hash-table))

#| Here we keep a mapping of various things, all slots are alists.
arg-name->real-arg keeps a mapping between the argument names (symbols)
                 and the real arguments that were supplied.
intern-arg->extern-arg keeps track of the correspondence between the
            meta-lines in the rule definition and the real lines in the proof
meta-vars->real-vars maps between meta-variables and the formulas to which
           they match.
failed will be true if somewhere we failed to match.
hyp-set-vars->hyp-sets is a mapping from variables to lists of hypotheses
The macros defined below are used just to make the names shorter.
|#

(eval-when (load compile eval)
(defstruct rule=mapping
  (arg-name->real-arg nil)
  (intern-arg->extern-arg nil)
  (meta-vars->real-vars nil)
  (failed nil)
  (hyp-set-vars->hyp-sets nil)    
)


(defmacro rule=map-name2real (mapping)
  `(rule=mapping-arg-name->real-arg ,mapping))

(defmacro rule=map-intern2extern (mapping)
  `(rule=mapping-intern-arg->extern-arg ,mapping))

(defmacro rule=map-meta2real (mapping)
  `(rule=mapping-meta-vars->real-vars ,mapping))

(defmacro rule=map-hyp-sets (mapping)
  `(rule=mapping-hyp-set-vars->hyp-sets ,mapping))

(defmacro rule=failed-mapping-p (mapping)
  `(rule=mapping-failed ,mapping))
)


(eval-when (load compile eval)
(defvar rule*verbose t "If nil, warnings will be suppressed, 
otherwise they will be printed.")
)

(defun rule~match-p (rulename arglist)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule name and a list of arguments.")
           (effect  "None.")
           (value   "If the arguments match the rule (so the rule could
be carried out), then T, else NIL."))
  (let* ((rule*verbose nil)
	 (rule (if (typep rulename 'rule+rule) 
		   rulename 
		 (rule~find-rule rulename)))
	 (mapping
	  (if rule (rule=do-match rule arglist)
	    (make-rule=mapping :failed t))))
    (if (rule=failed-mapping-p mapping)
	nil
      t)))

(defun rule=do-match (rule arglist)
  (declare
   (authors nesmith)
   (input "A rule and the list of arguments supplied.")
   (value "returns a rule=mapping
  with information on how to instantiate new lines;
if fails to match, returns a mapping with :failed t"))
  (with-slots (args argtypes arghelp preconds postconds sideconds actions
		      description metavars env) rule
   (let* ((arg-arglist-alist 
	   (rule=match-args-to-arglist arglist args argtypes))
	  (mapping (make-rule=mapping
		    :arg-name->real-arg 
		    arg-arglist-alist
		    :failed (eq arg-arglist-alist :fail))))
     ;; first see if all terms have proper types
     (dolist (nonlinearg (remove-if #'(lambda (sym) 
					(member sym preconds 
						:key #'nd~line-label))
				    args))
       (let ((wff (env~lookup-object nonlinearg env))
	     (realarg (nth (position nonlinearg args) arglist)))
	 (when (term~p realarg)
	   (rule=match-wffs mapping wff realarg)))
       (when (rule=failed-mapping-p mapping)
	 (return-from rule=do-match mapping)))
     (unless (rule=failed-mapping-p mapping)
       (dolist (precond preconds)
	 (push (cons precond (cdr (assoc (nd~line-label precond) 
					 (rule=map-name2real mapping))))
	       (rule=map-intern2extern mapping)))
       (rule=scan-precond-hypsets preconds mapping))
					;	(format t "~%~A~%" mapping)
     (unless (rule=failed-mapping-p mapping)
       (rule=match-preconds mapping preconds))
     (unless (rule=failed-mapping-p mapping)
       (rule=match-modified-postconds 
	mapping
	(remove-if-not
	 #'(lambda (line)
	     (member (nd~line-label line) preconds 
		     :key #'nd~line-label
		     :test #'string-equal))
	 postconds)))
     (unless (rule=failed-mapping-p mapping)
       (rule=match-sideconds mapping sideconds))
     mapping)))

(defun rule=scan-precond-hypsets (preconds mapping)
  (declare
   (authors nesmith)
   (input "The list of preconditions from the rule and the mapping so far.")
   (value "the mapping extended so that the hyp-set variables in the 
match to the sets of hypotheses in the real lines that are not explicitly
given."))
  (let ((intern2extern (rule=map-intern2extern mapping)))
    (dolist (precond preconds)
      (let* ((extern (cdr (assoc precond intern2extern)))
	     (hyp-set (first (rule=hypset precond)))
	     (extra-hyps (mapcar #'(lambda (x) (cdr (assoc x intern2extern)))
				 (nd~line-hyps precond)))
	     (hyp-set-mapping (rule=map-hyp-sets mapping))
	     (current-binding (cdr (assoc hyp-set hyp-set-mapping))))
	(push (cons hyp-set
		    (union current-binding
			   (set-difference (nd~line-hyps extern)
					   extra-hyps)))
	      (rule=map-hyp-sets mapping))))))
		

(defun rule=match-modified-postconds (mapping postconds)
; here check to see if the hypotheses would match up if this rule were called
  (declare
   (authors nesmith)
   (input "A mapping and the list of postconditions")
   (value "The mapping, marked as failed if: 1. a postcondition that would be
justified because of this rule would have, as justifying lines, lines whose
hypotheses are not contained in those of the justified line.  
2. A line would have justify itself."))
  (dolist (postcond postconds)
    (let* ((real-line (cdr (assoc (nd~line-label postcond)
				  (rule=map-intern2extern mapping)
				  :key #'nd~line-label
				  :test #'string-equal)))
	   (would-be-hyps (rule=figure-would-be-hyps postcond real-line 
						     mapping)))
      (unless (subsetp would-be-hyps (nd~line-hyps real-line))
	(rule=warn ";;; Hyp problem: Line ~A would have to have hypotheses ~A, which are ~%" (nd~line-label real-line) (mapcar #'nd~line-label would-be-hyps))
	(rule=warn ";;; which are not a subset of its existing hypotheses.~%")
	(setf (rule=mapping-failed mapping) t)
    )
      (when (and (not (nd~plan-p postcond))
		 (member real-line
			 (mapcar #'(lambda (jline)
				     (cdr (assoc (nd~line-label jline)
						 (rule=map-intern2extern
						  mapping)
						 :key #'nd~line-label
						 :test #'string-equal)))
				 (nd~line-just-lines postcond))))
	(rule=warn ";;; Line ~A cannot be used to justify itself.~%" real-line)
	(setf (rule=mapping-failed mapping) t))
      )))

(defun rule=figure-would-be-hyps (line real-line mapping)
  (declare
   (authors nesmith)
   (input "A meta-line from a rule, the real line it matches to, and the
mapping.")
   (value "A list of hypotheses this real-line would have to have if it
got the justification indicated for it by the meta-line."))
  (let ((hyp-sets (rule=hypset line))
	(extra-hyps (nd~line-hyps line))
	(just-lines (nd~line-just-lines line)))
    (remove-duplicates
    (cond ((nd~plan-p line)
	   (append (apply #'append
			  (mapcar #'(lambda (hypset)
				      (cdr (assoc hypset (rule=map-hyp-sets mapping))))
				  hyp-sets))
		   (mapcar #'(lambda (l) (cdr (assoc l
						     (rule=map-intern2extern mapping))))
			   extra-hyps)))
	  ((member line extra-hyps) (list real-line))
	  (t
	   (let ((just-line-hypsets (apply #'append (mapcar #'rule=hypset
							    just-lines))))
	     (union (apply #'append
			   (mapcar #'(lambda (hypset)
				(cdr (assoc hypset (rule=map-hyp-sets mapping))))
			    just-line-hypsets))
		    (append (apply #'append
				   (mapcar #'(lambda (hypset)
					       (cdr (assoc hypset (rule=map-hyp-sets mapping))))
					   hyp-sets))
			    (mapcar #'(lambda (l) (cdr (assoc l
							      (rule=map-intern2extern mapping))))
				    extra-hyps)))))))))

(defun rule=match-args-to-arglist (arglist args argtypes)
  (declare
   (authors nesmith)
   (input "The list of formal parameters, the arguments received, and the
required types of these arguments.")
   (value "If the proper number of arguments is given and  each of the
arguments has the proper type, an alist matching the parameter name to
the argument, otherwise the symbol :failed."))
   (unless (= (length arglist) (length args))
     (rule=warn ";;; Incorrect number of arguments received.~%")
     (return-from rule=match-args-to-arglist :fail))
  (mapcar #'(lambda (realarg param type)
	      (if (typep realarg type)
		  (cons 
		   param
		   realarg)
		(progn
		  (rule=warn ";;; Argument ~A should be a ~A; received ~A~%" param type realarg)
		  (return-from rule=match-args-to-arglist :fail))))
	  arglist args argtypes))



(defun rule=match-preconds (mapping preconds)
  (declare
   (authors nesmith)
   (input "A mapping and the list of preconditions")
   (value "the mapping updated by mapping the meta-formulas of the preconditions
against the formulas of the real arguments."))
  (dolist (precond preconds mapping)
    (rule=match-line-wffs
     mapping precond 
     (cdr (assoc precond (rule=map-intern2extern mapping))))
    (when (rule=failed-mapping-p mapping)
      (return-from rule=match-preconds mapping))))


(defun rule=match-line-wffs (mapping template real-line)
  (declare
   (authors nesmith)
   (input "a mapping, a meta-line and a real line")
   (value "the mapping extended with the meta-variables in the formula of
the meta-line associated with the real instances; if matching fails, the
mapping will have it :failed slot set to t."))
  (let ((template-wff (node~formula template))
	(real-wff (node~formula real-line)))
    (rule=match-wffs mapping template-wff real-wff)))

; here we handle the meta+error to catch any matching failures without
; going into the debugger.

; changes in sys module have been made 
(defun rule=match-wffs (mapping template-wff real-wff)
  (declare
   (authors nesmith)
   (input "a mapping, a meta-wff and a real wff")
   (effect "Matching of the wffs is attempted, using the already matched
part of the mapping.  This happens inside a handler-case for meta+error,
so if the match fails, we just set the :failed slot of the mapping to t
return the mapping.")
   (value "the mapping, either extended with the new match or a failure."))
  (sys~handler-case
      (progn
	(setf (rule=map-meta2real mapping)
	  (meta~match template-wff real-wff (rule=map-meta2real mapping)))
	mapping)
      (meta+error (condition) 
		  (format t "~&~A~%" condition)
		  (setf (rule=mapping-failed mapping) t)
		  mapping)))

;; before checking sideconds, first make sure all meta-vars have been
;; accounted for
(defun rule=realize (thing mapping)
  (declare
   (authors nesmith)
   (input "an object and a mapping")
   (value "the object to which this thing *really* corresponds to, using the
mapping."))
  (typecase thing
    (null nil)
    (symbol thing)
    (rule+meta-line
     (cdr (assoc thing (rule=map-intern2extern mapping))))
    (nd+line thing)
    (term+term
     (meta~meta-subst
      thing
      (rule=map-meta2real mapping)))
    (otherwise thing)))


(defun rule=match-sideconds (mapping sideconds) 
  (declare
   (authors nesmith)
   (input "a mapping and a list of side conditions")
   (effect "the side conditions are instantiated with the real lines and
formulas using the mapping, then are evaluated.  If any returns nil,
a failed mapping is returned.  Otherwise the original mapping is returned."))
  (let* ((instantiated-sideconds 
	  (mapcar 
	   #'(lambda (sidecond)
	       (cons (car sidecond)
		     (mapcar #'(lambda (arg) 
				 (rule=realize arg mapping))
			     (cdr sidecond))))
	   sideconds))
	 (bad-sidecond 
	  (find-if-not 
	   #'(lambda (sidecond)
	       (eval sidecond))
	  instantiated-sideconds)))
  (when bad-sidecond
    (when rule*verbose
	  (rule=warn ";;; Following sidecondition is not fulfilled: ~A~%"
		     bad-sidecond))
    (setf (rule=mapping-failed mapping) t))
  mapping))




(defmacro rule~def-rule (name &rest attribs)
  (declare
   (authors nesmith)
   (input "A name and list of attributes.
Here's an example of the syntax.
\\begin{code}
(rule~def-rule forall-i
  (arguments D1 X)
  (argumenttypes nd+line sym+sym)
  (arghelps \"An universal line\" \"A new parameter\")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants (forall (O (O AA)))))
  (meta-variables (A (O AA)) (X AA))
  (preconditions
   (D1 (H) () (forall A)))
  (postconditions
   (D2 (H) () (eval (hop~beta-contract (A X))))
   (D1 (H) () (forall A) (\"ForallI\" () (D2))))
  (actions ((PP D1 SS) (PP D1 D2 SS)))
  (sideconditions (nd~not-free-in-lines-or-hyps-p X D1))
  (description \"Universal introduction\"))
\\end{code}

Here's a description of the syntax:
\\begin{code}
(rule~def-rule {\\it name}
 (arguments {\\it symbol}+)
 (argumenttypes {\\it type\}+)
 (arghelps {\\it string}+)
 (declarations {\\it postdeclaration}*)
 (meta-variables {\\it symboldeclaration}*)
 (preconditions {\\it meta-line-precond}*)
 (postconditions {\\it meta-line-postcond}*)
 (sideconditions {\\it sidecondition}*)
 (actions {\\it action})
 (description {\\it string})
 )
where the number of arguments, argumenttypes and arghelps specified 
must be equal.

\\begin{postsyntax}
\syntax{
{\\nt meta-line-precond} ::= 
    ({\\nt name} ({\\nt hypset}*) ({\\nt extra-hyps*}) {\\nt formula}) .

{\\nt meta-line-postcond} ::= 
    ({\\nt name} ({\\nt hypset}*) ({\\nt extra-hyps*}) \\{{\\nt formula} | {\\nt eval-formula}\\} {\\nt justification}+). 

{\\nt hypset} ::= {\\nt symbol}.
{\\nt extra-hyp} ::= {\\nt symbol}.
{\\nt eval-formula} ::= (eval ({\\nt lispfunctionname} {\\nt term}*)).
{\\nt sidecondition} ::= ({\\nt lispfunctionname} \\{{\\nt term} | {\\nt linename}\\}*).

{\\nt action} ::= ({\\nt pattern} {\\nt pattern}+).
{\\nt pattern} ::= ({\\nt planline} {\\nt support-line}*).
{\\nt planline} ::= PP | {\\nt linename}.
{\\nt support-line} ::= SS | {\\nt linename}.
}
\\end{postsyntax}
\\end{code}

All the symbols and types used in the formulas and arguments of the rule
must be declared. Those which are to be interpreted as meta-variables 
must be declared in the {\\tt meta-variable} section.

Let's take a look at how the preconditions are specified.  Note that 
they have a name (which is how they can be referred to, and which should also
be the argument name used) and of course a formula.  For hypotheses, there
are two lists.  The first is a list of symbols, each of which represents
a {\\em set\\/} of hypotheses.  The symbols of the second list, the 
{\\em extra-hyps\\/}, each are the name of a single line, which must be
defined in the rule somewhere.

Let's take an example to show how the matching process works when a rule
is applied.
\\begin{code}
(rule~def-rule exists-e
  (arguments D1 X C1)
  (argumenttypes nd+line sym+sym nd+line)
  (arghelps \"An existential line\" \"A parameter\" \"Line to be proved\")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants (exists (O (O AA)))))
  (meta-variables (A (O AA)) (B O) (X AA))
  (preconditions
   (D1 (H) () (exists A))
   (C1 (H) () B))
  (postconditions
   (D2 () (D2) (eval (hop~beta-contract (A X))) (\"Choose\" (X)))
   (C2 (H) (D2) B)
   (C1 (H) ()   B (\"Exists-E\" () (D1 C2))))
  (actions ((C1 D1 SS) (C2 D2 SS)))
  (sideconditions
   (nd~not-free-in-lines-or-hyps-p X C1 D1))
  (description \"Existential elimination\"))
\\end{code}

Suppose we had the following proof:
\\begin{code}
H1  (H1) ! (exists (lam (X I) (P X)))  Hyp
    ...
C   (H1) ! (exists (lam (Y I) (P Y)))  Planned
\\end{code}

Suppose we apply this rule with the arguments {\\tt H1}, a new parameter
{\\tt Z} of type I, and the line {\\tt C}.

We get the following matches:
\\begin{itemize}
\\item {\\tt D1} \\(=\\) {\\tt H1} therefore
\\item {\\tt A} \\(=\\) {\\tt (lam (X I) (P X))} and
\\item the type-variable {\\tt AA} \\(=\\) {\\tt I}
\\item {\\tt X} \\(=\\) {\\tt Z} 
\\item {\\tt C1} \\(=\\) {\\tt C} and therefore
\\item {\\tt B} \\(=\\) {\\tt (lam (Y I) (P Y))}
\\item finally {\\tt H} \\(=\\) {\\tt (H1)}
\\end{itemize}

Because the matching succeeds, the sideconditions are checked.
In this case, we evaluate {\\vb (nd~not-free-in-lines-or-hyps-p Z C H1)}
which will succeed because {\\tt Z} is not free in the lines {\\tt C} or
{\\tt H1}. 

Now we actually carry out the rule.  To do so, we see that there are two
new lines in the postconditions, {\\tt D2} and {\\tt C2}.  Say that these
new lines will be named {\\tt L1} and {\\tt L2}. 
{\\tt L2} will
be created with the hypotheses {\\tt (H1 L1)} as well as, because
the symbol {\\tt D2} appears in the {\\tt extra-hyps} list of the
definition of {\\tt C2}.  The line {\\tt L1} will have only itself as a 
hypothesis, which corresponds to the definition for {\\tt D2}. 

The formula for {\\tt L2} will be {\\tt (lam (Y I) (P Y))}, because that
is the value of {\\tt B}.  For that of {\\tt L1}, we must evaluate the
formula {\\tt (hop~beta-contract (A X))}.  The formula {\\tt (A X)} is
really {\\tt ((lam (X I) (P X)) Z)}, so when it it beta-contracted, we
get {\\tt (P Z)}.  {\\tt C2} is a new line, but it has no justification,
so we make its real-life counterpart {\\tt L2} a planned line.

Note that by the rule, {\\tt C1} should get a justification, so we justify
{\\tt C} with that justification, instantiating the meta-variables.
Here's what the proof will look like after the rule is used.
\\begin{code}
H1  (H1)    ! (exists (lam (X I) (P X)))  Hyp
L1  (L1)    ! (P Z)                       Choose: (Z)
    ...
L2  (H1 L1) ! (exists (lam (Y I) (P Y)))  Planned
C   (H1)    ! (exists (lam (Y I) (P Y)))  Exists-E: (H1 L2)
\\end{code}

Now we must consider the {\\tt actions}.  These have an influence only on the
supports of a planned line. The only action for this rule was
{\\tt ((C1 D1 SS) (C2 D2 SS))}.  The first pattern, {\\tt (C1 D1 SS)}, matches
any plan line that is {\\tt C1} and whose supports include {\\tt D1}, whereas
all the remaining supports will match {\\tt SS}.  In this case it matches
the planned line {\\tt C} (being {\\tt C1}), assuming that {\\tt C} had {\\tt H1}
among its supports.

The remaining patterns are then constructed.  That is, the planned line
{\\tt C2} should then have as supports {\\tt D2} as well as the remaining
supports of {\\tt C}.  
")
   (effect "Parses the definition and creates a new rule (or redefines
 an existing rule with this name).")
   (value "The rule created."))
  (let ((args nil)
	(metavars nil)
	(declarations nil)
	(argtypes nil)
	(preconds nil)
	(postconds nil)
	(sideconds nil)
	(actions nil)
	(desc "")
	(var-mvar-alist nil)
	(arghelps nil)
	(environment (env~create))
	(mvar-list nil))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (cond
	    ((string-equal  (car attrib) :arguments)
	     (setq args 
	       (cdr attrib)))
	    ((string-equal (car attrib) :argumenttypes )
	     (setq argtypes (cdr attrib) ))
	    ((string-equal (car attrib) :preconditions)
	     (setq preconds (cdr attrib)))
	    ((string-equal (car attrib) :postconditions)
	     (setq postconds (cdr attrib)))
	    ((string-equal (car attrib) :sideconditions)
	     (setq sideconds 
	       (cdr attrib)))
	    ((string-equal (car attrib) :actions)
	     (setq actions (cdr attrib)))
	    ((string-equal (car attrib) :declarations)
	     (setq declarations (cdr attrib)))
	    ((string-equal (car attrib) :arghelps)
	     (setq arghelps (cdr attrib)))
	    ((string-equal (car attrib) :description)
	     (setq desc (cadr attrib)))
	    ((string-equal (car attrib) :meta-variables)
	     (setq metavars attrib))
	    (t
	     (error ";;;rule~~def-rule: Not expecting ~A" attrib)))
	(error ";;;rule~~def-rule: Not expecting ~A" attrib)))
    (unless (= (length args) (length argtypes) (length arghelps))
      (error ";;; rule~~def-rule: Number of arguments, argumenttypes, and argument help strings must be all be equal."))
    (setq var-mvar-alist
      (mapcar #'(lambda (sym) 
		  (cons sym (make-symbol (format nil "~A" sym))))
	      (append args 
		      (mapcar #'car (cdr metavars)))))
    (post~read-object-list declarations environment)
    (post~read-object-list (list (sublis var-mvar-alist metavars)) environment)
    (setq args (sublis var-mvar-alist args))
    (setq preconds
      (mapcar #'(lambda (line) (rule=process-line line environment nil))
	      (sublis var-mvar-alist preconds)))
    (setq postconds
      (mapcar #'(lambda (line) (rule=process-line line environment t))
	      (sublis var-mvar-alist postconds)))
    (rule=sub-lines-for-labels (append preconds postconds))
    (setq sideconds
      (sublis
       (mapcar #'(lambda (x) (cons (nd~line-label x) x)) (append preconds
								 postconds))
       (sublis (mapcar #'(lambda (pair)
			   (cons (car pair)
				 (or (env~lookup-object (cdr pair) environment)
				     (cdr pair))))
		       var-mvar-alist)
	       sideconds)))
    (setq actions 
      (sublis
       (mapcar #'(lambda (x) (cons (nd~line-label x) x)) (append preconds
								 postconds))
       (sublis var-mvar-alist actions)))
    `(progn
       (let ((newrule (make-instance 'rule+rule :name ',name
				     :preconds ',preconds
				     :postconds ',postconds
				     :sideconds ',sideconds
				     :desc ',desc
				     :args ',args
				     :argtypes ',argtypes
				     :actions ',actions
				     :arghelps ',arghelps
				     :metavars ',mvar-list
				     :env ',environment)))
	 (rule=warn ";;; Defining rule ~A~%" ',name)
	 (setf (gethash (symbol-name ',name)  rule*rule-hash-table)
	   newrule)
	 ))))



(defun rule~describe (rulename)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule name.")
           (effect  "Prints a description of the rule to standard output.")
           (value   "Undefined."))
  (flet ((print-line (line) (nd~show-line line)))
    (let ((rule*verbose t)
	  (*print-escape* nil)
	  (*print-gensym* nil)
	  (rule (rule~find-rule rulename)))
      (unless rule
	(rule=warn ";;; RULE~~DESCRIBE: no such rule ~A.~%" rulename)
	(return-from rule~describe nil))
      (with-slots (args argtypes arghelps preconds postconds sideconds actions
			description) rule
	(format t "~A: ~A~%" rulename description)
	(format t "Arguments for ~A are:~%" rulename)
	(mapc #'(lambda (name argtype arghelp)
		  (format t "~A: ~A, must be of type ~A~%" name arghelp argtype))
	      args argtypes arghelps)
	(format t "Preconditions (existing lines) are:~%")
	(mapc #'print-line preconds)
	(format t "Postconditions (new and modified lines) are:~%")
	(mapc #'print-line postconds)
	(when sideconds
	  (format t "Restrictions are:~%~{~A ~}" sideconds))
	(format t "~%Support structure changes are: ~A" actions)))))


(defun rule=warn (&rest args)
  (declare
   (authors nesmith)
   (value "Arguments as for format (without the stream)")
   (effect "If rule*verbose is true, uses format to print the arguments to
*error-output*"))
  (let ((*print-escape* nil)
	(*print-gensym* nil))
  (when rule*verbose
    (apply #'format *error-output* args))))



(eval-when (load compile eval)
(defclass rule=evaluable-formula (term+term)
  ((function :initarg :function :initform 'identity 
	     :writer rule=evaluable-formula-function-writer! 
	     :reader rule=evaluable-formula-function-reader 
	     )
   (args :initarg :args :initform nil 
	 :writer rule=evaluable-formula-args-writer!
	 :reader rule=evaluable-formula-args-reader))
  (:documentation "An evaluable-formula comes into the picture when we have a formula in
the postconditions that looks like (eval (f a b c)).  The (f a b c)
is the evaluable part.  F is a lisp function name and a b c are all
terms or more likely meta-terms.  "))
)

(defmethod nd~print-wff ((term rule=evaluable-formula))
  (format t "(EVAL (~A~{ ~S~}))" (rule=evaluable-formula-function-reader term)
	  (rule=evaluable-formula-args-reader term)))


(defmethod post~print ((term rule=evaluable-formula) stream)
  (format stream "(EVAL (~A~{ ~S~}))" 
	  (rule=evaluable-formula-function-reader term)
	  (rule=evaluable-formula-args-reader term)))


(defun rule=get-wff (wff line environment postcond-p)
  (declare
   (authors nesmith)
   (value "The parsed term.")
   (input "A Post representation of a wff, a line being defined as postcondition
 or precondition, an environment and an indicator that we are or are not
working on a postcondition.")
   (effect "the formula is read in using the environment. If the formula
is of the form (eval ..), then it is interpreted as an evaluable-formula,
otherwise as a meta-term."))
   
  (if (and (listp wff) (string= (car wff) "EVAL"))
      (progn
	(unless postcond-p
	  (error "Illegal line ~A in rule definition: only postconditions can use eval." line ))
	(setq wff (cadr wff))
	(make-instance 'rule=evaluable-formula
	  :function (car wff)
	  :args (mapcar #'(lambda (x) 
			    (meta~read-poly-term x environment))
			(cdr wff))))
    (meta~read-poly-term wff environment)))


; process-line

(defun rule=process-line (line environment postcond-p)
  (declare
   (authors nesmith)
   (input "A line, an environment and an indicator of whether or not we
are working on a postcondition (as opposed to a precondition).")
   (effect "the line is read in, using the definitions in the environment, and
a meta-line is created if no errors are found")
   (value "The new line"))
   (let ((newline (make-instance 'rule+meta-line))
	(label (first line))
	(hyp-sets (second line))
	(extra-hyps (third line))
	(formula (fourth line))
	(just (fifth line)))
    (nd~set-line-label! newline label)
    (if (or postcond-p (<= (length hyp-sets) 1))
      (setf (rule=hypset newline) hyp-sets)
      (error "RULE~~DEF-RULE: At most one hypothesis set for a precondition is
allowed.  Tried to use ~A for line ~A." hyp-sets label))
    (nd~set-line-hyps! newline 
		       (mapcar #'(lambda (sym) 
				   (make-symbol (format nil "~A" sym)))
			       extra-hyps))
    (node~set-formula! newline (rule=get-wff formula line environment postcond-p))
    (node~set-justification!
       newline
       (if just (funcall (if (string-equal (first just) "Planned")
		    #'nd~make-planned-justification
		  #'nd~make-verified-justification)
		(first just)
		(mapcar #'(lambda (x) (rule=get-wff x line environment postcond-p))
			(second just))
		(mapcar #'(lambda (sym) (make-symbol (format nil "~A" sym)))
			(third just)))
	 (if postcond-p
	     (nd~make-planned-justification "Planned" nil nil)
	   (make-instance 'rule+var-just))))
    newline))

; labels may appear in hyps or justifications where lines should appear.
; fix that here
(defun rule=sub-lines-for-labels (lines)
  (let ((label-line-alist 
	 (mapcar #'(lambda (x) (cons (nd~line-label x) x)) lines)))
    (dolist (line lines lines)
      (let ((hyps (nd~line-hyps line))
	    (just-lines (nd~just-lines (node~justification line))))
	(nd~set-line-hyps! line 
			   (sublis label-line-alist hyps
					:test
					#'(lambda (x y) 
					   (and (symbolp x)
						(symbolp y)
						(string-equal x y)))))
	(just~set-nodes! (node~justification line) 
			 (sublis label-line-alist just-lines
				 :test #'(lambda (x y) 
					   (and (symbolp x)
						(symbolp y)
						(string-equal x y))))))
      )))



(defun rule=clever-labelling (line)
; want to give this line a label based on what purpose it's serving.
; but put it off for the time being
  (declare (ignore line))
  (nd~make-new-line-name )
)

(defmethod meta~meta-subst ((wff rule=evaluable-formula) bindings)
  (apply (symbol-function (rule=evaluable-formula-function-reader wff))
	 (mapcar #'(lambda (x) (meta~meta-subst x bindings))
		 (rule=evaluable-formula-args-reader wff))))


(defun rule=make-real-new-lines (mapping new-lines)
  (declare
   (authors nesmith)
   (input "A mapping and a list of the meta-lines that appear only in
the postcondition.")
   (value "Two values are returned: the updated mapping and 
a list of new lines, using the instantiations for terms, hypotheses
and lines given in the mapping."))
  (let ((real-new-lines nil))
    (dolist (line new-lines (values mapping (nreverse real-new-lines)))
      (let ((real-new-line 
	     (make-instance 'nd+line
	       :formula (term~copy 
			 (meta~meta-subst (node~formula line)
					  (rule=map-meta2real mapping)))
	       :justification
	       (if (nd~plan-p line)
		   (nd~make-planned-justification "Planned" nil nil)
		 (nd~make-verified-justification
		  (nd~line-just-rule line)
		  (mapcar #'(lambda (term) 
			      (meta~meta-subst term 
					       (rule=map-meta2real mapping)))
			  (nd~line-just-terms line))			  
		  (sublis (rule=map-intern2extern mapping)
			  (nd~line-just-lines line)))))))
	(nd~set-line-label! real-new-line 
			    (rule=clever-labelling real-new-line))
	(let ((new-hyps (rule=figure-new-hyps line real-new-line mapping)))
	  (nd~set-line-hyps! real-new-line new-hyps))
	(push (cons line real-new-line) 
	      (rule=map-intern2extern mapping))
	(push real-new-line real-new-lines)))))

(defun rule=figure-new-hyps (line real-new-line mapping)
  (declare
   (authors nesmith)
   (input "A meta-line which appears only in the postconditions, 
   the new real line which it has given rise to, and the mapping.")
   (value "A list of the hypotheses for the real line, 
      using the instantiations for terms, hypotheses and lines given in 
      the mapping. Duplicates are removed."))
  (let ((hyp-sets (rule=hypset line))
	(extra-hyps (nd~line-hyps line))
	(just-lines (nd~line-just-lines line)))
    (remove-duplicates
     ;; three cases here.
     ;; 1. the new line is a planned line. we simply take the union of
     ;;    all the hyps from its hyp-sets, along with any extra-hyps.
    (cond ((nd~plan-p real-new-line)
	   (append 
	    (apply #'append
		   (mapcar 
		    #'(lambda (hypset)
			(cdr (assoc hypset 
				    (rule=map-hyp-sets mapping))))
		    hyp-sets))
	    (mapcar 
	     #'(lambda (l) (cdr (assoc l (rule=map-intern2extern mapping))))
	     extra-hyps)))
     ;; 2. the new line is itself a hypothesis (has itself as an extra-hyp).
     ;;    then we return just the line itself (minimizing the hyps).
	  ((member line extra-hyps) (list real-new-line))
     ;; 3. the new line is justified and not a hypothesis.
     ;;    then we need to have as a hyp each of the hyps contained 
     ;;    in the hyp-sets of its justifying lines.  We also need all the
     ;;    hyps specified by its own hyp-sets, as well as its extra-hyps.
	  (t
	   (let ((just-line-hypsets 
		  (apply #'append (mapcar #'rule=hypset just-lines))))
	     (union 
	      (apply #'append
		     (mapcar #'(lambda (hypset)
				 (cdr (assoc hypset 
					     (rule=map-hyp-sets mapping))))
			     just-line-hypsets))
	      (append 
	       (apply #'append
		      (mapcar #'(lambda (hypset)
				  (cdr (assoc hypset 
					      (rule=map-hyp-sets mapping))))
			      hyp-sets))
	       (mapcar #'(lambda (l) 
			   (cdr (assoc l (rule=map-intern2extern mapping))))
		       extra-hyps)))))))))
  

(defun rule=modify-old-lines (mapping lines)
  (declare
   (authors nesmith)
   (input "A mapping and the list of old lines that should be modified
          by the rule application.")
   (value "Not specified. Done for side effects only.")
   (effect "The lines whose justifications should be changed as a result
of the rule are changed, creating a new justification for them."))
  (if lines
      (let ((real-old-lines1 
	     (let ((real-old-lines1 lines))
	       (dolist (pair (rule=map-intern2extern mapping)
			   real-old-lines1)
		 (setq real-old-lines1 
		   (substitute-if 
		    (cdr pair) 
		    #'(lambda (x)
			(string= (nd~line-label x)
				 (nd~line-label (car pair))))
		    real-old-lines1))))))
	(do ((real-old-lines real-old-lines1 (cdr real-old-lines))
	     (line-schemas lines (cdr line-schemas)))
	    ((null (car real-old-lines)) real-old-lines1)
	  (let ((new-just (node~justification (car line-schemas))))
	    (node~set-justification! 
	     (car real-old-lines)
	     (funcall (if (string-equal (just~rule new-just) "Planned")
			  #'nd~make-planned-justification
			#'nd~make-verified-justification)
		      (just~rule new-just)
		      (mapcar #'(lambda (term) 
				  (meta~meta-subst term
				   (rule=map-meta2real mapping)))
			      (nd~just-terms new-just))
		      (sublis (rule=map-intern2extern mapping)
			      (nd~just-lines new-just))))
	    )))
  nil))
		      



(defun rule=add-new-lines-to-proof (new-lines old-lines)
  (declare
   (authors nesmith)
   (input "A list of the new lines created by the rule, as well
as the old lines involved in the rule application (which could perhaps
now be justified by one of the new lines.")
   (value "Done for side effects only")
   (effect "Adds each of the new lines to the proof before any lines
it justifies, and after each of the lines which justifies it or is a
hypothesis of it."))
  (dolist (line new-lines)
    (let ((justified-by (nd~line-just-lines line))
	  (justifies (remove-if-not #'(lambda (old-line)
					(member line (nd~line-just-lines
						      old-line)))
				    old-lines))
	  (supports (nd~line-supports line))
	  (hyps (nd~line-hyps line)))
      (nd~insert-line-after-before line (append supports justified-by hyps) 
				   justifies))))



(defun rule=match-action-pattern-lines (mapping pattern-to-match
					patterns-to-generate)
  (declare
   (authors nesmith)
   (input "A mapping, a pattern to match and the patterns to generate
when a match is found.")
   (value "Done for side effect only")
   (effect "After instantiating the patterns with the real lines that
should be used, we test each planned line of the current proof to see if
it and its supports matches the pattern-to-match.  If so, then we
apply the patterns-to-generate to it."))
  (let ((new-pattern-to-match 
	 (mapcar #'(lambda (x) 
		     (or (cdr (assoc x (rule=map-intern2extern mapping)))
			 x))
		 pattern-to-match))
	(new-patterns-to-generate
	 (mapcar #'(lambda (pattern-to-match)
		     (mapcar #'(lambda (x) 
				 (or (cdr (assoc x (rule=map-intern2extern 
						    mapping)))
				     x))
			     pattern-to-match))
		 patterns-to-generate)))
    (dolist (planned-line (nd~proof-lines nd*current-proof))
	(when (rule=match-plan-line planned-line new-pattern-to-match)
	  (rule=generate-new-patterns planned-line new-pattern-to-match 
				      new-patterns-to-generate)))))

(defun rule=generate-new-patterns (planned-line pattern-to-match
				   new-patterns-to-generate)
  (declare
   (authors nesmith)
   (input "A planned line, the pattern we have matched, and the new patterns
that should result."))
  (let* ((pattern-supports (cdr pattern-to-match))
	 (real-pattern-supports 
	    (remove-if #'(lambda (x) 
			   (and (symbolp x)
				(string-equal "SS" x)))
		       pattern-supports))
	 (real-supports (nd~line-supports planned-line))
	 (ss-supports 
	  (if (find-if #'(lambda (x) 
			   (and (symbolp x)
				(string-equal "SS" x)))
		       pattern-supports)
	      (set-difference real-supports real-pattern-supports))))
  (dolist (new-pattern-to-generate new-patterns-to-generate)
    (let* ((new-pline (car new-pattern-to-generate))
	   (new-supports (cdr new-pattern-to-generate)))
      (nd~set-line-supports! 
       (if (and (symbolp new-pline)
		(string-equal "PP" new-pline))
	   planned-line
	 new-pline)
       (remove-duplicates 
	(apply #'append
		(mapcar #'(lambda (thing)
			    (if (and (symbolp thing) (string-equal "SS" thing))
				ss-supports
			      (list thing)))
			new-supports))))))))

			    

(defun rule=match-plan-line (plan-line pattern-to-match)
  (declare
   (authors nesmith)
   (input "A plan line in the current proof and pattern of rule action to
 be matched.")
   (value "True if the plan line matches the pattern, nil otherwise."))
  ;; the plan line itself matches if the car of the pattern is PP (wildcard)
  ;; or is the plan line itself
  (when (or (and (symbolp (car pattern-to-match))
		 (string-equal "PP" (car pattern-to-match))
		 (nd~plan-p plan-line))
	    (eq plan-line (car pattern-to-match)))
    ;; in the pattern for the supports, SS acts as a wildcard, matching
    ;; the set of supports that hasn't already matched to something
    ;; this part of the pattern matches if all the explicitly written
    ;; supports in the pattern are also in the supports for the plan-line
    (let* ((pattern-supports (cdr pattern-to-match))
	   (real-pattern-supports 
	    (remove-if #'(lambda (x) 
			   (and (symbolp x)
				(string-equal "SS" x)))
		       pattern-supports))
	   (real-supports (nd~line-supports plan-line)))
      (if (set-difference real-pattern-supports real-supports) 
	  nil
	t))))
				      
    


(defun rule=carry-out-actions (rule mapping)
  (declare
   (authors nesmith)
   (input "A rule and a mapping of from meta-lines to lines.")
   (effect "The patterns of the rule's actions are matched against the
current proof and carried out, changing the structure of the support lines."))
  (let ((actions (rule~actions rule)))
    (dolist (action actions)
      (let ((pattern-to-match (car action))
	    (patterns-to-generate (cdr action)))
	(rule=match-action-pattern-lines mapping pattern-to-match
					 patterns-to-generate)))))


(defun rule~apply (rulename arg-list &optional (rule*verbose t)
			    (really-apply-rule t))
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A rule name,  a list of arguments to the rule,
and optional VERBOSE argument (defaults to T). A second optional
argument REALLY-APPLY-RULE (defaults to T), when NIL, will keep the rule 
from really being applied to the proof; only the new lines will be 
generated and returned along with the lines which would have been modified.")
           (effect  "Applies the rule using the arguments.")
           (value   "If carried out, returns two values: T and a list of
new and modified lines.  If matching failed, returns nil."))
  ;; first we try to match the arguments against the rule preconditions
  ;; and sideconditions
  (let* ((rule (rule~find-rule rulename))
	 (*print-gensym* nil)
	 (mapping (if rule (rule=do-match rule arg-list) 
		    (make-rule=mapping :failed t))))
    (when (rule=mapping-failed mapping)
      (if rule
	  (rule=warn ";;; Can't apply rule ~A with arguments:~{ ~A~}~%" 
		     rulename arg-list)
	(rule=warn ";;; No such rule: ~A~%" rulename))
      (return-from rule~apply nil))
    ;; if the match succeeded, we get working
    (let* ((preconds (copy-list (rule~preconds rule)))
	   (postconds (copy-list (rule~postconds rule)))
	   (new-lines 
	    (rule=order-lines
	     (remove-if 
	     #'(lambda (line)
		 (member (nd~line-label line) preconds 
			 :key #'nd~line-label
			 :test #'string-equal))
	     postconds)))
	   (lines-to-modify (rule=order-lines
			     (set-difference postconds new-lines)))
	   (real-new-lines nil))
      ;; first we make any new lines called for by the rule
      (multiple-value-setq (mapping real-new-lines)
	(rule=make-real-new-lines mapping new-lines))
      ;; modify any lines from the preconditions that also appear in
      ;; the postconditions (changing their justifications)
      (when really-apply-rule
	(setq lines-to-modify (rule=modify-old-lines mapping lines-to-modify))
	;; do the actions part of the rule
	(rule=carry-out-actions rule mapping)
	;; add any new lines to the proof at appropriate places
	(rule=add-new-lines-to-proof real-new-lines lines-to-modify)
	;; show the new lines as well as modified lines we just created, 
	;; if desired
	(when rule*verbose
	  (dolist (line (append real-new-lines lines-to-modify))
	    (nd~show-line line)))
	;; if any modified line is no longer a plan, take it out of
	;; the planned lines for the proof.
	(dolist (line lines-to-modify)
	  (unless (nd~plan-p line)
	    (nd~set-proof-planned-lines! 
	     nd*current-proof
	     (delete line (nd~proof-planned-lines nd*current-proof))))))
      ;; return two values: t (for success) and the list of new/modified lines
      (values t (append real-new-lines lines-to-modify)))))

(defun rule=order-lines (line-list)
; a < b if b uses a as a hyp or a justification
  (sort line-list 
	#'(lambda (x y)
	    (member (nd~line-label x) 
		    (append (nd~line-just-lines y)
			    (nd~line-hyps y))
			:test #'string-equal
			:key #'nd~line-label))))

