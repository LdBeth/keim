;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(mod~defmod meta :uses (abstr appl env keim mod poly post 
			      shsym sym sys term type )
	    :documentation "Metavariables"
	    :exports (
		      meta+var
		      meta+polyvar
		      meta~p
		      meta~read-term
		      meta~read-poly-term
		      meta~match
		      meta~match-bind
		      meta~meta-subst
		      meta+error
		      meta+mismatch
		      meta~mismatch-expected
		      meta~mismatch-found
		      meta+mismatch-type
		      meta~mismatch-type-expected
		      meta~mismatch-type-found
		      meta+illegal-match
		      meta~illegal-match-wffschema
		      meta~illegal-match-wffvalue
		      meta~illegal-match-gwff
		      meta~variable-create
		      )
	    )
		   
#{\section{Metavariables}\label{mod:meta}
\subsection{Introduction}
Metavariables are variables that stand in for other terms.  They are thus
used in {\vb META~MATCH-BIND} and {\vb META~META-SUBST}.  The class
META+VAR is used to distinguish these variables. It is a direct subclass
of SYM+VAR.

A complication comes when these variables are also polymorphic (have nonground
types).  These variables are in the class META+POLYVAR, which is a subclass of
POLY+POLY and  META+VAR. 
#}


(eval-when (load compile eval)
(defclass meta+var (sym+var)
  ()
  (:documentation "A metavariable.")))


(eval-when (load compile eval)
(defclass meta+polyvar (poly+poly meta+var)
  ()
  (:documentation "A metavariable that is also polymorphic.")))

(defun meta~p (thing)
  (declare (authors nesmith)
	   (input "A lisp object THING.")
	   (effect "None")
	   (value "T if the thing is a meta-variable, nil otherwise.")
	   (example "(meta~read-term '(meta-x) env) --> T"))
  (typep thing 'meta+var))

(defmethod post~read-object (terms env (indicator (eql :meta-variables)))
  (dolist (term terms nil)
    (post~read-object term env :meta-variable)))

(defmethod post~read-object (term env (indicator (eql :meta-variable)))
  (let ((meta*read-meta-wff t))
    (declare (special meta*read-meta-wff t))
    (post~read-object term env :variable-multiple)))

(defmethod env~post-print (key (meta meta+var) stream)
  (declare (ignore key))
  (format stream "~&(meta-variables (~A " (keim~name meta))
  (post~print (term~type meta) stream)
  (format stream "))~%")
  (values))

(defmethod type~subst-apply ((subst type+substitution) (term meta+polyvar))
  (let ((newtype (type~subst-apply subst (term~type term))))
    (poly~instantiate term newtype)))

(defmethod initialize-instance :after ((term sym+var) &rest rest)
  (declare (special meta*read-meta-wff)
	   (ignore rest))
  (if (and (boundp 'meta*read-meta-wff)
	   meta*read-meta-wff)
      (cond ((meta~p term) term)
	    ((poly~p term) (change-class term 'meta+polyvar))
	    (t (change-class term 'meta+var)))
    term))


(defun meta~read-term (thing env)
  (declare (authors nesmith)
	   (input "A thing and an environment, which contains the thing.")
	   (effect "None")
	   (value "The thing is read in as a term, with all variables"
		  "being declared as meta-variables.")
	   (example "Let env be an environment, e.g. created by (env~create), containing the meta-variable"
		    "X, e.g. added by (post~read-object-list '((meta-variables (x i))) env), then"
		    "X  env --> X"))
  (let ((meta*read-meta-wff t))
    (declare (special meta*read-meta-wff))
    (term~read thing env)))

(defun meta~read-poly-term (thing env)
  (declare (authors nesmith)
	   (input "A thing and an environment.")
	   (effect "None")
	   (value "The thing is read in as a polymorphic term, with all"
		  "variables being declared as polymorphic meta-variables.")
	   (example "Let env be an environment, e.g. created by (env~create), containing the"
		    "polymorphic variable X then"
		    "X  env --> X"))
  (let ((meta*read-meta-wff t))
    (declare (special meta*read-meta-wff))
    (term~read-poly thing env)))

#{
\subsection{Matching meta-formulas}
META~MATCH matches a meta-term (one which may contain meta-variables)
against a normal term (in which all meta-variables will be ignored). 
It returns an association list binding meta-variables to terms and
type variables to constant types. 

META~SUBST will apply such a binding list to a meta-term to produce
an instantiated term.
#}


(defun meta~match (wffschema gwff &optional (bindings nil) 
					    (additional-vars nil))
  (declare (authors nesmith)
	   (input "A term WFFSCHEMA (possibly containing meta-variables),"
                  "a term GWFF (which is treated as constant) and optional"
		  "BINDINGS (defaults to NIL), an a-list associating meta-variables"
		  "to terms and type-variables to types. It is assumed that"
		  "BINDINGS represents an idempotent substitution."
		  "Also as optional argument is a list of other term symbols which"
		  "are to be treated as meta-variables during the matching process."
		  "This allows the use of matching without changing classes of terms.")
	   (effect "None")
	   (value "An association list of meta-variables to terms and of type-variables"
		  "to types which represents a substitution, that if applied to"
		  "WFFSCHEMA by META~META-SUBST results in a term TERM~EQUAL-P to GWFF.")
	   (example "Let P be a binary predicate variable, F a unary function constant and"
		    "MX and MY be meta-variables and C a constant"
		    "(P MX MY)       (P (F C) C) --> ((MY . C) (MX . (F C)))"
		    ""
		    "(P MX MY)       (Q (F C) C) --> NIL"
		    ""
		    "(P MX MY)       (Q (F C) C)    NIL   (LIST 'P)--> ((Y . C) (X . (F C)) (P . Q))"
		    ""
		    "(P MX MY)       (Q (F C) C)    (LIST (MY . C))   (LIST 'P)"
		    "--> ((X . (F C)) (P . Q) (Y . C))"))
  (meta~match-bind wffschema gwff bindings additional-vars))


(defun meta=update-bindings (x y alist)
  (declare
   (authors nesmith)
   (input "A metavariable X, a term Y to associate with X, and a association list"
	  "representing a substitution")
   (value "The alist is changed by adding the pair (X . Y), and all previously-existing"
	  "pairs are updated by applying the substitution Y for X to them."))
  (acons x y
	 (mapcar #'(lambda (pair)
		     (cons (car pair)
			   (meta~meta-subst (cdr pair) (list (cons x y)))))
		 alist)))


(defgeneric meta~match-bind (wffschema gwff bindings 
			     &optional additional-vars)
  (declare (authors nesmith)
	   (input "A term WFFSCHEMA (possibly containing meta-variables),"
		  "a term GWFF (which is treated as constant) and BINDINGS, an"
		  "a-list associating meta-variables to terms and type-variables to types."
		  "It is assumed that BINDINGS represents an idempotent substitution."
		  "An optional parameter is ADDITIONAL-VARS, a list of term symbols"
		  "which should be treated as metavariables during the match process,"
		  "which may be normal terms.")
	   (effect "None")
	   (value "An association list of metavariables to terms and of"
		  "type-variables to types which represents a substitution, that if applied to"
		  "WFFSCHEMA by META~META-SUBST results in a term TERM~EQUAL-P to GWFF.")
	   (example "compare procedure META~MATCH, only difference here bindings obligatory"))
  (:method ((wffschema term+term) (gwff term+term) bindings &optional additional-vars)
      (declare (ignore bindings additional-vars))
    (meta=report-mismatch wffschema gwff))
  (:method ((wffschema sym+sym) (gwff sym+sym) bindings &optional additional-vars)
      (if (member wffschema additional-vars :test #'term~equal)
	  (let ((wffvalue (meta=wffeval wffschema bindings)))
	    (if (not (eq wffvalue wffschema))
		(meta~match-bind wffvalue gwff bindings additional-vars)
	      (if (keim~equal (term~type wffschema) (term~type gwff))
		  (meta=update-bindings wffschema gwff bindings)
		(meta=report-mismatch-type (meta~meta-subst wffschema bindings)
					   gwff))))
	(if (meta=same-const-p wffschema gwff)
	    bindings
	  (meta=report-mismatch wffschema gwff))))
  (:method ((wffschema appl+appl) (gwff appl+appl) bindings &optional additional-vars)
      (let ((wffschema-args (appl~arguments wffschema))
	    (gwff-args (appl~arguments gwff))
	    (wffschema-fun (appl~function wffschema))
	    (gwff-fun (appl~function gwff)))
	(cond 
	 ((> (length wffschema-args) (length gwff-args))
	  (meta=report-mismatch wffschema gwff))
	 (t				; more gwff-args than wffschema-args
	  (setq bindings
	    (meta~match-bind wffschema-fun
			     (appl~create
			      gwff-fun
			      (subseq gwff-args 0 
				      (- (length gwff-args)
					 (length wffschema-args))))
			     bindings additional-vars))
	  (mapc #'(lambda (x y) 
		    (setq bindings (meta~match-bind x y bindings additional-vars)))
		wffschema-args
		(nthcdr (- (length gwff-args)
			   (length wffschema-args))
			gwff-args))
	  bindings))))
  (:method ((wffschema abstr+abstr) (gwff abstr+abstr) bindings &optional additional-vars)
      (meta~match-bind (abstr~scope wffschema)
       (abstr~scope gwff)
       (meta~match-bind (abstr~bound-variable wffschema)
			(abstr~bound-variable gwff)
			bindings additional-vars) additional-vars))
  (:method ((wffschema meta+var) (gwff term+term) bindings &optional additional-vars)
      (let ((wffvalue (meta=wffeval wffschema bindings)))
	(if (not (eq wffvalue wffschema))
	    (meta~match-bind wffvalue gwff bindings additional-vars)
	  (if (keim~equal (term~type wffschema) (term~type gwff))
	   (meta=update-bindings wffschema gwff bindings)
	   (meta=report-mismatch-type (meta~meta-subst wffschema bindings)
				      gwff)))))
  (:method ((wffschema poly+poly) (gwff poly+poly) bindings &optional additional-vars)
    (declare (ignore additional-vars))
      (cond 
       ((not (string= (keim~name wffschema) (keim~name gwff)))
	(meta=report-mismatch wffschema gwff))
       ((keim~equal (term~type wffschema) (term~type gwff))
	bindings)
       ((type~ground-p (term~type wffschema))
	(meta=report-mismatch-type wffschema gwff))
       (t
	(let ((sigma (type~unify (meta~meta-subst (term~type wffschema)
						  bindings)
				 (term~type gwff))))
	    (if sigma
		(let* ((domain (type~subst-domain sigma))
		       (codomain (type~subst-codomain sigma)))
		  (pairlis domain codomain bindings))
	      (meta=report-mismatch-type (meta~meta-subst wffschema bindings)
					 gwff))))))
  (:method ((wffschema meta+polyvar) (gwff term+term) bindings  &optional additional-vars)
      (let ((wffvalue (meta=wffeval wffschema bindings)))
	(if (not (eq wffvalue wffschema))
	    (meta~match-bind wffvalue gwff bindings additional-vars)
	  (let ((sigma (type~unify (meta~meta-subst (term~type wffschema)
						    bindings)
				   (term~type gwff))))
	    (if sigma
		(let* ((domain (type~subst-domain sigma))
		       (codomain (type~subst-codomain sigma))
		       (newbindings (pairlis domain codomain 
					     bindings)))
		  (meta=update-bindings
		   wffschema gwff
		   newbindings))
	      (meta=report-mismatch-type (meta~meta-subst 
					  wffschema bindings)
					 gwff))))))
  )




(defgeneric meta=same-const-p (const1 const2)
  (declare (authors nesmith)
	   (input "Two term symbols.")
	   (effect "None")
	   (value "Tests general equality (such that a match between"
		  "these symbols is considered successful."))
  (:method ((const1 term+term) (const2 term+term))
      (term~equal-p const1 const2)))




(defgeneric meta~meta-subst (wffschema bindings)
  (declare (authors nesmith)
	   (input "A term WFFSCHEMA (possibly containing meta-variables), and BINDINGS, an"
		  "alist associating meta-variables to terms and type-variables to types."
		  "It is assumed that BINDINGS represents an idempotent substitution.")
	   (effect "None")
	   (value "The meta-variables and polymorphic variables in WFFSCHEMA"
		  "are instantiated using BINDINGS and the resulting term is returned.")
	   (example "(P MX MY)    ((MY . C) (MX . (F C))) --> (P (F C) C)"))
  (:method ((wffschema term+term) bindings)
      (declare (ignore bindings))
	  wffschema)
  (:method ((type type+type) bindings)
      (let ((typesubst
	     (apply #'type~substitution-create
	      (let ((domain nil) (codomain nil))
		(dolist (pair bindings (list domain codomain))
		  (when (type~variable-p (car pair))
		    (push (car pair) domain)
		    (push (cdr pair) codomain)))))))
	(type~subst-apply typesubst type)))
  (:method ((wffschema sym+sym) bindings)
    (meta=wffeval wffschema bindings))
  (:method ((wffschema abstr+abstr) bindings)
	  (let* ((old-var (abstr~bound-variable wffschema))
		 (new-var (meta~meta-subst old-var bindings))
		 (old-scope (abstr~scope wffschema))
		 (new-scope (meta~meta-subst 
			     (abstr~scope wffschema) 
			     (acons old-var new-var bindings))))
	    (if (and (eq new-scope old-scope) (eq old-var new-var))
		wffschema
	      (abstr~create new-var new-scope))))
  (:method ((wffschema appl+appl) bindings)
	  (let* ((old-fun (appl~function wffschema))
		 (new-fun (meta~meta-subst old-fun bindings))
		 (old-args (appl~arguments wffschema))
		 (new-args (mapcar #'(lambda (x) (meta~meta-subst x bindings))
				   old-args))
		 (use-new-fun (not (eq new-fun old-fun)))
		 (use-new-args 
		  (dotimes (i (length old-args) nil)
		    (unless (eq (nth i old-args) (nth i new-args))
		      (return t)))))
	    (if (or use-new-fun use-new-args)
		(appl~create
		 new-fun
		 new-args)
	      wffschema)))
  (:method ((wffschema meta+var) bindings)
      #+rule-debug(format t "~%Meta-thing: ~S~%" wffschema)
    (meta=wffeval wffschema bindings))
  (:method ((wffschema poly+poly) bindings)
      #+rule-debug(format t "~%Poly-thing: ~S~%" wffschema)
    (if (type~ground-p (term~type wffschema))
	wffschema
      (let ((sigma 
	     (let ((domain nil)
		   (codomain nil))
	       (dolist (pair bindings 
			 (type~substitution-create domain codomain))
		 (when (type~p (car pair))
		   (push (car pair) domain)
		   (push (cdr pair) codomain))))))
	(type~subst-apply sigma wffschema))))
  (:method ((wffschema meta+polyvar) bindings)
     #+rule-debug(format t "~%meta-poly-thing: ~S~%" wffschema)
    (let ((wffval (meta=wffeval wffschema bindings)))
      (if (not (eq wffval wffschema))
	  wffval
	(if (type~ground-p (term~type wffschema))
	    wffschema
	  (let ((sigma (let ((domain nil)
			     (codomain nil))
			 (dolist (pair bindings 
				   (type~substitution-create domain codomain))
			   (when (type~p (car pair))
			     (push (car pair) domain)
			     (push (cdr pair) codomain))))))
	    #+rule-debug(format t "~%type-subst: ~S" sigma)
	    (type~subst-apply sigma wffschema))))))
)







#{\subsection{Error handling}
We define the following conditions.
\begin{description}
\item[META+ERROR] A subclass of SYS+ERROR.
\item[META+MISMATCH] A subclass of META+ERROR, used for reporting general 
failures in matching two terms.
\item[META+MISMATCH-TYPE] A subclass of META+ERROR, used for reporting failures
in matching when two terms differ in type.
\end{description}

By handling the condition META+ERROR (with SYS~HANDLER-BIND, for 
example), you can detect and appropriately handle 
all matching failures.  This depends on the Lisp being able to do this.
#}



(sys~define-condition meta+error (sys+error)
		      ()
		      (lambda (condition stream)
			(declare (ignore condition))
			(format stream "~%Some kind of error in metavariable matching.")))


(sys~define-condition meta+mismatch (meta+error)
  ((expected) (found))
  (lambda (condition stream)
    (format stream "~%;;;Wff mismatch.  Meta-term is ")
    (post~print (meta+mismatch-expected condition) stream)
    (format stream ", but real term is ")
    (post~print (meta+mismatch-found condition) stream)))

(defmacro meta~mismatch-expected (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+mismatch, its EXPECTED slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch :expected 'A :found 'B) --> A"))
  `(meta+mismatch-expected ,err))

(defmacro meta~mismatch-found (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+mismatch, its FOUND slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch :expected 'A :found 'B) --> B"))
  `(meta+mismatch-found ,err))


(sys~define-condition meta+mismatch-type (meta+error)
  ((expected) (found))
  (lambda (condition out)
	     (let ((expect (meta+mismatch-type-expected condition))
		   (found (meta+mismatch-type-found condition)))
	     (princ "~%;;;Type mismatch.  Meta-term " out)
	     (post~print expect out)
	     (princ " must have type " out)
	     (post~print (term~type expect) out)
	     (princ " while real term " out)
	     (post~print found out)
	     (princ " has type " out)
	     (post~print (term~type found) out)
	     (princ "." out)
	     (terpri out))))
 

(defmacro meta~mismatch-type-expected (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+mismatch-type, its EXPECTED slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch-type :expected 'A :found 'B) --> A"))
  `(meta+mismatch-type-expected ,err))

(defmacro meta~mismatch-type-found (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+mismatch-type, its FOUND slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+mismatch-type :expected 'A :found 'B) --> B"))
  `(meta+mismatch-type-found ,err))


(sys~define-condition meta+illegal-match (meta+error)
  ((wffschema) (wffvalue)(gwff))
  (lambda (condition stream)
    (princ "~%;;; Illegal Match.  " stream)
    (post~print (meta+illegal-match-wffschema condition)
		stream)
    (princ " matched " stream)
    (post~print (meta+illegal-match-wffvalue condition) stream)
    (princ " but now matches " stream)
    (post~print (meta+illegal-match-gwff condition) stream)))

(defmacro meta~illegal-match-wffschema (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is a meta+illegal-match, its WFFSCHEMA slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> A"))
  `(meta+illegal-match-wffschema ,err))

(defmacro meta~illegal-match-wffvalue (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+illegal-match, its WFFVALUE slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> B"))
  `(meta+illegal-match-wffvalue ,err))

(defmacro meta~illegal-match-gwff (err)
  (declare
   (authors nesmith)
   (input "Any object.")
   (value "If the object is an meta+illegal-match, its GWFF slot will be"
	  "returned. Otherwise an error will result.")
   (example "(sys~make-condition 'meta+illegal-match :wffschema 'A :wffvalue 'B :gwff 'C) --> C"))
  `(meta+illegal-match-gwff ,err))


(defun meta=report-mismatch (expect found)
  (sys~signal (sys~make-condition 'meta+mismatch :expected expect :found found)))

(defun meta=report-mismatch-type (expect found)
  (sys~signal 
   (sys~make-condition 'meta+mismatch-type :expected expect :found found)))


(defun meta=wffeval (meta-var bindings)
  (or (cdr (assoc meta-var bindings
		  :test (if (poly~p meta-var) 
			    #'(lambda (x y) (and (poly~p y)
						 (eq (poly=instance-obj x)
						     (poly=instance-obj y))))
			  ;#'eql
			  #'(lambda (x y)
			      (and (term~p y) (term~equal x y))))))
      meta-var))

#{\subsection{Auxiliary functions}
#}

(defgeneric meta~variable-create (thing type)
  (declare 
   (authors nesmith)
   (input   "A term symbol, a shared symbol or a symbol, and a type.")
   (effect  "none")
   (value   "Creates a new meta-variable using the name of the given symbol and the"
	    "type and returns it.")
   (example "'X   (TYPE~I) --> X)"))
  (:method ((symbol symbol) (type type+type))
      (let ((meta*read-meta-wff t)
	    (shsym 
	     (make-instance 'shsym+sharedsymbol :name (string symbol) 
			    :type type)))
	(declare (special meta*read-meta-wff))
	(make-instance 'sym+var :symbol shsym :type type)))
  (:method ((symbol shsym+sharedsymbol) (type type+type))
      (let ((meta*read-meta-wff t)
	    (shsym 
	     (make-instance 'shsym+sharedsymbol :name (keim~name symbol)
			    :type type)))
	(declare (special meta*read-meta-wff))
	(make-instance 'sym+var :symbol shsym :type type)))
  (:method ((symbol sym+sym) (type type+type))
      (let ((meta*read-meta-wff t)
	    (shsym 
	     (make-instance 'shsym+sharedsymbol :name (keim~name symbol)
			    :type type)))
	(declare (special meta*read-meta-wff))
	(make-instance 'sym+var :symbol shsym :type type))))

		

(defmethod term~copy ((term meta+var))
  (let ((newterm (call-next-method)))
    (change-class newterm 'meta+var)
    newterm))

(defmethod term~copy ((term meta+polyvar))
  (let ((newterm (call-next-method)))
    (change-class newterm 'meta+polyvar)
    newterm))
