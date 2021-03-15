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

(mod~defmod term :uses (keim mod pos post type )
	    :documentation "Basic properties of terms."
	    :exports (
		      term~warning-rename-fn
		      term+top
		      term~reader
		      term~defgeneric
		      term+term
		      term~type
		      term~set-type!
		      term~set-term-binding!
		      term~term-binding
		      term~label
		      term~set-label!
		      term~plist
		      term~set-plist!
		      term~copy
		      term~copy-with-properties
		      term~p
		      term~general-p
		      term~set-term
		      term~termlist-p
		      term~nested-termlist-p
		      term~top
		      term~subterms
		      term~binding
		      term~set-binding!
		      term~equal
		      term~equal-p
		      term~equal-p-ab
		      term~=equal-p-ab-aux
		      term~equal-ab
		      term~=equal-ab-aux
		      term~subterm-positions
		      term~positions
		      term~position
		      term~at-position
		      term~env-lookup
		      term~env-lookup-new
		      term~read
		      term~read-poly
		      term~poly-subterm-find
		      )
	    )

(eval-when (load compile eval)
(defmacro term~warning-rename-fn (oldfn newfn)
  (let ((var (gentemp)))
    `(progn
       (defvar ,var t)
       (defmacro ,oldfn (&rest args)
	 (list 'progn
	 (list 'when ',var
	   (list 'warn "Function ~A has been renamed to ~A. ~
                  Please use that name instead.~%"
		 '',oldfn '',newfn)
	   (list 'setq ',var nil))
	 (list* ',newfn args))))))
)

#{ \section{General terms} \label{mod:term}

This contains only the most general functions concerning terms.
#}

(eval-when (load compile eval)
(defclass term+top () 
  ()
  (:documentation "This class is the uppermost class for terms.  All objects
which could be terms are subclasses of this one.")
  )

(defgeneric term~reader (x)
  (declare
   (authors nesmith)
   (input "An object which represents a logical term.")
   (value "The concrete term that the input represents.
  Terms may come in various forms: normal terms, definitions, lines of a 
  proof, etc.  This function should return the concrete term that the input 
  represents, or else an error.")
   (example "For the case of a normal term: (QQ Y Y)"))
  (:method (x)
   (error "~S does not represent a term." x))
  (:documentation "Given an object which represents a term, returns a
concrete term (subclass of term+term)."))



(defmacro term~defgeneric (name arglist &rest options)
  (declare 
   (authors nesmith)
   (input "Same syntax as DEFGENERIC, with the exception that some 
of the required parameters
of the argument list may be enclosed in parens, signifying that they
are always terms.  No &optional or &keyword parameters may be enclosed in
a list, because we can't defined methods to specialize on them anyway.")
   (effect "Expands into a DEFGENERIC form, with one difference.  A default
method will be created, specializing for the class TERM+TOP on each parameter
which was in a list.  In this new method, the function TERM~READER will be
called on each of the term arguments, and if any of them are 
different afterwards, the function will be called again, replacing the
original term arguments with the value of TERM~READER on them.  To avoid
infinite loops, the function recurses only if some argument has changed.

If no argument is enclosed in a list, then this is just like DEFGENERIC,
and you will get a warning.

One danger is that if the methods YOU define are not as specialized as
the default method that is created, it will be used instead of yours.

Here's an example of such a definition and how it will be expanded.  (Actually,
the expansion is more complicated to allow &optional and &keyword parameters,
but we will simplify it here.)

\\begin{code}
(term~defgeneric print-term ((x) y)
 (:method ((x term+term) y)
  (format t \"~A ~A~%\" x y)))
\\end{code}
expands to 

\\begin{code}
(defgeneric print-term (x y)
 (:method ((x term+term) y)
  (format t \"~A ~A~\%\" x y))
 (:method ((x term+top) y)
  (let ((req-args (list x y))
	(new-args (list (term~reader x) y)))
    (if (every #'eq req-args new-args)
        (error \"Default method defined by TERM~~DEFGENERIC makes no
progress using TERM~~READER: function is ~S, arguments are ~S\"
	       'print-term req-args)
      (apply #'print-term new-args)))))
\\end{code}
")
   (value "Same as DEFGENERIC"))
  (if (every #'symbolp arglist)
      `(progn
	 (warn "TERM~~DEFGENERIC used for ~A, but no term arguments were ~
                declared." ',name)
	 (defgeneric ,name ,arglist ,@options))
    (let* ((lambda-list-keyword-pos
	    (position-if #'(lambda (x) (member x lambda-list-keywords))
			 arglist))
	   (required-args (if lambda-list-keyword-pos
			      (subseq arglist 0 lambda-list-keyword-pos)
			    arglist))
	   (tail-args (if lambda-list-keyword-pos
			  (subseq arglist lambda-list-keyword-pos)
			nil))
	   (tail-args-minus-keywords
	    (let ((tail-args tail-args)
		  (new nil))
	    (loop 
	     (when (null tail-args) (return (nreverse new)))
	     (cond ((eq (car tail-args) '&rest)
		    (setq tail-args (cddr tail-args)))
		   ((member (car tail-args) lambda-list-keywords)
		    (setq tail-args (cdr tail-args)))
		   (t (push (car tail-args) new)
		      (setq tail-args (cdr tail-args)))))))
	   (tail-args-for-lambda-list
	    (mapcar #'(lambda (x)
			(if (member x tail-args-minus-keywords)
			    (list x nil (gensym))
			  x))
		    tail-args))
	   (tail-args-for-funcall
	    (let ((new nil)
		  (optional-p nil))
	    (dolist (x tail-args-for-lambda-list (nreverse new))
	      (cond ((eq x '&optional)
		     (setq optional-p t))
		    ((symbolp x) 
		     (setq optional-p nil))
		    (t (let ((var (car x))
			     (kvar (intern (string (car x))
					   (find-package :keyword)))
			     (svar (third x)))
			 (push
			  (if optional-p
			      `(if ,svar (list ,var) nil)
			    `(if ,svar (list ,kvar ,var) nil))
			    new)))))))
	   )
      `(defgeneric ,name ,(mapcar #'(lambda (x) (if (listp x) (car x) x))
				  arglist)
	 ,@options
	 (:method ,(append (mapcar #'(lambda (x) 
				       (if (listp x)
					   (list (car x) 'term+top)
					 x))
				   required-args)
			   tail-args-for-lambda-list
			   )
	  (let ((req-args (list ,@(mapcar #'(lambda (x) (if (listp x)
							    (car x)
							  x))
					  required-args)))
		(new-required-args 
		 (list ,@(mapcar 
			  #'(lambda (x) (if (listp x) 
					    (list 'term~reader (car x))
					  x))
			  required-args)))
		(optional-and-keyword-args
		 ,(if tail-args-for-funcall
		      (cons 'nconc tail-args-for-funcall)
		    nil)))
	    (if (every #'eq req-args new-required-args)
              (error "Default method defined by TERM~~DEFGENERIC makes no
progress using TERM~~READER: function is ~S, arguments are ~S"
                 ',name 
		 (append req-args optional-and-keyword-args))
	      (apply #',name ,(if tail-args-minus-keywords
				`(nconc new-required-args
					optional-and-keyword-args)
				'new-required-args)))))))))

)

(eval-when (load compile eval)
(defclass term+term (term+top keim+term)
  ((type :initarg :type :reader term=type :writer term=write-type!)
   (sort-system :initarg :sort-system
		:reader term=sort-system
		:writer term=write-sort-system!
		:documentation "The sort system of the term.")
   (sort-information :initarg :sort-information
		     :reader term=sort-information
		     :writer term=write-sort-information!
		     :documentation "A slot where some sort information of the term is kept,
                                     this need not be the fully computed set of sorts, but can also
                                     be some partial results, we make no assumption about the contents of
                                     this slot, it is private to the sort computation algorithms.")
   (binding :initform nil
	    :reader term=term-binding 
	    :writer term=write-term-binding!
	    :documentation "This slot provides the binding mechanism for terms.")
   ;;no new slot, only a new access-function
   (plist :reader term=plist :writer term=write-plist!) 
   (label :initform nil
	  :reader term=term-label
	  :writer term=write-term-label!
	  ))
  (:documentation "This is the superclass to all concrete terms in KEIM")))

(defmethod term~reader ((term term+term))
  (declare
   (authors nesmith)
   (input "A term+term")
   (effect "none")
   (value "The argument is returned. That is \vb{term~reader} is the identity
           on term+term arguments."))
  term)


(term~defgeneric term~type ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
 	   (effect  "None." )
	   (value   "The type of TERM." )
	   (example "Let X be a variable of type i, X --> i"
		    "let QQ be a predicate of type (i,i)->o, (QQ X X) --> o"))
  (:method ((term term+term))
	   (term=type term)))


(term~defgeneric term~set-type! ((term) newtype) 
		 (declare (edited  "16-JUN-1992 16:40" )
			  (authors nesmith )
			  (input   "A term and a type NEWTYPE." )
			  (effect  "Sets the type of the term to NEWTYPE." )
			  (value   "NEWTYPE." )
			  (example "Let Q be a binary predicate of type (i,i)->o"
				   "and X is a variable of type i, the call"
				   "(term~type (Q x x) (type~i)) will set the type of the term (Q x x)) to i, inconsistent with type of the components"))
		 (:method ((term term+term) (newtype type+type))
			  (term=write-type! newtype term)))

(term~defgeneric term~sort-system ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
 	   (effect  "None." )
	   (value   "The sort-system of TERM." ))
  (:method ((term term+term))
	   (term=sort-system term)))


(term~defgeneric term~set-sort-system! ((term) new-sort-system) 
		 (declare (edited  "16-JUN-1992 16:40" )
			  (authors nesmith )
			  (input   "A term and a sort-system NEW-SORT-SYSTEM." )
			  (effect  "Sets the sort-system of the term to NEW-SORT-SYSTEM." )
			  (value   "NEW-SORT-SYSTEM." ))
		 (:method ((term term+term) new-sort-system)
			  (term=write-sort-system! new-sort-system term)))

(term~defgeneric term~sort-information ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
 	   (effect  "None." )
	   (value   "The sort-information of TERM." ))
  (:method ((term term+term))
	   (term=sort-information term)))


(term~defgeneric term~set-sort-information! ((term) new-sort-information) 
		 (declare (edited  "16-JUN-1992 16:40" )
			  (authors nesmith )
			  (input   "A term and a type NEWTYPE." )
			  (effect  "Sets the type of the term to NEW-SORT-INFORMATION." )
			  (value   "NEW-SORT-INFORMATION." ))
		 (:method ((term term+term) new-sort-information)
			  (term=write-sort-information! new-sort-information term)))


(term~defgeneric term~set-term-binding! ((term) binding) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term and a binding, i.e. NIL or a term of the same type." )
	   (effect  "Sets the binding cell of TERM to BINDING." )
	   (value   "Undefined" )
	   (example "Let term1 (Q X X) be of type i, and A a constant of type i"
		    "after calling (term~set-term-binding! term1 a)"
		    "(term~term-binding term1) --> A"))
  (:method ((term term+term) (value (eql nil)))
           (term=write-term-binding! value term))
  (:method ((term term+term) (binding term+term))
	   (if (keim~equal (term~type term) (term~type binding))
	       (term=write-term-binding! binding term)
	       (error "The type of the proposed binding ~A does not match that of ~A" binding term))))



(term~defgeneric term~term-binding ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "The value of the binding cell of TERM." ))
  (:method ((term term+term))
	   (term=term-binding term)))




(term~warning-rename-fn term~term-label term~label)

(term~defgeneric term~label ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "The label of TERM." )
	   (example "The following evaluations are carried out succesively:"
		    "(term~label term1) --> NIL"
		    "(term~set-label! term1 't1)"
		    "(term~label term1) --> T1"))
  (:method ((term term+term))
	   (term=term-label term)))

(term~warning-rename-fn term~set-term-label! term~set-label!)

(term~defgeneric term~set-label! ((term) label) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors huang KOHLHASE )
	   (input   "A term and a label." )
	   (effect  "The label of TERM is set to LABEL." )
	   (value   "Undefined" ))
  (:method ((term term+term) label)
	   (term=write-term-label! label term)))

(term~defgeneric term~plist ((term))
  (declare (edited  "11-SEP-1992 14:06")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "The property list of this term.")
	   (example "(term~set-plist! term1 '(res pres)) --> (RES PRES)"
		    "(term~plist term1) --> (RES PRES)"))
  (:method ((term term+term))
	   (term=plist term)))

(term~defgeneric term~set-plist! ((term) plist)
  (declare (edited  "11-SEP-1992 14:11")
	   (authors RICHTS)
	   (input   "A TERM and list of associations.")
	   (effect  "The property list of TERM is set to PLIST.")
	   (value   "The new property-list of TERM, i.e. PLIST."))
  (:method ((term term+term) plist)
   (term=write-plist! plist term)))


(term~defgeneric term~copy ((object))
  (declare (edited  "12-SEP-1991 14:56")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "A new object equal to object where all subcomponents are copied recursively."
		    "Also the term-bindings remain shared."))
  (:method ((termlist list))
   (mapcar #'term~copy termlist)))

(defun term~copy-with-properties (obj)
  (declare (authors NESMITH)
           (input   "A keim+object.")
           (effect  "Copy the object recursively with term~copy, but also copy 
the property lists of each subobject.  This function uses an :around method on
term~copy to do its dirty deed.")
           (value   "The new copied object."))
  (let ((term*copy-properties t))
    (declare (special term*copy-properties))
    (term~copy obj)))

(defmethod term~copy :around ((obj keim+object))
  (declare (special term*copy-properties))
  (let ((newobj (call-next-method)))	; make the copy recursively
    (when (boundp 'term*copy-properties)
      (keim~set-plist! newobj (copy-list (keim~plist obj))))
    newobj))


(defgeneric term~p (object)
  (declare (edited  "06-DEC-1991 14:04")
	   (authors RICHTS)
	   (input   "A LISP object.")
	   (effect  "None.")
	   (value   "True, iff OBJECT is a concrete term, namely, of class term+term"))
  (:method ((thing t)) nil)
  (:method ((term term+term))
	   t))

(defgeneric term~general-p (object)
  (declare (edited  "06-DEC-1991 14:04")
	   (authors RICHTS)
	   (input   "A LISP object.")
	   (effect  "None.")
	   (value   "True, iff OBJECT is a general (abstract) term, namely, of class term+top"))
  (:method ((thing t)) nil)
  (:method ((term term+top))
	   t))

(term~defgeneric term~set-term ((term1) (term2))
  (declare (edited  "12-SEP-1991 15:03")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "All slots of TERM1 are set to those of TERM2 (the property list is copied).")
	   (value   "The changed TERM1."))
)

(defun term~termlist-p (object)
  (declare (edited  "04-AUG-1991 12:10")
	   (authors RICHTS nesmith)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff object is a non-empty list of terms.")
	   (example "(X (Q X X)) --> T"))
  (and (consp object)
       (term~p (car object)) 
       (or (null (cdr object)) 
	   (term~termlist-p (cdr object)))))


(defun term~nested-termlist-p (object)
  (declare (edited  "04-AUG-1991 12:15")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff object is a nested termlist."
		    "(a simple term, ((((nil)))) and (nil nil nil) are nested termlists, too)."))
  (if (consp object)
      (and (term~nested-termlist-p (car object)) (term~nested-termlist-p (cdr object)))
      (or (null object) (term~p object))))

(term~defgeneric term~top ((term))
  (declare (edited  "02-AUG-1991 20:40")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If TERM is a constant or variable TERM itself,"
		    "if it's an application the applied function of the application in n-normal form,"
                    " that is, if ((f a b) b) is the term then f,"
		    "if it's an abstraction the top of the scope of the abstraction."))
  )


(term~defgeneric term~subterms ((term))
  (declare (edited  "02-AUG-1991 20:46")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "If term is a constant or variable NIL,"
		    "if it's an application a list containing the applied function and the arguments,"
		    "if it's an abstraction a list containing the scope."))
  )


(term~defgeneric term~binding ((term))
  (declare (edited  "18-OCT-1991 17:18")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "An error if TERM is a constant or variable and has a term-binding and a symbol-binding.")
	   (value   "The term-binding or the symbol-binding if TERM has these slots and one of them contains something."))
  (:method ((term term+term))
    (term~term-binding term)))



(term~defgeneric term~set-binding! ((term) new-binding)
  (declare (edited  "26-AUG-1992 13:32")
	   (authors RICHTS)
	   (input   "Terms TERM and NEW-BINDING.")
	   (effect  "The binding cell of TERM is bound to NEW-BINDING.")
	   (value   "Undefined."))
  (:method ((term term+term) new-binding)
	   (term~set-term-binding! term new-binding)))


#{
\subsection{Miscellaneous Functions}
#}


(defmethod keim~equal ((term1 term+term) (term2 term+term))
  (declare (edited  "28-OCT-1992 16:30")
	   (authors nesmith)
	   (input   "Two terms.")
	   (value "True if the terms are TERM~EQUAL."))
  (term~equal term1 term2))


(term~defgeneric term~equal ((term1) (term2))
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2.")
	   (value "This predicate returns T in the following situations:
TERM1 and TERM2 are both constants whose shared symbols are eq;
TERM1 and TERM2 are both variables whose shared symbols are eq;
TERM1 and TERM2 are both applications whose respective heads and arguments
are pairwise term~equal;
TERM1 and TERM2 are both abstractions whose respective bound variables
and scopes are pairwise term~equal.
This is the most stringent form of term~equality.  See the functions
TERM~EQUAL-P, TERM~EQUAL-AB and TERM~EQUAL-P-AB for others.  
This predicate is the default for KEIM~EQUAL when used on terms."))
  (:method ((term1 term+term) (term2 term+term))
    nil))

(term~defgeneric term~equal-p ((term1) (term2))
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2.")
	   (value "This predicate returns T if the terms \"look\" the same,
that is, if they could be constructed by the same POST input.  Symbols 
(variables and constants) are term~equal-p if they have the same type and
their shared symbols have the same name. 
Applications are term~equal-p if their subterms are term~equal-p.
Abstractions are term~equal-p if their bound variables and scopes are
term~equal-p.
See TERM~EQUAL, TERM~EQUAL-AB and TERM~EQUAL-P-AB."))
  (:method ((term1 term+term) (term2 term+term))
    nil))

(defun term~equal-p-ab (term1 term2)
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2.")
	   (value "This predicate returns T if the terms \"look\" the same,
that is, if they could be constructed by the same POST input, with the
additional proviso that two terms that differ only by a change of bound 
variables are still term~equal-p-ab. Symbols (variables and constants) are 
term~equal-p-ab if they have the same type and their shared symbols have the 
same name. 
Applications are term~equal-p-ab if their subterms are term~equal-p-ab.
Abstractions are term~equal-p-ab if their bound variables and scopes are
term~equal-p, up to change of bound variables.  This predicate uses
TERM~=EQUAL-P-AB-AUX as an auxiliary function.
See TERM~EQUAL, TERM~EQUAL-P, and TERM~EQUAL-P-AB."))
  (term~=equal-p-ab-aux term1 term2 nil))

(term~defgeneric term~=equal-p-ab-aux ((term1) (term2) varalist)
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2, and an alist, associating variables in TERM1 to those in TERM2.")
	   (value "This predicate returns T if the terms \"look\" the same,
or are associated by VARALIST."))
  (:method ((term1 term+term) (term2 term+term) (varalist list))
    nil))

(defun term~equal-ab (term1 term2)
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2.")
	   (value "This predicate returns T if the terms are term~equal, with the
additional proviso that two terms that differ only by a change of bound 
variables are still term~equal-ab. Symbols (variables and constants) are 
term~equal-ab if they have the same type and their shared symbols have the 
same name. 
Applications are term~equal-ab if their subterms are term~equal-ab.
Abstractions are term~equal-ab if their bound variables and scopes are
term~equal, up to change of bound variables.  This predicate uses
TERM~=EQUAL-AB-AUX as an auxiliary function.
See TERM~EQUAL, TERM~EQUAL-P and TERM~EQUAL-AB."))
  (term~=equal-ab-aux term1 term2 nil))

(term~defgeneric term~=equal-ab-aux ((term1) (term2) varalist)
  (declare (authors nesmith)
	   (input "Two terms, TERM1 and TERM2, and an alist, associating variables in TERM1 to those in TERM2.")
	   (value "This predicate returns T if the terms are keim~equal,
or are associated by VARALIST."))
  (:method ((term1 term+term) (term2 term+term) (varalist list))
    nil))

#{\subsubsection{Positions in terms}
#}

(defun term~subterm-positions (subterm object &key (test #'keim~equal))
  (declare (edited  "20-AUG-1991 14:11")
	   (authors RICHTS)
	   (input   "A terms, an object and a test-function.")
	   (effect  "None.")
	   (value   "The positions of all occurrences of SUBTERM in OBJECT.")
	   (example "a      (f a (f a a))     ->    ((1) (2 1) (2 2))."
		    "f      (f a (f a a))     ->    ((0) (2 0))."
		    "P      [x].(P x)         ->    ((0 0))."
		    "ALL    (ALL [x].(P a x)) ->    ((0))."
		    "a      (ALL [x].(P a x)) ->    ((1 0 1))."))
  (term~positions object #'(lambda (term) (funcall test subterm term))))

(term~defgeneric term~positions ((object) test)
  (declare (edited  "20-AUG-1991 14:11")
	   (authors RICHTS)
	   (input   "A term and a test-function.")
	   (effect  "None.")
	   (value   "The positions of all subterms in TERM where TEST evaluates to true.")
	   (example "(f a (f a a))     #'fo~constant-p ->    ((1) (2 1) (2 2))."
		    "(f a (f a a))     #'fo~function-p ->    ((0) (2 0))."
		    "(ALL [x].(P x))   #'abstr~p    ->    ((1))."
		    "[x].(P x)         #'appl~p     ->    ((0))."))
  (:method ((object-list list) test)
	   (let* ((i -1)
		  (subpositions
		   (mapcan #'(lambda (object)
			       (incf i)
			       (mapcar #'(lambda (position)
					   (pos~add-front i position))
				       (term~positions object test)))
			   object-list)))
					;(if (funcall test object-list) (cons (pos~empty) subpositions))
	     subpositions))
  (:method ((term term+term) test)
	   (let ((positions (term~positions (term~subterms term) test)))
	     (if (funcall test term)
		 (cons (pos~empty) positions)
		 positions))))

(term~defgeneric term~position ((object) test)
  (declare (edited  "20-AUG-1991 14:11")
	   (authors RICHTS)
	   (input   "An object and a test-function.")
	   (effect  "None.")
	   (value   "The position of the first subobject in OBJECT where TEST evaluates to true.")
	   (example "(f a (f a a))     #'fo~constant-p ->    (1)."
		    "(f a (f a a))     #'fo~function-p ->    (0)."))
  (:method ((object-list list) test)
   (if (funcall test object-list)
       (pos~empty)
       (let ((i -1))
	 (some #'(lambda (object)
		   (let ((subposition (term~position object test)))
		     (incf i)
		     (if subposition
			 (pos~add-front i subposition)
			 nil)))
	       object-list))))
  (:method ((term term+term) test)
   (if (funcall test term)
       (pos~empty)
       (let ((i -1))
	 (some #'(lambda (subterm)
		   (let ((subposition (term~position subterm test)))
		     (incf i)
		     (if subposition
			 (pos~add-front i subposition)
			 nil)))
	       (term~subterms term))))))

(term~defgeneric term~at-position ((object) position)
  (declare (edited  "24-JUN-1992 10:06" )
	   (authors KOHLHASE )
	   (input   "A term or list of terms and a position.")
	   (effect  "None.")
	   (value   "The (list) of subtems of OBJECT at the position POSITION.")
	   (example "(QQ Y Y) [2] --> Y"))
  (:method ((object-list list) position)
   (if (pos~empty-p position)
       object-list
       (term~at-position (elt object-list (pos~first position)) (pos~rest position))))
  (:method ((term term+term) position)
   (if (pos~empty-p position)
       term
       (let ((number (pos~first position))
	     (subterms (term~subterms term)))
	 (if (> number (length subterms))
	     (error "Position ~A not in term ~A." position term)
	     (term~at-position (elt subterms number) (pos~rest position)))))))


(defun term~env-lookup (term env)
  (declare
   (authors nesmith)
   (input  "A POST representation of a term and an environment.")
   (effect "none")
   (value  "Interprets the input in the environment and constructs and returns the corresponding term."))
  (post~read-object term env :existing-term))

(defun term~env-lookup-new (term env)
  (declare
   (authors nesmith)
   (input  "A POST representation of a term and an environment.")
   (effect "none")
   (value  "Interprets the input in the environment and constructs "
	   "the corresponding term.  Makes a copy of the result before returning it (so"
	   "the result is always new)."))
  (term~copy (term~env-lookup term env)))

(defun term~read (term env)
  (declare 
   (authors nesmith)
   (effect "")
   (input "A POST representation of a term, and an environment.")
   (value "A term if one can be parsed from the input.  This term must
have ground, not variable, types, or an error is signaled.")
   (example "Let A be a constant in the environment env, (term~read '(a) env) --> A"
	    "Let Q and QQ be unary and binary functions in env, the constant a and the logical connectives are also in env,"
	    "(term~read '((EQUIV (Q a) (FORALL (lam (X I) (IMPLIES (Q x) (QQ x x)))))) env) -->"
	    "(EQUIV (Q a) (FORALL [X].(IMPLIES (Q X) (QQ X X))))"))
  (let* ((obj (term~read-poly term env))
	 (poly-subterm (term~poly-subterm-find obj)))
    (if poly-subterm
	(post~error "Term read: ~S contains a subterm ~S of polymorphic type 
~S, which is not allowed." obj poly-subterm (term~type poly-subterm))
      obj)))

(defun term~read-poly (term env)
  (declare 
   (authors nesmith)
   (effect "")
   (input "A POST representation of a term, and an environment.")
   (value "A term, possibly containing variable types, if one can be 
parsed from the input.")
   (example "Let type1 be a type variable,"
            "(term~read-poly `((EQUIV (Q a) (FORALL (lam (X type1) (IMPLIES (Q x) (QQ x x)))))) env)"
	    "(EQUIV (Q A) (FORALL [X].(IMPLIES (Q X) (QQ X X))))"))
  (post~read-object term env :existing-term))


(term~defgeneric term~poly-subterm-find ((term))
  (declare (authors nesmith)
	   (input "A term")
	   (effect "none")
	   (value "NIL if all symbols in the term have ground type, otherwise
the first subterm that has polymorphic type."))
  (:method ((term term+term))
     (unless (type~ground-p (term~type term))
       term))
  (:documentation "Recurses through the term, checking to see that all
symbols have ground type, returns first subterm that doesn't."))
