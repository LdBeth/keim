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

(mod~defmod termix :uses (env keim mod post term )
	    :documentation "Using terms in larger structures"
	    :exports (
		      termix+mixin
		      termix~set-term!
		      termix~term
		      termix+named-term
		      termix~named-term-p
		      termix~read-named-term
		      termix~named-term-abbrev
		      )
	    )

#{
\section{Term mixins}\label{mod:termix}
 Often we would like to have objects which behave as terms, but also
 have other properties (such as names).  Here TERMIX+MIXIN is a new 
 KEIM+OBJECT
 that contains a slot TERM, where a term can be placed.
#}

; rename term+mixin -> termix+mixin

(eval-when (load compile eval)
(defclass termix+mixin (term+top keim+object)
  ((term :initform nil :initarg :term :reader termix=term
	 :writer termix=write-term!))
  (:documentation "An object which contains a term as a slot.")
  )
)


(term~warning-rename-fn  term~set-mixin-term!  termix~set-term!)

(defgeneric termix~set-term! (term newval)
  (declare 
   (authors nesmith)
   (input   "A term-mixin object and a new term.")
   (effect  "Sets the TERM slot of the term-mixin to the new term.")
   (value   "the new  term")
   (example "Let mixin1 is an object of class termix+mixin"
	    "(termix~set-term! mixin1 (QQ Y Y)) --> (QQ Y Y)"
	    "(termix~term mixin1) --> (QQ Y Y)"))
  (:method ((term termix+mixin) (newval term+term))
	   (termix=write-term! newval term)))

(term~warning-rename-fn  term~mixin-term  termix~term)

(defun termix~term (term)
  (declare 
   (authors nesmith)
   (input   "A term-mixin object.")
   (effect  "none")
   (value   "the value of the TERM slot."))
  (termix=term term))

(defmethod term~reader ((term termix+mixin))
  (termix~term term))

(defmethod print-object ((thing termix+mixin) stream)
  (print-object (termix~term thing) stream))

#{
\section{Named terms}
 Here we define TERMIX+NAMED-TERM.  Instances of this class will
 contain a name slot as well as a term slot, so we can associate
 names with terms.
#}

; rename term+named-term -> termix+named-term 

(eval-when (load compile eval)
(defclass termix+named-term (termix+mixin keim+name)
    ()
  (:documentation "This is the superclass to all terms in KEIM that have a name")))

(term~warning-rename-fn  term~named-term-p  termix~named-term-p)

(defun termix~named-term-p (thing)
  (declare 
   (authors nesmith)
   (input   "An object")
   (effect  "none")
   (value   "T if the object is a named-term, otherwise nil."))
  (typep thing 'termix+named-term))

(defmethod print-object :around ((named-term termix+named-term) stream)
	   (format stream "(~A ~S " (termix~named-term-abbrev named-term) 
		   (keim~name named-term))
	   (call-next-method)
	   (format stream ")"))

(term~warning-rename-fn  term~read-named-term  termix~read-named-term)

(defun termix~read-named-term (symbol env)
  (declare 
   (authors nesmith)
   (input   "A symbol and a environment")
   (effect  "none")
   (value   "If the symbol is associated with a named-term in the environment,
returns the named-term."))
  (let ((obj (env~lookup-object symbol env)))
    (unless (termix~named-term-p obj)
      (post~error "~A is not a named term in environment." symbol))
    obj))

(term~warning-rename-fn  term~named-term-abbrev  termix~named-term-abbrev)

(defgeneric termix~named-term-abbrev (named-term)
  (declare 
   (authors nesmith)
   (input   "A named-term")
   (effect  "none")
   (value   "A short string identifying the specific type of the named-term 
(used in printing)."))
  (:method ((thing termix+named-term))
	   "namedterm")
  (:documentation "A string abbreviation for this type of named of term. Used in printing."))

(defmethod env~post-print (key (term termix+named-term) stream)
  (declare (ignore key))
  (post~print term stream)
  (values))


