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

(mod~defmod conc :uses (env keim mod post term termix )
	    :documentation  "Datastructures and basic functionality for conclusions of problems."
	    :exports (
		      conc+conclusion
		      conc~p
		      conc~formula
		      conc~create
		      )
	    )

#{
\section{Conclusions}
A particular type of named term is the conclusion.  This stands for the conclusion in a proof, that is, the
proposition we wish to prove, given a particular set of assumptions.  #}

; rename term+conclusion -> conc+conclusion

(eval-when (load compile eval)
(defclass conc+conclusion (termix+named-term)
    ()
    (:documentation "The class of conclusions.")))

(term~warning-rename-fn  term~conclusion-p  conc~p)

(defun conc~p (object)
  (declare 
   (authors nesmith)
   (input   "An object")
   (effect  "none")
   (value   "T if the object is an conclusion, otherwise nil."))
  (typep object 'conc+conclusion))

(term~warning-rename-fn  term~conclusion-formula  conc~formula)

(defun conc~formula (thing)
  (termix~term thing))

(term~warning-rename-fn  term~conclusion-create  conc~create)

(defun conc~create (name term)
  (declare 
   (authors nesmith)
   (input   "A symbol and a term")
   (effect  "none")
   (value   "Creates and returns an conclusion with given name and term
values for the slots."))
  (make-instance 'conc+conclusion :name name :term term))

(defmethod term~copy ((conclusion conc+conclusion))
  (conc~create 
   (keim~name conclusion) 
   (term~copy 
    (conc~formula conclusion))))

(defmethod termix~named-term-abbrev ((term conc+conclusion))
  "conc")

(defmethod post~print ((conclusion conc+conclusion) stream)
  (format stream "(conclusion ~A " (keim~name conclusion))
  (post~print (termix~term conclusion) stream)
  (format stream ")"))


(defmethod post~read-object ((thing list) (env env+environment) 
			    (indicator (eql :conclusion)))
  (let ((name (post~read-symbol (car thing) env)))
    (if (rest thing)
	(let* ((term (term~env-lookup (cadr thing) env))
	       (conc (conc~create name term)))
	  (env~enter name conc env)
	  conc)
	(let ((conc (env~lookup-object name env)))
	  (unless (conc~p conc)
	    (error "No conclusion associated with key ~A in environment" name))
	  conc))))

#{Conclusions have the \post\ syntax 
\begin{postsyntax}
\syntax{
\nt{conclusion}    ::= (conclusion \nt{name} \nt{formula}).}
\end{postsyntax} 

#}

