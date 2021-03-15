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

(mod~defmod assum :uses (env keim mod post term termix )
	    :exports (
		      assum+assumption
		      assum~p
		      assum~formula
		      assum~create
		      )
	    )

#{
\section{Assumptions}
A particular type of named term is the assumption.  This stands for the
assumption in a proof, for example, the axioms from which we wish to
prove a theorem.
#}

; rename term+assumption -> assum+assumption

(eval-when (load compile eval)
(defclass assum+assumption (termix+named-term)
    ()
    (:documentation "The class of assumptions.")))

(term~warning-rename-fn  term~assumption-p  assum~p)

(defun assum~p (object)
  (declare 
   (authors nesmith)
   (input   "An object")
   (effect  "none")
   (value   "T if the object is an assumption, otherwise nil."))
  (typep object 'assum+assumption))

(term~warning-rename-fn  term~assumption-formula  assum~formula)

(defun assum~formula (thing)
  (termix~term thing))

(term~warning-rename-fn  term~assumption-create  assum~create)

(defun assum~create (name term)
  (declare 
   (authors nesmith)
   (input   "A symbol and a term")
   (effect  "none")
   (value   "Creates and returns an assumption with given name and term
values for the slots."))
  (make-instance 'assum+assumption :name name :term term))

(defmethod term~copy ((assumption assum+assumption))
  (assum~create 
   (keim~name assumption) 
   (term~copy 
    (assum~formula assumption))))

(defmethod termix~named-term-abbrev ((term assum+assumption))
  "ass")

(defmethod post~print ((assumption assum+assumption) stream)
  (format stream "(assumption ~A " (keim~name assumption))
  (post~print (termix~term assumption) stream)
  (format stream ")"))



(defmethod post~read-object ((thing list) (env env+environment) 
			    (indicator (eql :assumption)))
  (let ((name (post~read-symbol (car thing) env)))
    (if (rest thing)
	(let* ((term (term~env-lookup (cadr thing) env))
	       (assump (assum~create name term)))
	  (env~enter name assump env)
	  assump)
	(let ((assump (env~lookup-object name env)))
	  (unless (assum~p assump)
	    (error "No assumption associated with key ~A in environment" name))
	  assump))))


#{Assumptions have the \post\ syntax 
\begin{postsyntax}
\syntax{
\nt{assumption}    ::= (assumption \nt{name} \nt{formula}).
}

\end{postsyntax} 

#}

