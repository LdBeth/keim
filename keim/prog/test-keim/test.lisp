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

(mod~defmod test :uses (mod env post)
                    :documentation "Interface to all test functions."
		    :exports (
			      test~predefined-symbols
			      test~init-predef
			      test~environment
			      test~set-environment!
			      test~read-post
			      test~read-term
			      test~read-type
			      )
		    )

#{\section{Testing KEIM}

This module contains all the functionality to ease interactive test sessions. #}

(defvar test*predefined-symbols 
      '((type-constants o i)
	(type-variables aa bb)
	(constants
	 (= (o aa aa))
	 (false o)
	 (true o)
	 (and (o o o))
	 (or (o o o))
	 (implies (o o o))
	 (equiv (o o o))
	 (not (o o))
	 (forall (o (o aa)))
	 (exists (o (o aa)))
	 (subset (o (o aa) (o aa)))
	 (superset (o (o aa) (o aa)))
	 (union ((o aa) (o aa) (o aa)))
	 (intersection ((o aa) (o aa) (o aa)))
	 (image ((o aa) (o bb) (aa bb)))
	 (defn (o aa aa)))))


#{\subsection{Environments}

One of the problems when getting started with \keim\ is the tedious task of creating an enviornment that
contains all common types, constants. The following functions provide just that#}

(defun test~predefined-symbols ()
  (declare (edited  "13-AUG-1993 16:16" )
	   (authors KOHLHASE )
	   (input  "None.")
	   (effect "None.")
	   (value  "A POST representation of the symbols that TEST~INIT-PREDEF will enter into the test environment."))
  test*predefined-symbols)


(defun test~init-predef ()
  (declare (edited  "16-JUN-1992 08:43" )
	   (authors KOHLHASE )
	   (input   "None.")
	   (effect  "None." )
	   (value   "Creates an environment 
and fills it with the predefined constants."))
  (let ((env (env~create)))
    (post~read-object-list test*predefined-symbols env)
    env))


;For test purposes there exists a global variable containing the last created environment.

(defvar test*environment)


(defun test~environment ()
  (declare (edited  "14-APR-1993 12:02")
	 (authors RICHTS )
	 (input    "None.")
	 (effect   "None.")
	 (value    "The global Testing environment."))
  test*environment)

(defun test~set-environment! (new-environment)
  (declare (edited  "31-AUG-1992 13:44")
	   (authors RICHTS)
	   (input   "An environment.")
	   (effect  "The global environment (only for test purposes) is set to NEW-ENVIRONMENT.")
	   (value   "NEW-ENVIRONMENT which is now the globel environment."))
  (setf test*environment new-environment))



#{\subsection{Reading \post\ expressions}

The functions for reading \post\ expressions are tedious for the use in interacive debugging sessions.
Therefore \keim\ provides a simplified interface for test purposes. These functions mainly differ from those
in the {\vb post} module in the handling of environment which is shortcut in this interface These functions
should only be used for testing and not in actual programs.#}


(defun test~read-post (expression &optional env )
  (declare (edited  "31-AUG-1992 12:14")
	   (authors RICHTS)
	   (input   "A post-expression and an environment, NIL, T or 0. POST-EXPRESSION must be a"
		    "POST expression for <problem>, <position>, <clause>, <literal> or <substitution>." )
	   (effect  "If ENVIRONMENT is NIL then the global environment (only for test purposes) is taken;"
		    "If it is T then a new environment with predefined symbols is created;"
		    "If it is 0 then a new environment without predefined symbols is created;"
		    "If ENVIRONMENT is an environment then it is taken itself."
		    "The choosen environment is changed by the POST-commands and"
		    "the global environment (only for test purposes) is set to the choosen environment."
		    "An error occurs if POST-EXPRESSION is not a correct post-expression in the choosen environment.")
	   (value   "The result of the POST-commands."))
  (when (eq env t)
    (setf env (test~read-post (test~predefined-symbols) (env~create))))
  (unless (env~p env)
    (setf env (test~environment))
    (unless (env~p env)
      (error "KEIM: test~read-post got no environment there is no test-environment (try calling (test~read-post <expr> t))")))
  #|(let ((env (if (env~p env) env
		 (if (eq env t)
		     (progn (test~read-post (test~predefined-symbols) (env~create))
			    (test~read-post expression))
		     (test~environment)))))|#
  (test~set-environment! env)
  (if (and (listp expression) (symbolp (car expression)))
      (post~read-object expression env nil)
      (post~read-object-list expression env)))

#{This function is further specialized to facilitate the treatment of the indicator keywords, which in the
test version the user does not have to remember in this version. For example in a virgin \keim\ with system
{\vb test-keim}, the following commands will provide a term for further use.
\begin{code}
KEIM(1): (test~read-term '(forall (lam (A O) 
				   (forall (lam (B O)
						(implies (and A B) (and B A))))))
			 T)

(FORALL [A].(FORALL [B].(IMPLIES (AND A B) (AND B A))))
KEIM(2):
\end{code}#}

(defun test~read-term (post-term &optional environment)
  (declare (edited  "31-AUG-1992 12:10")
	   (authors RICHTS)
	   (input   "A post-term and an environment, NIL, T or 0.")
	   (effect  "An environment is choosen like in test~read-post."
		    "An error if POST-TERM is not a correct post-term in ENVIRONMENT.")
	   (value   "The term described by POST-TERM and the choosen environment."))
  (car (test~read-post (list (list :existing-term post-term)) environment)))

(defun test~read-type (post-type &optional environment)
  (declare (edited  "31-AUG-1992 12:10")
	   (authors RICHTS)
	   (input   "A post-type and an environment, NIL, T or 0.")
	   (effect  "An environment is choosen like in test~read-post."
		    "An error if POST-TYPE is not a correct post-type in ENVIRONMENT.")
	   (value   "The type described by POST-TYPE and the choosen environment."))
  (car (test~read-post (list (list :existing-type post-type)) environment)))


