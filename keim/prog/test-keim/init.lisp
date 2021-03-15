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

(mod~defmod init :uses (mod env post)
                    :documentation "Interface to all initializing functions."
		    :exports 
		    (init~initialize-keim
		     )
)

;;;begin{latex}
;;;\chapter{Initializing KEIM}
;;; Functions which are used by KEIM on startup.
;;;end{latex}

(setq env*predef
      `((type-constants o i)
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
	 (ifthen (aa o aa))
	 (powerset (o (o aa) (o aa)))
	 (image ((o aa) (o bb) (aa bb)))
	 (defn (o aa aa)))))



(defun init~initialize-keim ()
  (declare (edited  "16-JUN-1992 08:43" )
	   (authors KOHLHASE )
	   (input   "None.")
	   (effect  "None." )
	   (value   "Initializes KEIM, i.e. creates an environment 
and fills it with the predefined constants."))
  (post~read-object-list (env~create) env*predef))
