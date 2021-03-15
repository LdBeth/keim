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

(setq theorem (post~read-term '(not (exists (lam (x i) (exists (lam (y i) (and (q x y) (p2 x y))))))) cnf*env))

(setq axiom (post~read-term '(forall (lam (x i) (exists (lam (y i) (and (q x y) (p2 x y)))))) cnf*env))


#|
> (cnf~normalize theorem cnf*env)
({[C-8] -(Q X Y) -(P2 X Y)})
(NOT (SK-EXISTS [X].(SK-EXISTS [Y].(AND (Q X Y) (P2 X Y)) Y) X))
> (cnf~normalize axiom cnf*env)
({[C-9] +(Q X (f-2 X))} {[C-10] +(P2 X (f-2 X))})
(SK-FORALL [X].(SK-EXISTS [Y].(AND (Q X Y) (P2 X Y)) (f-2 X)) X)
|#

(test~read-post '((problem Nummer1 test
		       ((constants (p (o i i)) (q (o i i)))
			(assumption ass (forall (lam (x1 i) (exists (lam (x2 i) (and (q x1 x2) (p x1 x2)))))))
			(conclusion con (not (exists (lam (x1 i) (exists (lam (x2 i) (and (q x1 x2) (p x1 x2))))))))
			(variables (y1 i) (y2 i))       ; Dies sind die Skolemvariablen 
			(constants (f-2 (i i))))        ; und -konstanten
		       (resolution-proof
			 (cnf ((assumption ass (sk-forall (lam (x1 i) (sk-exists (lam (x2 i) (and (q x1 x2) (p x1 x2))) (f-2 y1))) y1))
			       (conclusion con (not (sk-exists (lam (x1 i) (sk-exists (lam (x2 i) (and (q x1 x2) (p x1 x2))) y2)) y1))))
			      ((clause c1 ((z1 i)) (+ (q z1 (f-2 z1))))
			       (clause c2 ((z2 i)) (+ (p z2 (f-2 z2))))
			       (clause c3 ((z3 i) (z4 i)) (- (q z3 z4)) (- (p z3 z4))))
			      ())              ;hier fehlt die delta-relation
			 (resolution step1 (clause d1 ((v1 i)) (- (p v1 (f-2 v1))))
				     (c1 (position 1)) (c3 (position 1))
				     (substitution (z1 z3 z4) (v1 v1 (f-2 v1))))
			 (resolution step2 (clause d2 ((v2 i)))
				     (c2 (position 1)) (d1 (position 1))
				     (substitution (z2 v1) (v2 v2)))
			 )
		       (:list)))
	      t)

;Die Skolemvariablen und -konstanten muessen im Augenblick im problem deklariert werden. Vielleicht ist es sinnvoll dafuer
;in der CNF-Form einen Platz vorzusehen.
;Es fehlt der Uebergang von den bei der Skolemisierung umbenannten Variablen (y1 y2) zu den Variablen in den Klauseln (z1 - z4).
;Dies ist eine 1 zu n Beziehung. Vielleicht kann man dies in der Delta-Relation mit einbauen.

;(subst~compose-substitution

