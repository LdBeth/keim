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

(test~read-post '((variables (x i) (y i) (z i) (sx (o i)))
	      (constants (a i) (b i) (f1 (i i)) (f2 (i i i)) (g1 (i i))
			 (P1 (o i)) (P2 (o i i)) (S (o i)) (Q o))) t)

(test~read-post '((assumption refl (forall (lam (x i) (p2 x x))))
	      (assumption trans (forall (lam (x i) (forall (lam (y i) (forall (lam (z i)
				   (implies (and (p2 x y) (p2 y z)) (p2 x z)))))))))
	      (conclusion theorem (forall (lam (x i) (P2 x (f2 x y)))))
	      (assumption symm (forall (lam (x i) (forall (lam (y i) (equiv (p2 x y) (p2 y x)))))))))

(test~read-post '((position) 
	      (position 0)
	      (position 1 2 3 4)))

(test~read-post '((clause cl99 ((x1 i) (x2 i) (x3 i))
		      (+ (p1 x1)) (- (p2 x2 (f1 a))) (+ (p1 (f2 b x3))))
	      (clause empty ())))

(test~read-post '((substitution () ())
	      (substitution (x y z) (a b (f1 x)))))


;;;;;;


(test~read-post '((variables (x i) (y i) (z i) (sx (o i)))
	      (constants (a i) (b i) (f1 (i i)) (f2 (i i i)) (g1 (i i))
			 (P1 (o i)) (P2 (o i i)) (S (o i)) (Q o))
	      (clause cl99 ((x1 i) (x2 i) (x3 i))
		      (+ (p1 x1)) (- (p2 x2 (f1 a))) (+ (p1 (f2 b x3))))) t)

 


(test~read-post '((problem Nummer1 test
	   ((constants (a i) (b i) (f1 (i i)) (f2 (i i i)) (g1 (i i))
		       (P1 (o i)) (P2 (o i i)) (S (o i)) (Q o))
	    (assumption refl (forall (lam (x i) (p2 x x))))
	    (assumption trans 
			(forall (lam (x i) 
				     (forall (lam (y i) 
						  (forall (lam (z i)
							       (implies (and (p2 x y) (p2 y z)) (p2 x z)))))))))
	    (conclusion theorem (forall (lam (x i) 
					     (exists (lam (y i) (P2 x (f2 x y)))))))
	    (assumption symm (forall (lam (x i) (forall (lam (y i) (equiv (p2 x y) (p2 y x))))))))
	   (resolution-proof
	    (cnf ((clause c1 ((x1 i)) (+ (p2 x1 x1)))
		  (clause c2 ((x2 i) (x3 i) (x4 i)) (- (p2 x2 x3)) (- (p2 x3 x4)) (+ (p2 x2 x4)))
		  (clause e1 ((z1 i) (z2 i)) (+ (= (f2 z1 z2) (f2 z2 z1))))
		  (clause e2 ((z3 i)) (- (p1 (f2 a z3))))
		  )
		 (delta-relation
		  (refl ((position 1 0) (c1 (position 1))))
		  (trans ((position 1 0 1 0 1 0 1 1) (c2 (position 1)))
			 ((position 1 0 1 0 1 0 1 2) (c2 (position 2)))
			 ((position 1 0 1 0 1 0 2) (c2 (position 3))))
		  (theorem ((position 0 0 0) (e1 (position 1)) (e2 (position 2))))))
	    (resolution step1 (clause c3 ((x5 i) (x6 i)) (+ (p2 x5 x6)))
			(c1 (position 1)) (c2 (position 2))
			(substitution (x1 x2 x3 x4) (a b x5 x6)))
	    (factoring step2 (clause c4 ((y1 i) (y2 i)) (- (p2 y1 y1)) (+ (p2 y1 y1)))
		       (c2 (position 1) (position 2))
		       (substitution (x2 x3 x4) (y1 y1 y1)))
	    (paramodulation step3 (clause e3 ((z4 i)) (- (p1 (f2 z4 a))))
			    (e2 (position 1 1 1)) (e1 (position 1) LR)
			    (substitution (z1 z2 z3) (a z4 z4)))
	    (resolution step4 (clause c5 ((x7 i) (x8 i)) (+ (p2 x7 x8)))
			(c1 (position 1)) (c3 (position 1))
			(substitution (x1 x5 x6) (a b x7 x8)))
	    )
		))
	    t)

(test~read-post '((problem Nummer2 test
	   ((constants (a i) (b i) (f1 (i i)) (f2 (i i i)) (g1 (i i))
		       (P1 (o i)) (P2 (o i i)) (S (o i)) (Q o))
	    (assumption refl (forall (lam (x i) (p2 x x))))
	    (assumption trans 
			(forall (lam (x i) 
				     (forall (lam (y i) 
						  (forall (lam (z i)
							       (implies (and (p2 x y) (p2 y z)) (p2 x z)))))))))
	    (conclusion theorem (forall (lam (x i) 
					     (exists (lam (y i) (P2 x (f2 x y)))))))
	    (assumption symm (forall (lam (x i) (forall (lam (y i) (equiv (p2 x y) (p2 y x))))))))
	   (many-proofs
	     (problem Nummer2.1 test
		      ((conclusion theorem (forall (lam (x i) 
					     (exists (lam (y i) (P2 x (f2 x y))))))))
		      (resolution-proof
			(cnf ((clause c2 ((x2 i) (x3 i) (x4 i)) (- (p2 x2 x3)) (- (p2 x3 x4)) (+ (p2 x2 x4)))
			      )
			     (delta-relation
			       (trans ((position 1 0 1 0 1 0 1 1) (c2 (position 1)))
				      ((position 1 0 1 0 1 0 1 2) (c2 (position 2)))
				      ((position 1 0 1 0 1 0 2) (c2 (position 3))))))
			(factoring step2 (clause c4 ((y1 i) (y2 i)) (- (p2 y1 y1)) (+ (p2 y1 y1)))
				   (c2 (position 1) (position 2))
				   (substitution (x2 x3 x4) (y1 y1 y1)))
			))
	     (problem nummer2.2 test
		      ((conclusion theorem (forall (lam (x i) 
					     (exists (lam (y i) (P2 x (f2 x y))))))))
		      (resolution-proof
			(cnf ((clause e1 ((z1 i) (z2 i)) (+ (= (f2 z1 z2) (f2 z2 z1))))
			      (clause e2 ((z3 i)) (- (p1 (f2 a z3))))
			      )
			     (delta-relation
			       (theorem ((position 0 0 0) (e1 (position 1)) (e2 (position 2))))))
			(paramodulation step3 (clause e3 ((z4 i)) (- (p1 (f2 z4 a))))
					(e2 (position 1 1 1)) (e1 (position 1) LR)
					(substitution (z1 z2 z3) (a z4 z4)))
			)
		      ))
	   t)))








