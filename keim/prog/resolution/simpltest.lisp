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
;; Beispiel 1:

(setq problem (keim::prob~read '(problem sk-test-2 proved
				 ((type-constants o i)
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
				   (exists (o (o aa))))
				  (constants (a i) (b i) (f (o i)) (p (o i)))
				  (assumption ass1 (exists (lam (y i) (f y))))
				  (conclusion theorem (exists (lam (y i) (f y))))
				  (constants (c-1 i)))
				 (resolution-proof
				  (cnf    ((clause c-2 ((x1 i)) (- (f x1)))
					   (clause c-4 () (+ (f c-1))))
				   (delta-relation
				    (ass1 ((position 1 0) (c-4 (position 0))))
				    (theorem ((position 1 0) (c-2 (position 0)))))
				   )
				  (resolution step-1 (clause c-5 ())
				   (c-2 (position 0)) (c-4 (position 0))
				   (substitution (x1) (c-1)))
			 	  ))))

(setq env (prob~environment problem))
(setq term1 (term~read '(and (f a) (and (f a) (or (f b) (f a)))) env))


;; Beispiel 2:
(setq problem (keim::prob~read '(problem sk-test-1 proved
				       ((constants (a i) (b i) (p (o i i)))
					(conclusion theorem (not (implies (forall (lam (x i) (exists (lam (y i) (p x y))))) (exists (lam (z1 i) (exists (lam (z2 i) (and (p a z1) (p b z2)))))))))
					(constants (f-1 (i i))))
				       (resolution-proof
					(cnf    ((clause c-1 ((x3 i)) (+ (p x3 (f-1 x3))))
						 (clause c-2 ((x6 i) (x7 i)) (- (p a x6)) (- (p b x7))))
					 (delta-relation
					  (theorem ((position 1 2 1 0 1 0 2) (c-2 (position 1)))
						   ((position 1 2 1 0 1 0 1) (c-2 (position 0)))
						   ((position 1 1 1 0 1 0) (c-1 (position 0)))))
					 )
					(resolution step-1 (clause c-24 ((x21 i)) (- (p b x21)))
					 (c-1 (position 0)) (c-2 (position 0))
					 (substitution (x3 x6 x7) (a (f-1 a) x21)))
					(resolution step-2 (clause c-27 ())
					 (c-1 (position 0)) (c-24 (position 0))
					 (substitution (x3 x21) (b (f-1 b))))
					))
				     (env~init-predefined-symbols (env~create)))))

;;; 2' :
  (setq problem (keim::prob~read '(problem sk-test-1 proved
				   ((constants (a i) (b i) (p (o i i)))
				    (conclusion theorem (not (implies (forall (lam (x i) (exists (lam (y i) (p x y))))) (exists (lam (z1 i) (exists (lam (z2 i) (and (p a z1) (p b z2)))))))))
				    (constants (f-1 (i i))))
				   (resolution-proof
				    (cnf    ((clause c-1 ((x3 i)) (+ (p x3 (f-1 x3))))
					     (clause c-2 ((x6 i) (x7 i)) (- (p a x6)) (- (p b x7))))
				     (delta-relation
				      (theorem ((position 1 2 1 0 1 0 2) (c-2 (position 1)))
					       ((position 1 2 1 0 1 0 1) (c-2 (position 0)))
					       ((position 1 1 1 0 1 0) (c-1 (position 0)))))
				     )
				    (resolution step-1 (clause c-24 ((x21 i)) (- (p b x21)))
				     (c-1 (position 0)) (c-2 (position 0))
				     (substitution (x3 x6 x7) (a (f-1 a) (f-1 b))))
				    (resolution step-2 (clause c-27 ())
				     (c-1 (position 0)) (c-24 (position 0))
				     (substitution (x3 x21) (b (f-1 b))))
				    ))
				 (env~init-predefined-symbols (env~create))))


;; Beispiel 3:




(setq problem-2 (keim::prob~read '(problem subgroup proved
		      ((constants (c i) (e i) (p (o i i i)) (s (o i)) (inv (i i)))
		       (assumption ass1 (forall (lam (u i) (p u (inv u) e))))
		       (assumption ass2 (forall (lam (w i) (p e w w))))
		       (assumption ass3 (forall (lam (x i) (forall (lam (y i) (forall (lam (z i) (implies (and (s x) (and (s y) (p x (inv y) z))) (s z)))))))))
		       (conclusion theo (forall (lam (v i) (implies (s v) (s (inv v))))))
		       )
		      (resolution-proof
		       (cnf    ((clause c17 ((x3 i)) (+ (p x3 (inv x3) e)))
				(clause c19 ((x7 i)) (+ (p e x7 x7)))
				(clause c23 ((x10 i) (x9 i) (x8 i)) (- (s x8)) (- (s x9)) (- (p x8 (inv x9) x10)) (+ (s x10)))
				(clause c12 () (+ (s c)))
				(clause c36 () (- (s (inv c)))))
			       (delta-relation
				(ass3 ((position 1 0 1 0 1 0 2) (c23 (position 3)))
				      ((position 1 0 1 0 1 0 1 2 2) (c23 (position 2)))
				      ((position 1 0 1 0 1 0 1 2 1) (c23 (position 1)))
				      ((position 1 0 1 0 1 0 1 1) (c23 (position 0))))
				(ass2 ((position 1 0) (c19 (position 0))))
				(ass1 ((position 1 0) (c17 (position 0))))
				(theo ((position 1 0 2) (c36 (position 0)))
				      ((position 1 0 1) (c12 (position 0)))))
			       )
		       (resolution step-1 (clause c45 ((x40 i) (x41 i)) (- (s x41)) (- (p c (inv x41) x40)) (+ (s x40)))
				   (c12 (position 0)) (c23 (position 0))
				   (substitution (x8 x9 x10) (c x41 x40)))
		       (resolution step-2 (clause c62 () (- (s c)) (+ (s e)))
				   (c17 (position 0)) (c45 (position 1))
				   (substitution (x3 x41 x40) (c c e)))
		       (resolution step-3 (clause c60 () (+ (s e)))
				   (c12 (position 0)) (c62 (position 0))
				   (substitution () ()))
		       (resolution step-4 (clause c73 ((x66 i) (x47 i)) (- (s x47)) (- (p x47 (inv c) x66)) (+ (s x66)))
				   (c12 (position 0)) (c23 (position 1))
				   (substitution (x9 x8 x10) (c x47 x66)))
		       (resolution step-5 (clause c70 () (- (s e)) (+ (s (inv c))))
				   (c19 (position 0)) (c73 (position 1))
				   (substitution (x47 x7 x66) (e (inv c) (inv c))))
		       (resolution step-6 (clause c67 () (+ (s (inv c))))
				   (c60 (position 0)) (c70 (position 0))
				   (substitution () ()))
		       (resolution step-7 (clause c58 ())
				   (c67 (position 0)) (c36 (position 0))
				   (substitution () ()))
		       ))
		      (env~init-predefined-symbols (env~create))))





(post~read-object-list '((constants (f o) (g o) (h o))) trans*environment)
(setq term (term~read '(implies (and h (or f g)) (or (and h f) (and h g))) trans*environment))


#|

Dies ist kein Beispiel:

(post==read '
 ((problem Nummer1 test
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
|#


(defun recursively-remove-nil-in-list (obj)
  (if (and (atom obj) obj)
      obj
      (append (remove 'nil (mapcar #'rec-remove obj)))))
  

(defun rem-elem (obj elem)
  (if (and (atom obj) obj)
      obj
      (append (remove elem (mapcar #'rec-remove obj elem)))))


(defun all-instances (delta input-formula substitution)
  (mapcar #'(lambda (pair)
	      (let ((formula (keim::delta=get-delta-formula pair))
		    (fpos (keim::delta=get-delta-position-in-formula pair))
		    (cl (keim::delta=get-delta-clause pair))
		    (clpos (keim::delta=get-delta-position-in-clause pair)))
		(format T "~%~S "(erg~variable-binding delta input-formula fpos substitution))))
	  (delta~get-subrelations delta))
  NIL)


(defun read-problem (path)
  (with-open-file (stream path :direction :input)
    (prob~read (read stream))))

(defun test ()
  (setq problem (keim::prob~read '(problem sk-test-1 proved
				   ((constants (a i) (b i) (p (o i i)))
				    (conclusion theorem (not (implies (forall (lam (x i) (exists (lam (y i) (p x y))))) (exists (lam (z1 i) (exists (lam (z2 i) (and (p a z1) (p b z2)))))))))
				    (constants (f-1 (i i))))
				   (resolution-proof
				    (cnf    ((clause c-1 ((x3 i)) (+ (p x3 (f-1 x3))))
					     (clause c-2 ((x6 i) (x7 i)) (- (p a x6)) (- (p b x7))))
				     (delta-relation
				      (theorem ((position 1 2 1 0 1 0 2) (c-2 (position 1)))
					       ((position 1 2 1 0 1 0 1) (c-2 (position 0)))
					       ((position 1 1 1 0 1 0) (c-1 (position 0)))))
				     )
				    (resolution step-1 (clause c-24 ((x21 i)) (- (p b x21)))
				     (c-1 (position 0)) (c-2 (position 0))
				     (substitution (x3 x6 x7) (a (f-1 a) x21)))
				    (resolution step-2 (clause c-27 ())
				     (c-1 (position 0)) (c-24 (position 0))
				     (substitution (x3 x21) (b (f-1 b))))
				    ))
				 (env~init-predefined-symbols (env~create))))
  (multiple-value-setq (res tab th) (trans~tab-proof problem ))
  (setq l (trans=get-tableau-formula-list tab))
  (setq tableau-formula (nth 4 l))
  (setq formula (trans=get-tableau-formula  tf))
  (setq new-formula (termc~quantification-scope formula))
  (setq variable (termc~quantification-bound-variable  formula))
  (setq position (pos~concatenate (trans=get-position tableau-formula) (pos~list-position '(1 0))))
  (setq atom-in-input-formula (term~at-position trans*input-formula position))
  (setq varx (first (appl~arguments atom-in-input-formula)))
  (setq consa (first (appl~arguments new-formula)))
  (setq unifier (list (cons varx consa))))
;;;(setq unifier (trans=tab-subst-to-alist (uni~unify atom-in-input-formula new-formula))))

(defun test-0 ()
  (setq tab (trans~tab-initialize trans*tableau-formula))
  (multiple-value-setq (l1 l2 l3) (trans=tab-find-tableau-componentes (trans=get-tableau-formula-list tab)))
  (trans=tab-apply-one-not-splitting-rule tab (nth 0 l1))
  (multiple-value-setq (l1 l2 l3) (trans=tab-find-tableau-componentes (trans=get-tableau-formula-list tab)))
  (trans=tab-apply-one-not-splitting-rule tab (nth 0 l1))
  (multiple-value-setq (l1 l2 l3) (trans=tab-find-tableau-componentes (trans=get-tableau-formula-list tab)))
  (trans=tab-apply-one-not-splitting-rule tab (nth 1 l1))
  (trans=tab-apply-one-not-splitting-rule tab (nth 0 l1))
  (setq tf (first (last (trans=get-tableau-formula-list tab))))
  (setq formula (trans=get-tableau-formula tf))
  (setq new-formula (termc~quantification-scope formula))
  (setq variable (termc~quantification-bound-variable  formula))
  (setq position (pos~concatenate (trans=get-position tf) (pos~list-position '(1 0))))
  (setq atom-in-input-formula (term~at-position trans*input-formula position))
  (setq unifier (trans=term-binding atom-in-input-formula new-formula))
  (setq position-list (trans=tab-get-all-atoms-to-instance new-formula position))
  (setq position-1 (nth 0 position-list))
  (setq position-2 (nth 1 position-list))
  (setq term-list-1 (trans~variable-of-atom-to-ground-term variable trans*delta-relation trans*input-formula position-1 trans*graph-substitution unifier))
  (setq bindings-2 (erg~variable-binding trans*delta-relation trans*input-formula position-2 trans*graph-substitution))
  (setq all-bindings-according-unifier-2 (mapcan #'(lambda (list)
						     (if (subsetp unifier list :test #'trans=equal-pairs-p)
							 list
							 NIL))
						 bindings-2))
  (setq all-terms-2 (mapcan #'(lambda (pair)
				(if (term~equal (car pair) variable)
                                    (list (cdr pair))
                                    NIL))
                            all-bindings-according-unifier-2)))
 
;;					;  (if all-terms-2
					;      all-terms-2
					;     (error "For ~A is no instantiation!" variable))
;;(setq term-list-2 (trans~variable-of-atom-to-ground-term variable trans*delta-relation trans*input-formula position-2 trans*graph-substitution unifier))
;;;(setq term-list (trans~variable-of-formula-to-ground-term
					 variable trans*delta-relation trans*input-formula  position-list trans*graph-substitution unifier)))
;;;					;                                        ;  
					;                                        ;  (setq derived-formula-list (remove-duplicates (trans=tab-make-instances-of-formula-with-variable
					;                                        ;                                                 new-formula
					;                                        ;                                                 trans*input-formula
;                                        ;                                                 position
;                                        ;                                                 trans*delta-relation
;                                        ;                                                 variable
;                                        ;                                                 trans*graph-substitution
;                                        ;                                                 unifier)
;                                        ;                                                :test #'term~equal))
;                                        ;(multiple-value-setq (l1 l2 l3) (trans=tab-find-tableau-componentes (trans=get-tableau-formula-list tab)))
;                                        ;(trans=tab-apply-one-not-splitting-rule tab (nth 0 l1))
;                                        ;(trans=tab-apply-one-not-splitting-rule tab (nth 1 l1))
;                                        ;(trans=tab-apply-one-not-splitting-rule tab (nth 2 l1))
;                                ;        
  



(defun  test-2 ()
  (setq problem (keim::prob~read '(problem sk-test-1 proved
				   ((constants (a i) (b i) (p (o i i)))
				    (conclusion theorem (not (implies (forall (lam (x i) (exists (lam (y i) (p x y))))) (exists (lam (z1 i) (exists (lam (z2 i) (and (p a z1) (p b z2)))))))))
				    (constants (f-1 (i i))))
				   (resolution-proof
				    (cnf    ((clause c-1 ((x3 i)) (+ (p x3 (f-1 x3))))
					     (clause c-2 ((x6 i) (x7 i)) (- (p a x6)) (- (p b x7))))
				     (delta-relation
				      (theorem ((position 1 2 1 0 1 0 2) (c-2 (position 1)))
					       ((position 1 2 1 0 1 0 1) (c-2 (position 0)))
					       ((position 1 1 1 0 1 0) (c-1 (position 0)))))
				     )
				    (resolution step-1 (clause c-24 ((x21 i)) (- (p b x21)))
				     (c-1 (position 0)) (c-2 (position 0))
				     (substitution (x3 x6 x7) (a (f-1 a) x21)))
				    (resolution step-2 (clause c-27 ())
				     (c-1 (position 0)) (c-24 (position 0))
				     (substitution (x3 x21) (b (f-1 b))))
				    ))
				 (env~init-predefined-symbols (env~create))))
  (multiple-value-setq (res tab theorem) (trans~tab-proof problem))
					;(test-0)
  (trans~tab-print-object tab t)
  )


























