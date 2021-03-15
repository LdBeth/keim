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
; example rules
(rule~def-rule same
  (arguments D1 C1)
  (argumenttypes nd+line nd+line)
  (arghelps "A line" "A line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O))
  (preconditions
   (D1 (H) () A)
   (C1 (H) () A))
  (postconditions
   (C1 (H) () A ("Same" () (D1))))
  (actions ((C1 D1 SS)))
  (sideconditions)
  (description "Justify a line by another with the same formula."))

(rule~def-rule or-ir
  (arguments C1)
  (argumenttypes nd+line)
  (arghelps "A disjunction line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (C1 (H) () (or A B)))
  (postconditions
   (L1 (H) () B )
   (C1 (H) () (or A B) ("OrIR" nil (l1))))
  (actions ((C1 SS) (L1 SS)))
  (sideconditions)
  (description "Disjunction introduction right"))

(rule~def-rule or-il
  (arguments C1)
  (argumenttypes nd+line)
  (arghelps "A disjunction line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (C1 (H) () (or A B)))
  (postconditions
   (L1 (H) () A )
   (C1 (H) () (or A B) ("OrIL" nil (l1))))
  (actions ((C1 SS) (L1 SS)))
  (sideconditions)
  (description "Disjunction introduction left"))
	   
(rule~def-rule or-e
  (arguments L1 L2)
  (argumenttypes nd+line nd+line)
  (arghelps "A disjunction line" "A line to be proven")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O) (C O))
  (preconditions
   (L1 (H) () (or A B))
   (L2 (H) () C))
  (postconditions
   (H1 (H) (H1) A ("Case 1" nil ()))
   (L3 (H) (H1) C)
   (H2 (H) (H2) B ("Case 2" nil ()))
   (L4 (H) (H2) C)
   (L2 (H) () C ("OrE" nil (L1 L3 L4))))
  (actions ((L2 L1 SS) (L3 H1 SS) (L4 H2 SS)))
  (sideconditions)
  (description "Disjunction elimination"))

(rule~def-rule and-i
  (arguments L3)
  (argumenttypes nd+line)
  (arghelps "A conjunction line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (L3 (H) () (and A B)))
  (postconditions
   (L1 (H) () A )
   (L2 (H) () B )
   (L3 (H) () (and A B) ("AndI" nil (L1 L2))))
  (actions ((L3 SS) (L1 SS) (L2 SS)))
  (sideconditions)
  (description "Conjunction introduction"))

(rule~def-rule and-e
  (arguments L3)
  (argumenttypes nd+line)
  (arghelps "A conjunction line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (L3 (H) () (and A B)))
  (postconditions
   (L1 (H) () A  ("AndE" nil (L3)))
   (L2 (H) () B  ("AndE" nil (L3))))
  (actions ((PP L3) (PP L1 L2)))
  (sideconditions)
  (description "Conjunction elimination"))

(rule~def-rule implies-i
  (arguments L3)
  (argumenttypes nd+line)
  (arghelps "An implication line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (L3 (H) () (implies A B)))
  (postconditions
   (L1 (H) (L1) A  ("Hyp" nil nil))
   (L2 (H) (L1) B )
   (L3 (H) () (implies A B) ("ImpI" nil (L2))))
  (actions ((L3 SS) (L2 L1 SS)))
  (sideconditions)
  (description "Implication introduction"))


(rule~def-rule implies-e
  (arguments L3 L4)
  (argumenttypes nd+line nd+line)
  (arghelps "An implication line" "Antecedent")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A O) (B O))
  (preconditions
   (L3 (H) () (implies A B))
   (L4 (H) () A))
  (postconditions
   (L2 (H) () B ("ImpE" nil (L4 L3))))
  (actions ((PP L3 L4 SS) (PP L2 SS)))
  (sideconditions)
  (description "Implication elimination"))

(rule~def-rule not-i
  (arguments L3)
  (argumenttypes nd+line)
  (arghelps "A negation line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))
    (false O)))
  (meta-variables (A O))
  (preconditions
   (L3 (H) () (not A)))
  (postconditions
   (L1 (H) (L1) A  ("Hyp" nil nil))
   (L2 (H) (L1) false )
   (L3 (H) () (not A) ("NotI" nil (L2))))
  (actions ((L3 SS) (L2 L1 SS)))
  (sideconditions)
  (description "Implication introduction"))


(rule~def-rule not-e
  (arguments L1 C1)
  (argumenttypes nd+line nd+line)
  (arghelps "A negation line" "A falsehood line")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))
    (false O)))
  (meta-variables (A O))
  (preconditions
   (L1 (H) () (not A))
   (C1 (H) () false ))
  (postconditions
   (L2 (H) () A  )
   (C1 (H) () false ("NotE" nil (L1 L2))))
  (actions ((C1 L1 SS) (L2 L1 SS)))
  (sideconditions)
  (description "Negation elimination"))

(rule~def-rule exists-i
  (arguments C1 X)
  (argumenttypes nd+line term+term)
  (arghelps "An existential line" "A term")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A (O AA)) (X AA))
  (preconditions
   (C1 (H) () (exists A)))
  (postconditions
   (C2 (H) () (eval (hop~beta-contract (A X))))
   (C1 (H) () (exists A) ("Exists-I" (X) (C2))))
  (actions ((C1 SS) (C2 SS)))
  (sideconditions)
  (description "Existential introduction"))

(rule~def-rule exists-e
  (arguments D1 X C1)
  (argumenttypes nd+line sym+sym nd+line)
  (arghelps "An existential line" "A term" "Line to be proved")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A (O AA)) (B O) (X AA) )
  (preconditions
   (D1 (H) () (exists A))
   (C1 (H) () B))
  (postconditions
   (D2 (H) (D2) (eval (hop~beta-contract (A X))) ("Hyp" (X)))
   (C2 (H) (D2) B)
   (C1 (H) () B ("Exists-E" () (D2 C2))))
  (actions ((C1 D1 SS) (C2 D2 SS)))
  (sideconditions
   (nd~not-free-in-lines-or-hyps-p X C1 D1))
  (description "Existential elimination"))

(rule~def-rule forall-e
  (arguments D1 Y)
  (argumenttypes nd+line term+term)
  (arghelps "An universal line" "A term")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A (O AA)) (Y AA))
  (preconditions
   (D1 (H) () (forall A)))
  (postconditions
   (D2 (H) ()  (eval (hop~beta-contract (A Y))) ("ForallE" (Y) (D1))))
  (actions ((PP D1 SS) (PP D1 D2 SS)))
  (sideconditions)
  (description "Universal elimination"))

(rule~def-rule forall-i
  (arguments D1 X)
  (argumenttypes nd+line sym+sym)
  (arghelps "An universal line" "A new parameter")
  (declarations
   (type-constants O)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA)))))
  (meta-variables (A (O AA)) (X AA))
  (preconditions
   (D1 (H) () (forall A)))
  (postconditions
   (D2 (H) () (eval (hop~beta-contract (A X))))
   (D1 (H) () (forall A) ("ForallI" () (D2))))
  (actions ((PP D1 SS) (PP D1 D2 SS)))
  (sideconditions (nd~not-free-in-lines-or-hyps-p X D1))
  (description "Universal introduction"))

; now some tests

(nd~def-nd-proof and-e-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))    (constants
     (X O)  (Y O)))
  (assumptions (l1 (and X Y)))
  (conclusion l100 X )
  (plans (l100))
  (lines
   (l1 (l1) (and X Y) ("Hyp" nil nil) nil)
   (l100 (l1) X  ("Planned" nil nil) (l1))))


(defun rule==test-rule (rule-name args)
  (rule~describe rule-name)
  (format t "~%Proof currently looks like:~%")
  (nd~show-proof nd*current-proof)
  (format t "~%Doing ~S~%" rule-name)
  (rule~apply rule-name args)
  (format t "~%Result of applying ~S:~%" rule-name)
  (nd~show-proof nd*current-proof))



#+test-all
(let* ((nd*current-proof (nd~find-proof 'and-e-ex))
         (l1 (nd~label-2-line 'l1))
         (l2 (nd~label-2-line 'l2))
         (l100 (nd~label-2-line 'l100)))
  (rule==test-rule 'and-e (list l1)))




(nd~def-nd-proof and-i-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (X O)  (Y O)))
  (assumptions (l1 X) (l2 Y))
  (conclusion l100 (and X Y))
  (plans (l100))
  (lines
   (l1 (l1) X  ("Hyp" nil nil) nil)
   (l2 (l2) Y ("Hyp" nil nil) nil)
   (l100 (l1 l2) (and X Y)  ("Planned" nil nil) (l1 l2))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'and-i-ex))
         (l1 (nd~label-2-line 'l1))
         (l2 (nd~label-2-line 'l2))
         (l100 (nd~label-2-line 'l100)))
   (rule==test-rule 'and-i (list l100)))



(nd~def-nd-proof or-i-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (X O)  (Y O)))
  (assumptions (l1 X ))
  (conclusion l100 (or X Y))
  (plans (l100))
  (lines
   (l1 (L1) X ("Hyp" nil nil) ())
   (l100 (L1) (or X Y)  ("Planned" nil nil) (L1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'or-i-ex))
         (l100 (nd~label-2-line 'l100)))
   (rule==test-rule 'or-ir (list l100))
   (format t "~%Making line L100 a plan line again.~%")
   (nd~planify l100)
   (format t "~%Doing or-il~%")
   (rule==test-rule 'or-il (list l100)))


(nd~def-nd-proof or-e-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (X O)))
  (assumptions (l1 (or X X)))
  (conclusion l100 X )
  (plans (l100))
  (lines
   (l1 (l1)  (or X X) ("Hyp" nil nil))
   (l100 (l1) X  ("Planned" nil nil) (l1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'or-e-ex))
         (l100 (nd~label-2-line 'l100))
	 (l1 (nd~label-2-line 'l1)))
   (rule==test-rule 'or-e (list l1 l100)))




(nd~def-nd-proof implies-e-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (X O) (Y O)))
  (assumptions (l1 (implies X Y)) (l2 X))
  (conclusion l100 Y)
  (plans (l100))
  (lines
   (L1 (L1) (implies X Y) ("Hyp" nil nil))
   (L2 (L2) X ("Hyp" nil nil))
   (l100 (L1 L2) Y  ("Planned" nil nil) ())))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'implies-e-ex))
       (l100 (nd~label-2-line 'l100))
       (l1 (nd~label-2-line 'l1))
       (l2 (nd~label-2-line 'l2)))
   (rule==test-rule 'implies-e (list l1 l2)))



(nd~def-nd-proof forall-e-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (A I)
     (P (O I))))
  (assumptions (l1 (forall (lam (X I) (P X)))))
  (conclusion l100 (P A))
  (plans (l100))
  (lines
   (l1 (l1) (forall (lam (X I) (P X)))  ("Hyp" nil nil) ())
   (l100 (l1) (P A) ("Planned" nil nil) (l1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'forall-e-ex))
       (l1 (nd~label-2-line 'l1))
       (env (prob~environment nd*current-proof)))
  (rule==test-rule 'forall-e (list l1 (term~read 'a env))))


(nd~def-nd-proof forall-i-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (P (O I))
     (A I)))
  (assumptions (l1 (P A)))
  (conclusion l100 (forall (lam (Y I) (P Y))))
  (plans (l100))
  (lines
   (l1 (l1) (P A) ("Hyp" nil nil) ())
   (l100 (l1) (forall (lam (Y I) (P Y))) ("Planned" nil nil) (l1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'forall-i-ex))
       (l100 (nd~label-2-line 'l100))
       (env (prob~environment nd*current-proof)))
  (format t "~%First time should fail because A is free in a hypothesis.~%")
  (rule==test-rule 'forall-i (list l100 (env~lookup-object 'a env)))
  (post~read-object-list '((constants (z i))) env)
  (format t "~%This time should be okay because Z is not free in a hypothesis.~%")
  (rule==test-rule 'forall-i (list l100 (env~lookup-object 'z env)))
)


(nd~def-nd-proof exists-i-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (P (O I))
     (A I)))
  (assumptions (l1 (P A)))
  (conclusion l100 (exists (lam (Y I) (P Y))))
  (plans (l100))
  (lines
   (l1 (l1) (P A) ("Hyp" nil nil) ())
   (l100 (l1) (exists (lam (Y I) (P Y))) ("Planned" nil nil) (l1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'exists-i-ex))
       (l100 (nd~label-2-line 'l100))
       (env (prob~environment nd*current-proof)))
  (rule==test-rule 'exists-i (list l100 (env~lookup-object 'a env)))
)

(nd~def-nd-proof exists-e-ex
  (declarations 
   (type-constants O I)
   (type-variables AA)
   (constants
    (and (O O O))
    (or (O O O))
    (implies (O O O))
    (not (O O))
    (exists (O (O AA)))
    (forall (O (O AA))))
    (constants
     (P (O I I))
     (A I)))
  (assumptions (l1 (exists (lam (X I) (P X A)))))
  (conclusion l100 (exists (lam (Y I) (exists (lam (Z I) (P Y Z))))))
  (plans (l100))
  (lines
   (l1 (l1) (exists (lam (X I) (P X A))) ("Hyp" nil nil) ())
   (l100 (l1) (exists (lam (Y I) (exists (lam (Z I) (P Y Z))))) 
	 ("Planned" nil nil) (l1))))

#+test-all
(let* ((nd*current-proof (nd~find-proof 'exists-e-ex))
       (l100 (nd~label-2-line 'l100))
       (l1 (nd~label-2-line 'l1))
       (env (prob~environment nd*current-proof)))
  (format t "~%First time should fail because A is free in a hypothesis.~%")
  (rule==test-rule 'exists-e (list l1 (env~lookup-object 'a env) l100))
  (format t "~%This time should succeed because W is new.~%")
  (post~read-object-list '((constants (W I))) env)
  (rule==test-rule 'exists-e (list l1 (env~lookup-object 'w env) l100))
)

