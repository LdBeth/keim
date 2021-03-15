(in-package :keim)
;; set up an initial environment

(setq env (env~create))

(post~read-object-list 
 '((type-constants O I)
   (type-variables AA)
   (constants (and (O O O)) (or (O O O)) (not (O O)) (implies (O O O))
	      (forall (O (O AA))) (exists (O (O AA)))))
 env)
   
			 
(post~read-object-list
	(list '(problem test1 initialized 
	 ((constants (P (O )))
	  (conclusion conc
		      (or P (not P))))))
	(env~create env))

(setq test1 (prob~find-problem 'test1))


(post~read-object-list
	(list '(problem test2 initialized 
	 ((constants (P (O i)) (X I))
	  (conclusion conc
		      (implies (forall (lam (X i) (P X))) (P X))))))
	(env~create env))

(setq test2 (prob~find-problem 'test2))


;;; if you don't use factoring, you can't win
(post~read-object-list
	(list '(problem fac-test initialized 
	 ((constants (P (O i)))
	  (conclusion conc
		      (not (and (or (forall (lam (x i) (p x)))
			 (forall (lam (y i) (p y))))
                   (or (forall (lam (z i) (not (p z))))
		       (forall (lam (w i) (not (p w)))))))))))
	(env~create env))


(setq fac-test (prob~find-problem 'fac-test))

;;; should fail gracefully
(post~read-object-list
	(list '(problem fail-test initialized 
	 ((constants (P O ) (Q O))
	  (conclusion conc
		(not (and P Q))))))
	(env~create env))

(setq fail-test (prob~find-problem 'fail-test))


;;; full set on two elements
(post~read-object-list
	(list '(problem full-set initialized 
	 ((constants (A O ) (B O))
	  (conclusion conc
		      (not (and (and (and (or a b)
					  (or a (not b)))
				     (or (not a ) b))
				(or (not a) (not b))))))))
	(env~create env))




;;; easy
(post~read-object-list
	(list '(problem prop-test initialized 
	 ((constants (P O ) (Q O) (R O))
	  (conclusion conc 
		      (not (and (and (and (and P (or Q P))
					  (or Q (not R)))
				     (or (not P) R))
				(or (not P) (not Q))))))))
	(env~create env))

;;; puzzle from lewis carroll, need forward subsumption
(post~read-object-list
 (list '(problem school initialized
		 ((constants (a O) (b o) (c o) (d o) (e o) (h o)
			     (k o) (r o) (s o) (t o) (m o) (n o)
			     (l o))

		  (assumption a1  d)
		  (assumption a2 (or (not a) (or (not b) c)))
		  (assumption a3 (or (not e) (or (not k) m )))
		  (assumption a4 ( or (not r) (or s t)))
		  (assumption a5
			      ( or (not c) (or (not d) k)))
		  (assumption a6
			      (or (not r) ( or e n)))
		  (assumption a7
			      ( or (not h) (or (not l) r)))
		  (assumption a8
			      ( or (not c) (or (not m) (not e))))
		  (assumption a9
			      (or (not s) (or (not n) (not k))))
		  (assumption a10
			      (or (not a) (or b (or (not h) (not r)))))
		  (assumption a11
			      (or (not a) (or (not d) l)))
		  (assumption a12
			      (or (not c) (or (not t) e)))
		  (assumption a13
			      (or a b))
		  (assumption a14
			      (or c d))
		  (assumption a15
			      (or e h))
		  (assumption a16
			      (or k l))
		  (assumption a17
			      (or m n))
		  (assumption a18
			      (or r s))
		  (conclusion conc (or (not a) (not h))))))
 (env~create env))

;;; salt and mustard puzzle from lewis carroll
(post~read-object-list
 (list '(problem salt initialized
		 ((constants (sl o) (mb o) (sb o) (sm o) (ml o) (sc o)
			     (mc o) (sd o) (md o) (mm o))
		  (assumption a1 (or sl 
				     (or  (not mb) 
					  (or  (not sb)
					       (or (not sm) 
						   (or (not ml) 
						       (or  sc (or  mc 
								    (or  
								     sd 
								     (or  md  mm))))))))))
		  (assumption a2 (or (not sm) (or  mb  ml)))
		  (assumption a3 (or (not sm) (or  mb sl)))
		  (assumption a4 (or (not sm) (or  sb  ml)))
		  (assumption a5 (or (not sm) (or  sb  sl)))
		  (assumption a6 (or (not ml) (or  (not mc)   (not mm))))
		  (assumption a7 (or (not ml) (or  (not mc)   (not sm))))
		  (assumption a8 (or (not ml) (or  (not sc)   (not mm))))
		  (assumption a9 (or (not ml) (or  (not sc)   (not sm))))                      
		  (assumption a10 (or (not md) (or  (not ml)   (not mm))))
		  (assumption a11 (or (not md) (or  (not ml)   (not sm))))
		  (assumption a12 (or (not md) (or  (not sl)   (not mm))))
		  (assumption a13 (or (not md) (or  (not sl)   (not sm))))
		  (assumption a14 (or (not sd) (or  (not mb)   mc)))
		  (assumption a15 (or (not sd) (or  (not mb)  sc)))
		  (assumption a16 (or (not sd) (or  (not sb)   mc)))
		  (assumption a17 (or (not sd) (or  (not sb)   sc)))
		  (assumption a18 (or (not mc) (or  md   ml)))
		  (assumption a19 (or (not mc) (or  md   sl)))
		  (assumption a20 (or (not mc) (or  sd   ml)))
		  (assumption a21 (or (not mc) (or  sd   sl)))
		  (assumption a22 (or (not mb) (or  (not md)   mm)))
		  (assumption a23 (or (not mb) (or  (not md)   sm)))
		  (assumption a24 (or (not mb) (or  (not sd)   mm)))
		  (assumption a25 (or (not mb) (or  (not sd)  sm)))
		  (assumption a26 (or sd (or  (not md)   mm)))
		  (assumption a27 (or (not sd) (or  md   mm)))
		  (assumption a28 (or sc (or  (not mc)  mm)))
		  (assumption a29 (or (not sc) (or  mc   mm)))
		  (assumption a30 (or (not sl) (or  (not ml)   sm)))
		  (assumption a31 (or (not sb) (or  (not mb)   sm)))
		  (assumption a32 (or sd (or  (not md)   sl)))
		  (assumption a33 (or (not sd) (or  md   sl)))
		  (assumption a34 (or sb (or  (not mb)   sl)))
		  (assumption a35 (or (not sb) (or  mb   sl)))
		  (assumption a36 (or (not sc) (or  (not mc)   sd)))
		  (assumption a37 (or (not sl) (or  (not ml)  mc)))
		  (assumption a38 (or (not sd) (or  (not md)  mc)))
		  (assumption a39 (or sb (or  (not mb)   sc)))
		  (assumption a40 (or (not sb) (or  mb   sc)))
		  (assumption a41 (or (not sm) (or  (not mm)  mb)))
		  (assumption a42 (or sl (or  (not ml)  sb)))
		  (assumption a43 (or (not sl) (or  ml   sb)))
		  (assumption a44 (or sc (or  (not mc)  sb)))
		  (assumption a45 (or (not sc) (or  mc   sb)))
		  (assumption a46 (or (not sc) (or  (not mb) (or  (not sb)   (not mm)))))
		  (assumption a47 (or (not sc) (or  (not mb) (or  (not sb)  (not sm)))))
		  (assumption a48 (or (not sc) (or  sb (or  mb   (not mm)))))
		  (assumption a49 (or (not sc) (or  sb (or  mb   (not sm)))))
		  (assumption a50 (or (not mm) (or  (not mc) (or  (not sc) (or  (not md)   (not sd))))))
		  (assumption a51 (or (not mm) (or  sc (or  mc (or  sd   md)))))
		  (assumption a52 (or (not sl) (or  (not mb) (or  (not sb) (or  (not md)   (not sd))))))
		  (assumption a53 (or (not sl) (or  (not mb) (or  (not sb) (or  sd   md)))))
		  (assumption a54 (or (not sl) (or  sb (or  mb (or  (not md)   (not sd))))))
		  (assumption a55 (or (not sb) (or  (not mc) (or  (not sc) (or  (not ml)  (not sl))))))
		  (assumption a56 (or (not sb) (or  (not mc) (or  (not sc) (or  sl   ml)))))
		  (assumption a58 (or sm (or  mm  ml)))
		  (assumption a59 (or sc (or  mc   ml)))
		  (assumption a60 (or sm (or  mm    md)))
		  (assumption a61 (or sl (or  ml   md)))
		  (assumption a62 (or sb (or  mb   sd)))
		  (assumption a63 (or sm (or  mm   sc)))
		  (conclusion conc (and (not sd) (and (not md) (not mb)))))))
 (env~create env))

