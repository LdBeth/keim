(in-package :resprover)

;; These routines are used in factoring clauses.  When factoring is
;; turned on, each time a clause's node is constructed
;; its factors are also created,
;; if, of course, the clause isn't a tautology and hasn't been subsumed.

;; GET-FACTORS is called by PROCESS-NEW-CLAUSE. It returns the first-level
;; factors of the clause.
;; First the first literal of the clause is checked against all other
;; literals, and SINGLE-FACTOR returns all factors which be made by
;; matching that literal with some other.  Null factors are removed, and
;; also duplicate factors.  GET-FACTORS is called recursively for each
;; literal in the clause, and it appends any factors created into a single
;; list of factors.  Since each new factor will be given to PROCESS-NEW-CLAUSE
;; each factor will eventually get factored.
;; We work with lists of literals below.

(defun get-factors (clause)
  (let ((part-of-clause (cl~literals clause))
	(whole-clause (cl~literals clause)))
    (mapcar #'(lambda (x) (construct-clause-factor clause x))
	    (get-factors-aux part-of-clause whole-clause))))

;; GET-FACTORS-AUX takes two lists of literals as arguments. 
(defun get-factors-aux (part-of-clause whole-clause)
  ;; if we're down to one literal, stop
  (if (<= (length part-of-clause) 1) 
      nil
      (remove-set-equals
       (delete nil
	       (nconc
		(mapcar #'(lambda (x)
			    (single-factor 
			     (car part-of-clause) x whole-clause))
			(cdr part-of-clause))
		(get-factors-aux (cdr part-of-clause) whole-clause)))
       )))

(defun new-get-factors (clause)
  (let ((whole-clause (cl~literals clause)))
    (do ((current-lit (car whole-clause) (car other-lits))
	 (other-lits (cdr whole-clause) (cdr other-lits))
	 (factors nil (nconc (if other-lits
				 (mapcar #'(lambda (x)
					     (single-factor current-lit x
							    whole-clause))
					 other-lits)) 
			     factors)))
	((null other-lits)
	 (mapcar #'(lambda (x) (construct-clause-factor clause x))
		 (remove-set-equals (delete nil factors)))))))
     

; SINGLE-FACTOR tries to unify two literals, applies the substitution to the
; whole clause, and removes duplicate literals.  

(defun single-factor (literal1 literal2 whole-clause)
  (when (or (and (lit~positive-p literal1) (lit~positive-p literal2))
	    (and (not (lit~positive-p literal1))
		 (not (lit~positive-p literal2))))
    (del-sub (unify-lits literal1 literal2) whole-clause)))



;; CONSTRUCT-CLAUSE-FACTOR makes a clause, renames its variables,
;; then makes its justification a factoring justification.  Finally,
;; the class of the new clause is updated to type new-clause.
(defun construct-clause-factor (parent literal-list)
  (multiple-value-bind (new-clause renaming)
      (top~rename (clause-create literal-list :name "tmp") :return-renaming t)
    (node~set-justification! 
     new-clause
     ;; don't save positions, unifier
     (res~factoring-create parent nil nil renaming (gensym "STEP")))
    (change-class new-clause 'new-clause)
    new-clause))


(defun remove-set-equals (list)
  (delete-duplicates list :test #'literals-set-equal))

;; DEL-SUB applies a substitution to a list of literals and removes any
;; duplicates in the result.  if the substitution is nil, then nil is
;; returned.
(defun del-sub (subst list)
  (if subst
      (delete-duplicates (subst~apply subst list) :test #'literal-equal-p)
      nil))


; implements set equality
; assume that we have already removed duplicates from each list
(defun literals-set-equal (list1 list2)
  (and (= (length list1) (length list2))
       (dolist (l1 list1 t)
	 (unless (find l1 list2 :test #'literal-equal-p)
	   (return-from literals-set-equal nil)))))


