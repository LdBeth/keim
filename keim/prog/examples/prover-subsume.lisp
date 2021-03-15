(in-package :resprover)

;;; Return T if any clause in VEC subsumes CLAUSE. 
(defun forward-subsumption-p (clause vec)
  (let ((parents (if (res~factoring-p clause) (parent-clauses clause))))
    (dotimes (i (fill-pointer vec))
      (let ((val (aref vec i)))
	(when (and val
		   (not (eq val clause)) ; don't check with itself
		   (not (new-clause-ignore val)) 
		   (not (member val parents)) ; don't redo factors
		   (subsumes-p val clause))
	  (return-from forward-subsumption-p t))))))

(defun do-backward-subsumption (clause vec)
  (dotimes (i (fill-pointer vec))
    (let ((val (aref vec i)))
      (when (and val
		 (not (eq val clause))
		 (not (new-clause-ignore val))
		 (subsumes-p clause val))
	(setf (new-clause-ignore val) t)))))

(defun parent-clauses (clause)
  (cond ((res~resolution-p clause) (res~resolution-clauses clause))
	((res~initial-p clause) nil)
	((res~factoring-p clause)
	 (list (res~factoring-clause clause)))
	(t nil)))

;;; This is an extremely inefficient algorithm for subsumption.
;;; The idea is, C subsumes D if:
;;;   we negate each literal in D and make it a single clause, at the
;;;   same time getting rid of all its variables in favor of new constants
;;;   then we try to resolve away C using these unit clauses. 

(defun subsumes-p (clause1 clause2)
  (unless (> (length (cl~literals clause1))
	     (length (cl~literals clause2)))
    (let ((neg-clauses (make-ground clause2))
	  (clause-set (list (cl~literals clause1))))
      ;; neg-clauses is a list of lists of literals,
      ;; as is clause-set 
      (loop
       (when (member nil clause-set) (return-from subsumes-p t))
       (setq clause-set (get-all-resolvents-subsume clause-set neg-clauses))
       (when (null clause-set) (return-from subsumes-p nil))))))

;;; Return a list of lists, each containing a literal.
;;; The literals are the negated literals from the clause, with
;;; new constants replacing all the free variables.

(defun make-ground (clause)
  (let* ((lits (cl~literals clause))
	 (vars (top~free-variables lits))
	 (consts 
	  (mapcar #'(lambda (var) 
		      (sym~constant-create (gensym "CON")
					 (term~type var)))
		  vars)))
    (mapcar #'list
	    (mapcar #'(lambda (lit)
			(lit~set-polarity! lit
					   (not (lit~positive-p lit)))
			lit)
		    (subst~apply (subst~create vars consts) lits)))))

;;; We don't want to create new clauses, just use lists of literals.
;;; For that reason, we have just modified this stuff from the 
;;; normal resolution. 

(defun get-literals-subsume (clause1 clause2)
  ;; clause1 and clause2 are just lists of literals
  (mapcan #'(lambda (x) (match-literal-to-clause-subsume x clause2))
	  clause1))


(defun match-literal-to-clause-subsume (literal clause)
  (cond ((null clause)
         nil)
        ; if literal is positive
	((lit~positive-p literal)
	 (match-literal-to-literals literal
				    (remove-if #'lit~positive-p clause)))
	(t
	 (match-literal-to-literals literal
				    (remove-if-not #'lit~positive-p clause)))))


(defun get-resolvents-subsume (clause1 clause2)
  (let* ((literal-list (get-literals-subsume clause1 clause2))
	 (substitution-list
	  (mapcar #'(lambda (x) (unify-lits (car x) (cdr x)))
		  literal-list))
	 ;; remove any nils from substitution-list and remove their
	 ;; corresponding literal pairs from literal-list
	 (resolvents nil))
    (multiple-value-setq (substitution-list literal-list)
	(remove-null-pairs substitution-list literal-list))
    ;; get a list of resolvents using the literals and substitutions
    (setq resolvents
	  (mapcar #'(lambda (sub lit-pair)
		      (subst-and-remove-subsume clause1 clause2 sub lit-pair))
		  substitution-list literal-list))
    resolvents))

(defun subst-and-remove-subsume (clause1-literals clause2-literals subst literal-pair)
  (let* ((new-clause1-literals 
	  (remove (car literal-pair) clause1-literals))
	 (new-clause2-literals
	  (remove (cdr literal-pair) clause2-literals)))
    (subst~apply subst 
		 (append new-clause1-literals new-clause2-literals))))

(defun get-all-resolvents-subsume (clause-set neg-clauses)
  (mapcan
   #'(lambda (clause)
       (mapcan #'(lambda (neg-clause)
		   (get-resolvents-subsume clause neg-clause))
	       neg-clauses))
   clause-set))



