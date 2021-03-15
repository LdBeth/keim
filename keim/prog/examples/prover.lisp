(in-package :resprover)

(eval-when (load compile eval)
 (defclass new-clause (cl+clause)
   ((ignore :accessor new-clause-ignore :initform nil :initarg :ignore
	    :documentation "T if the clause should be ignored in the
future (because subsumed or a factor has been found)")
    (ranking :accessor new-clause-ranking :initform 0 :initarg :ranking
	     :documentation "an integer indicating how desirable this
clause is (with 0 being the best and going upward from there)")
    (retained :accessor new-clause-retained :initform nil
	      :initarg :retained
	      :documentation "True if this clause has been accepted and
used to generate new clauses."))
   (:documentation "CL+CLAUSE with three new slots. They tell us if the clause
should be ignored or not, give a ranking, and tell if the clause is
retained or in the set of support.")))

(defmethod update-instance-for-different-class
 :after ((old cl+clause) (new new-clause) &rest others)
 (declare (ignore others))
 (setf (new-clause-ranking new) (length (cl~literals new))))

(defvar *all-clauses* (make-array '(100) :initial-element nil
				      :adjustable t
				      :fill-pointer 0)
  "Vector of all clauses that have been generated.")


(defvar *input-clauses* nil "list of input clauses")
(defvar *total-clauses* 0 "The number of the last clause we have generated")

(defvar *report-generated* nil "If T, reports clauses as they are generated.")
(defvar *report-kept* t "If T, reports clauses if they are kept.")
(defvar *use-unit-resolution* nil "If T, do resolution steps only where one of
the clauses is a unit clause.")

(defvar *use-unit-preference* t
  "If T, as soon as a unit clause is generated, check to see if it can
be resolved with an existing unit clause.")
(defvar *use-input-resolution* nil
  "If T, do resolution steps only where one of the clauses is an 
input clause.")
(defvar *use-forward-subsumption* t "If T, discard each newly-generated 
clause if it is subsumed by a previously-generated clause.")
(defvar *use-backward-subsumption* nil "If T, discard any previously-generated
clauses if they are subsumed by a new clause")
(defvar *use-factoring* t "If T, do factoring on all newly-created clauses")


(defun empty-p (vector)
  (zerop (fill-pointer vector)))

;;; Conditions 

(sys~define-condition 
 empty-clause-found 
 (sys+error)
 (clause)
 (lambda (cond stream) (format stream "#<EMPTY-CLAUSE ~A>"
			      (empty-clause-found-clause cond))))

(sys~define-condition 
 cant-proceed
 (sys+error)
 ()
 (lambda (cond stream)
   (declare (ignore cond))
   (princ stream "#<CANT-PROCEED>")))


;;; Creates a clause using arguments, changes its class to new-clause
(defun clause-create (literals &rest args)
  (let ((new-clause (apply #'cl~create (delete-duplicates 
					literals 
					:test #'literal-equal-p)
			   args)))
    (change-class new-clause 'new-clause)
    new-clause))

;; LITERAL-EQUAL-P returns T when two the literals have the same polarity
;; and their atoms are term~equal

(defun literal-equal-p (lit1 lit2)
  (if (lit~positive-p lit1)
      (and (lit~positive-p lit2)
	   (term~equal (lit~atom lit1) (lit~atom lit2)))
      (and (not (lit~positive-p lit2))
	   (term~equal (lit~atom lit1) (lit~atom lit2)))))


(defun ignore-clause-p (clause)
  (new-clause-ignore clause))

(defun unit-p (clause)
  (null (cdr (cl~literals clause))))

(defun input-p (clause)
  (member clause *input-clauses*))

;;; a clause is a tautology if it contains two literals of form
;;; -P and +P (the atoms must match exactly, that is they are unifiable
;;; by the empty substitution).
(defun tautology-p (literal-list)
  (mapl
   #'(lambda (l)
       (let ((lit (car l))
	     (others (cdr l)))
	 (when (find-if (if (lit~positive-p lit)
			 #'(lambda (x) (and (not (lit~positive-p x))
					    (let ((subst (unify-lits lit x)))
					      (and subst
						   (subst~empty-p subst)))))
			 #'(lambda (x) (and (lit~positive-p x)
					    (let ((subst (unify-lits lit x)))
					      (and subst
						   (subst~empty-p subst))))))
		     others)
	   (return-from tautology-p t))))
   literal-list)
  nil)


;;; Make a name for the clause, incrementing *total-clauses* at same time
(defun give-clause-a-number (clause)
  (incf *total-clauses*)
  (keim~set-name! clause (make-symbol (format nil "C-~D" *total-clauses*))))

(defun add-new-clause (clause vector)
  (vector-push-extend clause vector))

;;; PROCESS-NEW-CLAUSE handles each new clause created.
;;; It runs in the following fashion:
;;; 1. If *report-generated* is T, print the clause.
;;; 2. If clause is empty, we add it to *all-clauses* and signal empty-clause-found
;;; 3. If clause is a tautology, we bail out and return nil.
;;; 4. If forward-subsumption is being used, we apply it.  If clause is
;;;    subsumed, bail out.
;;; 5. Add the clause to *all-clauses*
;;; 6. If *report-kept*, print the clause.
;;; 7. If *use-unit-preference* and clause is unit clause, we try to 
;;;    immediately resolve it with existing unit clauses.
;;; 8. Do backward subsumption if *use-backward-subsumption* is T.
;;; 9. Generate all factors of the clause if *use-factoring* is T. 

(defun process-new-clause (clause)
  (when *report-generated* 
    (report-clause clause))
  (when (null (cl~literals clause))
    (give-clause-a-number clause)
    (add-new-clause clause *all-clauses*) 
    (when *report-kept* 
      (report-clause clause))
    (sys~signal 
     (sys~make-condition 'empty-clause-found :clause clause)))
  (when (tautology-p (cl~literals clause))
    (return-from process-new-clause nil))
  (when *use-forward-subsumption* 
    (when (forward-subsumption-p clause *all-clauses*)
      (setf (new-clause-ignore clause) t)
      (return-from process-new-clause nil)))
  (give-clause-a-number clause)
  (add-new-clause clause *all-clauses*) 
  (when *report-kept* 
    (report-clause clause))
  (when (and (unit-p clause) *use-unit-preference*)
    (do-unit-conflict clause *all-clauses*))
  (when *use-backward-subsumption*
    (do-backward-subsumption clause *all-clauses*))
  (when *use-factoring*
    (dolist (factor (get-factors clause))
      (process-new-clause factor))))

(defun move-from-sos-to-retained (clause)
  (setf (new-clause-retained clause) t))

(defun initialize-initial-clauses (problem)
  (sys~handler-case
   (progn
     (setf (fill-pointer *all-clauses*) 0)
     (setq *total-clauses* 0
	   *input-clauses* nil)
     (let ((clauses (res~proof-clauses problem)))
       (dolist (clause clauses)
	 (construct-clause-initial (cl~literals clause)))))
   (empty-clause-found (c) 
		       (declare (ignore c))
		       (report-message "Eureka!")
		       (get-clauses-from-vectors problem *all-clauses*)
		       (return-from initialize-initial-clauses))))

(defun construct-clause-initial (literal-list)
  (let ((new-clause (clause-create literal-list  :name "tmp")))
    (node~set-justification! 
     new-clause
     (res~initial-create (gensym "STEP")))
    (push new-clause *input-clauses*)
    (process-new-clause new-clause)))

;;; Pick the best clause from the vector.  Basically, all this does is
;;; find the clause (not already retained) with the lowest ranking. 

(defun pick-clause-from (vec)
  (when (empty-p vec)
    (sys~signal 'cant-proceed))
  (let ((length (fill-pointer vec)))
    (let ((best-clause nil)
	  (best-ranking nil))
    (dotimes (i length
		(if best-clause best-clause (sys~signal 'cant-proceed)))
      (let ((val (aref vec i)))
	(when (and val (not (ignore-clause-p val))
		   (not (new-clause-retained val))
		   (or (null best-ranking)
		       (< (new-clause-ranking val) best-ranking)))
	  (setq best-clause val
		best-ranking (new-clause-ranking val))
	  ))))))

;;; Two clauses are resolvable if they have literals which could be
;;; complementary.  *use-unit-resolution* and *use-input-resolution*
;;; (only one can be used at at time) can also be used to screen the
;;; clauses.

(defun resolvable-p (clause1 clause2)
  (cond ((eq clause1 clause2) nil)
	((or (ignore-clause-p clause1) 
	    (ignore-clause-p clause2)
	    (not (complementary-literals-p clause1 clause2)))
	 nil)
	(*use-unit-resolution*
	 (if (or (unit-p clause1)
		 (unit-p clause2))
	     t
	     nil))
	;; if input resolution is being used
	(*use-input-resolution*
	 (if (or (input-p clause1)
		 (input-p clause2))
	     t
	     nil))
	(t t)))

;;;  Given a clause and a vector of clauses, step down the vector 
;;;  trying to generate all resolvents with the given clause, but
;;;  only using already-retained clauses.

(defun generate-new-sos-clauses (clause vec)
  (let ((length (fill-pointer vec)))
    (dotimes (i length)
      (let ((val (aref vec i)))
	(when (and val (new-clause-retained val) (resolvable-p clause val))
	  (resolve-clauses clause val)))))
  )

;;; Returns T if the clauses contain literals which have opposing polarities
;;; and same head 
(defun complementary-literals-p (clause1 clause2)
  (get-literals clause1 clause2 nil))

; returns a list of pairs of literals which are complementary in the two
; clauses.  each literal in clause1 is sent with clause2 to match-literals
; which places any complementary pairs in match-list.

(defun get-literals (clause1 clause2 &optional (all-pairs t))
  (if all-pairs
      (mapcan #'(lambda (x) (match-literal-to-clause x clause2))
	      (cl~literals clause1))
      (some #'(lambda (x) (match-literal-to-clause x clause2))
	    (cl~literals clause1))))

; finds complementary literals, returning a list of pairs

(defun match-literal-to-clause (literal clause)
  (cond ((null clause)
         nil)
        ; if literal is positive
	((lit~positive-p literal)
	 (match-literal-to-literals literal
				    (cl~neg-literals clause)))
	(t
	 (match-literal-to-literals literal
				    (cl~pos-literals clause)))))
;;; return all pairs of literal with the literals in other-lits, where
;;; they have the same predicate
(defun match-literal-to-literals (literal other-lits)
  (delete nil
	  (mapcar #'(lambda (l2)
		      (when (term~equal (term~top literal) (term~top l2))
			(cons literal l2)))
		  other-lits)))

;;; unify the atoms of two literals, return the substitution
(defun unify-lits (lit1 lit2)
  (uni~unify (lit~atom lit1) (lit~atom lit2)))

;;; given two clauses:
;;; 1. Get all pairs of literals which might be unifiable
;;; 2. For each pair, generate the unifier (if possible),
;;;    and discard any pairs that were not unifiable (unify-lits returned nil)
;;; 3. for each pair, apply the corresponding substitution to the clauses,
;;;    removing the literals which were matched, and collapsing the
;;;    remainders of the clauses together.
;;; 4. create a new clause from each of the resolvents.

(defun resolve-clauses (clause1 clause2)
  (let* ((literal-list (get-literals clause1 clause2))
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
		      (subst-and-remove clause1 clause2 sub lit-pair))
		  substitution-list literal-list))
    (mapcar #'(lambda (lit-list subst lit-pair) 
		(construct-clause-res 
		 lit-list subst 
		 (list clause1 clause2)
		 (list (pos~list-position 
			(list 
			 (position (car lit-pair)
				   (cl~literals clause1))))
		       (pos~list-position 
			(list 
			 (position (cdr lit-pair)
				   (cl~literals clause2)))))))
	    resolvents substitution-list literal-list)
    ))

; moves down two lists. if any element of the first list is null, then
; it and its counterpart are removed from the respective lists.  
(defun remove-null-pairs (list1 list2)
  (if (null list1)
      (values nil nil)
      (multiple-value-bind (newlist1 newlist2)
	  (remove-null-pairs (cdr list1) (cdr list2))
	(if (null (car list1))
	    (values newlist1 newlist2)
	    (values (cons (car list1) newlist1)
		    (cons (car list2) newlist2))))))

;;; The car of literal-pair is a literal in clause1, and the
;;; cdr of literal-pair is a literal in clause2.  Subst is a substitution
;;; unifying these two literals.  We apply the substitution to the
;;; union of the remaining literals from the two clauses, and return a
;;; list of the instantiated literals.

(defun subst-and-remove (clause1 clause2 subst literal-pair)
  (let* ((clause1-literals (cl~literals clause1))
	 (clause2-literals (cl~literals clause2))
	 (new-clause1-literals 
	  (remove (car literal-pair) clause1-literals))
	 (new-clause2-literals
	  (remove (cdr literal-pair) clause2-literals)))
    (subst~apply subst 
		 (append new-clause1-literals new-clause2-literals))))


;;; literal-list is a list of literals that results from resolving
;;; the parents (list of two clauses) with the substitution.  The positions
;;; show the respective positions of the literals used to generate the
;;; substitution in their clauses.
;;; Make a new clause, rename its variables, then send to process-new-clause.

(defun construct-clause-res (literal-list substitution parents positions)
  (multiple-value-bind (new-clause renaming)
      (top~rename (clause-create literal-list :name "tmp") :return-renaming t)
    (change-class new-clause 'new-clause)
    (node~set-justification! 
     new-clause
     (res~resolution-create 
      parents positions substitution renaming
      (gensym "STEP")))
    (process-new-clause new-clause)))


;;; Problem is a KEIM Problem, with vec being the vector of clauses
;;; created during its proof.

(defun get-clauses-from-vectors (problem vec)
  (let ((clauses nil)
	(init-clauses nil)
	(empty-clause nil))
    (dotimes (i (fill-pointer vec))
	(let ((val (aref vec i)))
	  (when val
	    (if (res~initial-p val)
	      (push val init-clauses)
	      (push val clauses))
	    (when (cl~empty-p val)
	      (setq empty-clause val))
	    )))
    (setf (fill-pointer vec) 0)
    (proof~set-steps! problem (nreverse clauses))
    (res~set-proof-clauses! problem (nreverse init-clauses))
    (when empty-clause
      (res~set-proof-empty-clause! problem empty-clause))))
  

;;; problem is an initialized KEIM problem, for which we have already
;;; set up *all-clauses*. Loop with do-a-round, and catch
;;; any conditions for success or failure.

(defun run-prover (problem)
  (sys~handler-case 
   (loop
      (do-a-round))
   (empty-clause-found (c)
		       (declare (ignore c))
		       (report-message "Eureka!")
		       (get-clauses-from-vectors problem *all-clauses*)
		       (return-from run-prover))
   (cant-proceed (c)
		 (declare (ignore c))
		 (report-message "CANNOT PROCEED.")
		 (get-clauses-from-vectors problem *all-clauses*)
		 (return-from run-prover))))


;;; DO-A-ROUND simply picks a clause from *all-clauses*, makes it a 
;;; retained clause, and generates all new clauses between it and
;;; already-existing retained clauses.

(defun do-a-round ()
  (let ((current-clause (pick-clause-from *all-clauses*)))
    (move-from-sos-to-retained current-clause)
    (generate-new-sos-clauses current-clause *all-clauses*)))


;;; clause must be a unit clause.  We try to resolve it with all
;;; existing unit clauses.

(defun do-unit-conflict (clause vec)
  (let* ((lit (car (cl~literals clause)))
	 (pos (lit~positive-p lit))
	 (length (fill-pointer vec)))
    (dotimes (i length)
      (let ((val (aref vec i)))
	(when (and val
		   (unit-p val)
		   (if pos
		       (not (lit~positive-p (car (cl~literals val))))
		       (lit~positive-p (car (cl~literals val))))
		   (unify-lits lit (car (cl~literals val))))
	  (resolve-clauses clause val)
	  (return-from do-unit-conflict nil))))))



