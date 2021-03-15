(in-package :resprover)

;;; define a command interface for prover
(eval-when (load compile eval)
(arg~deftype problem
 (predicate prob~p)
 (read-function read-problem)
 (help "a KEIM problem"))
)

(defun read-problem-sym-or-string (obj)
  (let ((prob (prob~find-problem obj)))
    (if prob 
	prob
	(arg~signal-wrong-type 'problem obj))))

(defmethod read-problem ((obj string) &rest others)
  (declare (ignore others))
  (read-problem-sym-or-string obj))

(defmethod read-problem ((obj symbol) &rest others)
  (declare (ignore others))
  (read-problem-sym-or-string obj))

(defmethod read-problem ((obj prob+problem) &rest others)
  (declare (ignore others))
  obj)

(eval-when (load compile eval)
(arg~deftype res-proof
 (predicate res~proof-p)
 (read-function read-res-proof)
 (help "a KEIM resolution proof"))
)

(defun read-res-proof-sym-or-string (obj)
  (let ((prob (or (res~find-proof obj)
		  (prob~find-problem obj))))
    (cond ((res~proof-p prob) prob)
	  ((prob~problem-p prob) (read-res-proof prob))
	  (t (arg~signal-wrong-type 'res-proof obj)))))

(defmethod read-res-proof ((obj string) &rest others)
  (declare (ignore others))
  (read-res-proof-sym-or-string obj))

(defmethod read-res-proof ((obj symbol) &rest others)
  (declare (ignore others))
  (read-res-proof-sym-or-string obj))

(defmethod read-res-proof ((obj res+proof) &rest others)
  (declare (ignore others))
  obj)

(defmethod read-res-proof ((obj prob+problem) &rest others)
  (declare (ignore others))
  (let* ((newproof (cnf~normalize-problem obj)))
    newproof))

(eval-when (load compile eval)
(com~defcategory resprover
  (direct-supercategories top)
  (help "Resolution prover commands")))

(defun prover (problem)
  (let ((newproblem (cnf~normalize-problem problem)))
    (setq *current-proof* newproblem)
    (unwind-protect
	(run-prover newproblem)
      (setq *last-problem* newproblem)
    )))

(defvar *ready-to-roll* nil "When T, signals that we have just initialized
a problem for resolution and can run GO now.")

(defun start-proof (problem)
  (let ((newproblem (cnf~normalize-problem problem)))
    (setq *current-proof* newproblem)
    (initialize-initial-clauses newproblem)
    (setq *ready-to-roll* t)
    ))

(defun do-resolution ()
  (if *ready-to-roll*
      (progn (setq *ready-to-roll* nil) (run-prover *current-proof*))
      (report-message "Not ready to run resolution.  Initialize a problem with
the command PROVE.")))

  
(com~defcommand prove
  (argnames problem)
  (argtypes problem)
  (arghelps "A KEIM problem")
  (categories resprover)
  (function start-proof)
  (help "Initialize a problem for resolution proof."))

(com~defcommand go
  (argnames )
  (argtypes)
  (arghelps)
  (categories resprover)
  (function do-resolution)
  (help "Run resolution on the initialized problem."))

;;; First we define a condition which will be signaled when we want to leave
;;; the PROVERtop level, LEAVE-PROVER, which inherits from SYS+ABORT.

(sys~define-condition leave-prover (sys+abort)
  ()
  (lambda (cond stream) (declare (ignore cond)) 
	  (format stream "Leaving PROVER."))
  (:documentation "Signals that we should leave PROVER."))


(defvar *current-interface* nil)
(defvar *current-command* nil)
(defvar *current-style* 'resprover)
(defvar *debugger-on-break* t "Set to T if you want to go into the debugger
when a break or error occurs.  If NIL, the top-level will keep you from
hitting the debugger." )


(com~defcommand exit
  (function leave)
  (argnames )
  (argtypes )
  (categories resprover)
  (help "Leave the PROVER top level."))

(defun leave ()
  (sys~signal (sys~make-condition 'leave-prover)))

;;; The command BREAK just goes into the debugger.

(com~defcommand break
  (function break)
  (argnames )
  (argtypes )
  (categories resprover)
  (help "Go directly into the debugger."))

(defun break ()
  (invoke-debugger (make-condition 'simple-break)))

(defun prover-top (&optional (interface (asi~create)))
  (declare 
   (authors nesmith)
   (input "An interface (creates one of type ASI~CREATE by default.")
   (value "Undefined")
   (effect "After initialization, runs a read-eval-print loop, reading a
command, getting its arguments and applying the command."))
  (let ((*current-interface* interface)
	(*current-command* nil))
    (labels ((loop-for-arg (name type help default)
	       (let ((prompt (if name (format nil "~A (~A) ~A: " 
					      name type help)
			       "PROVER: ")))
		   (loop
		     (sys~handler-case
		      (progn
			(inter~terpri *current-interface*)
			;; returns out of loop only if normal exit of prompt
			;; occurs. any error puts us in the handler below
			(return-from 
			 loop-for-arg
			 (if (com~specified-arg-p default)
			     (inter~prompt-for-input-with-default
			      *current-interface* prompt type 
			      default)
			     (inter~prompt-for-input 
			      *current-interface* prompt type))))
		      (arg+input-error (c) (inter~print-error 
					    *current-interface* c)
				       (inter~terpri *current-interface*))))))
	     (loop-for-args (names types helps command input-args)
	       (let ((real-args nil))
		 (setq real-args
		   (mapcar #'(lambda (name type help arg)
			       (if (com~specified-arg-p arg)
				   (sys~handler-case
				    (arg~read-type type arg)
				    (arg+input-error (c) (inter~print-error 
							  *current-interface* c)
						     (inter~terpri *current-interface*)
						     (loop-for-arg name type help (com~unspecified))))
				 arg))
			   names types helps input-args))
		 (loop
		   (let* ((poss-args (com~apply-defaults command real-args))
			  (n (or (position-if-not #'com~specified-arg-p real-args)
				 (mismatch poss-args real-args
					   :test-not 
					   #'(lambda (x y) (and (com~specified-arg-p x)
								(not (com~specified-arg-p y))))))))
		     (if n
			 (setf (nth n real-args)
			   (loop-for-arg (nth n names) (nth n types) (nth n helps) (nth n poss-args)))
		       (return-from loop-for-args real-args))))))
	     (main-loop ()
	       (let ((input-list nil))
		 (inter~terpri *current-interface*)
		 (inter~output-object *current-interface* "PROVER: ")
		 (setq input-list (sys~handler-case
				   (asi~linereadp (asi~input *current-interface*))
				   (asi+no-input-error () nil)))
		 (when input-list
		   (setq *current-command*
		     (sys~handler-case
		      (arg~read-type 'command (car input-list))
		      (arg+input-error (c) (inter~print-error 
					    *current-interface* c)
				       (inter~terpri *current-interface*)
				       (return-from main-loop))))
		   (let* ((argnames (com~argnames *current-command*))
			  (argtypes (com~argtypes *current-command*))
			  (arghelps (com~arghelps *current-command*))
			  (input-args 
			   (append (if (cdr input-list)
				       (subseq (cdr input-list) 0 
					       (min (length argtypes)
						    (length input-list)))
				       nil)
				   (make-list (max 0 
						   (- (length argtypes) 
						      (length (cdr input-list))))
					      :initial-element (com~unspecified))))
			  (fun (com~function *current-command*))
			  (real-args nil))
		     (setq real-args
		       (loop-for-args argnames argtypes arghelps *current-command* input-args))
		     (apply (symbol-function fun) real-args))))))

      (with-simple-restart (abort "Exit PROVER top level.")
	(loop
	 (with-simple-restart (abort "Return to PROVER top level." )
	   (if *debugger-on-break*
	       (sys~handler-case
		(main-loop)
		(leave-prover (c) 
			      (declare (ignore c)) 
			      (return-from prover-top nil))
		(com+abort-command (c) (inter~print-warning interface c)))
	       (sys~handler-case
		(main-loop)
		(leave-prover (c) (declare (ignore c)) 
				   (return-from prover-top nil))
		(com+abort-command (c) (inter~print-warning interface c))
		(break (c) 
		       (inter~print-warning 
			*current-interface* c))
		(simple-error (c)
			      (inter~print-warning 
			       *current-interface* c))
		(sys+error (c) (inter~terpri *current-interface*)
			   (inter~print-warning *current-interface* c)
			   (inter~terpri *current-interface*)))
	       )))))))
  

;;; PRINTING LIST of COMMANDS

(com~defcommand ?
  (function ?)
  (argnames )
  (argtypes )
  (categories resprover)
  (help "Show all RESPROVER commands."))

(defun command-p (thing)
  (and (com~command-p thing)
       (find-if #'(lambda (x) (typep (com~find-category x)
				     (class-name 
					(class-of 
					 (com~find-category 'resprover)))))
		(com~categories thing))))

(defun ? ()
  (inter~output-object 
   *current-interface*
  (pprint-category-top (com~find-category 'resprover))))


;;; COMMAND HELP

(com~defcommand help
  (function help)
  (argnames command)
  (argtypes command)
  (arghelps "Command for which help is desired")
  (categories resprover)
  (defaults ((com~find-command 'help)))
  (help "Show help for a command."))

(defun help (command)
  (let* ((argnames (com~argnames command))
	 (argtypes (com~argtypes command))
	 (arghelps (com~arghelps command))
	 (help (com~help command))
	 (line1* nil)
	 (line2* nil)
	 (argnamehelppairs (mapcar #'(lambda (x y) (list x y)) 
				   argnames arghelps))
	 (str nil))
    (do* ((line1 (mapcar #'symbol-name argnames) (cdr line1))
	  (line2 (mapcar #'symbol-name argtypes) (cdr line2))
	  (line1-lengths (mapcar #'length line1) (cdr line1-lengths))
	  (line2-lengths (mapcar #'length line2) (cdr line2-lengths))
	  (tmp (format nil "PROVER: ~A" (keim~name command)))
	  (ltmp (length tmp)))
	((null line1) (setq line1* (cons tmp (nreverse line1*)))
	 (setq line2* (cons (make-string ltmp :initial-element #\space)
			    (nreverse line2*))))
      (let ((l1 (car line1))(l1l (car line1-lengths))
	    (l2 (car line2))(l2l (car line2-lengths)))
	(if (> l1l l2l)
	    (progn
	      (let* ((diff (- l1l l2l)) (diff2 (floor diff 2)))
		(push l1 line1*)
		(push (format nil "~A~A~A" 
			      (make-string diff2 
					   :initial-element
					   #\space)
			      l2
			      (make-string (- diff diff2)
					   :initial-element
					   #\space))
		      line2*)))
	    (progn
	      (let* ((diff (- l2l l1l)) (diff2 (floor diff 2)))
		(push l2 line2*)
		(push (format nil "~A~A~A" 
			      (make-string diff2 
					   :initial-element
					   #\space)
			      l1
			      (make-string (- diff diff2)
					   :initial-element
					   #\space))
		      line1*))))))
    ;; a cheapo way to do it, not very good 
    (setq str
	  (format nil "~%~A is a command.~%~
                       ~A~%The command format for ~A is:~%~%~
                       ~{~A  ~}~%~{~A  ~}~%~
                       ~@[The arguments have the following meanings:~%~
                       ~:{~A : ~A~%~}~%~]"
		  (keim~name command) help (keim~name command) 
		  line1* line2* argnamehelppairs))
    (inter~output-object *current-interface* str)
    
    (when (rule~find-rule (keim~name command))
      (inter~output-object 
       *current-interface*
       (with-output-to-string (*standard-output*)
	 (rule~describe (keim~name command))))
      (inter~terpri *current-interface*))
  ))

