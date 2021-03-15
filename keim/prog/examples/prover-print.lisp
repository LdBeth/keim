(in-package :resprover)

;;; PRINTING
;;; Here we define a style of printing proofs, and a command PRINT.
;;; Farther below we have a style for printing commands and command categories,
;;; which is used by the ? command.

(defun get-used-clauses (proof)
  (let ((empty (res~proof-empty-clause proof)))
    (do ((clauses-to-save nil)
	 (clauses-to-check nil (cdr clauses-to-check))
	 (current empty (car clauses-to-check)))
	((null current) (remove-if #'res~initial-p clauses-to-save))
      (pushnew current clauses-to-save)
      (dolist (clause (just~nodes (node~justification current)))
	(pushnew clause clauses-to-check)))))

(pp~defstyle resprover 
  :parent nd-pretty
  :pprint-methods
  ((string (lambda (s l) (write-string l s)))
   (res+proof
    (lambda (stream proof)
      (let ((*standard-output* stream))
	(terpri)
	(pprint-logical-block (nil nil)
	(write-string "Problem: ")
	(write-string (string (keim~name proof)))
	(pprint-newline :mandatory)
	(write-string (if (res~proof-empty-clause proof)
		      "Proven." "Not proven."))
	(pprint-newline :mandatory)
	(pprint-logical-block (nil (res~proof-clauses proof) 
				   :per-line-prefix "  ")
	 (write-string "Initial clauses:")
	 (loop (pprint-exit-if-list-exhausted)
	   (write (pprint-pop) :pretty t)
	   (pprint-newline :mandatory)))
	(pprint-newline :mandatory)
	(pprint-logical-block (nil (if (res~proof-empty-clause proof)
				       (get-used-clauses proof)
				       (proof~steps proof))
				   :per-line-prefix "  ")
	 (pprint-exit-if-list-exhausted)
	 (write "Derived clauses:" :pretty t)
	  (loop (pprint-exit-if-list-exhausted)
		(write (pprint-pop) :pretty t)
		(pprint-newline :mandatory)))))))
   (cl+clause
    (lambda (stream clause)
      (let ((*standard-output* stream))
	(pprint-newline :mandatory)
	(write-string (string (keim~name clause)))
	(write-char #\.)
	(if (cl~literals clause)
	    (pprint-logical-block (nil (cl~literals clause))
       	      (pprint-tab :section 4 1)
	      (loop (pprint-exit-if-list-exhausted)
		    (write (pprint-pop) :pretty t)
		    (write-char #\space)
		    (pprint-newline :fill)))
	    (write-string "    BOX"))
	(write-char #\space)
	(pprint-logical-block (nil nil)
	  (write (node~justification clause) :pretty t)))))
   (res+resolution
    (lambda (stream just)
      (let ((*standard-output* stream))
	(pprint-logical-block (nil nil)
	(write-string "Res: ")
	(pprint-indent :current 1)
	(pprint-logical-block (nil (res~resolution-clauses just)
				   :prefix "<" :suffix ">")
	(write-string (string (keim~name (pprint-pop))))
	(write-char #\,)
	(pprint-newline :fill)
	(write-string (string (keim~name (pprint-pop)))))
	(write-char #\space)
	(write (res~just-unifier just) :pretty t)))))
   (res+initial
    (lambda (stream just)
      (declare (ignore just))
      (write-string "Initial" stream)))
   (res+factoring
    (lambda (stream just)
      (let ((*standard-output* stream))
	(pprint-logical-block (nil nil)
	(write-string "Fac: ")
	(pprint-indent :block 1)
	(pprint-logical-block (nil (list (res~factoring-clause just))
				   :prefix "<" :suffix ">")
	(write-string (string (keim~name (pprint-pop)))))
	))))
   (subst+substitution
    (lambda (stream sub)
      (let ((*standard-output* stream)
	    (pairs (mapcar #'list (subst~domain sub) (subst~codomain sub))))
	(pprint-logical-block (nil pairs :prefix "[" :suffix "]")
	  (loop (pprint-exit-if-list-exhausted)
	    (pprint-logical-block (nil (pprint-pop))
	      (loop (write (pprint-pop) :pretty t)
		    (pprint-exit-if-list-exhausted)
		    (write-string " :-> ")))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\;)
	    (pprint-newline :linear))))))))
	
(com~defcommand print
  (function print-res-proof)
  (argnames proof)
  (argtypes res-proof)
  (arghelps "A proof to print")
  (defaults ((if (res~proof-p *current-proof*)
		 *current-proof* (com~unspecified))))
  (categories resprover)
  (help "Print the last proof worked on."))

(defun print-res-proof (problem)
  (inter~terpri *current-interface*)
  (inter~output-object *current-interface* 
    (if (typep problem 'res+proof)
      (pp~pprint-to-string problem 'resprover)
      (post~pprint problem nil))))

;;; PRINTING COMMAND DESCRIPTIONS

(pp~defstyle 
 command-print
 :help "Style used for printing list of commands"
 :pprint-methods
  ((com+category (lambda (s l) (let ((*standard-output* s))
				 (pprint-category l))))
   (com+command  (lambda (s l) (let ((*standard-output* s)) 
				 (pprint-command l))))
   (string (lambda (s l) (write-string l s)))))

(defun pprint-category (category)
  (declare (special oc*already-com oc*already-cat))
  (unless (member category oc*already-cat)
  (pushnew category oc*already-cat)
  (let* ((catname (keim~name category))
	 (subcategories
	  (set-difference 
	   (mapcar #'com~find-category
		   (sort (copy-list (com~cat-subcategories category))
			 #'string-lessp))
	   oc*already-cat))
	 (commands (let ((coms nil))
		     (maphash #'(lambda (key val)
				  (declare (ignore key))
				  (when (member catname
						(com~categories val))
				    (push val coms)))
			      (com~command-hash-table))
		     (sort 
		      (set-difference coms
				      oc*already-com)
		      #'string-lessp :key #'keim~name)
		       )))
    (when (or commands subcategories)
      (pprint-logical-block (nil nil)
	(write (help~help-string category))
	(pprint-newline :mandatory)
	(pprint-logical-block (nil commands)
	  (loop (pprint-exit-if-list-exhausted)
		(pprint-tab :section 4 1)
		(write (pprint-pop))
		(pprint-newline :mandatory)))
	(pprint-logical-block (nil subcategories)
        (loop (pprint-exit-if-list-exhausted)
      		(pprint-tab :section 4 1)
		(write (pprint-pop))
		(pprint-newline :mandatory))
	))))))

(defun pprint-command (command)
  (declare (special oc*already-com oc*already-cat))
  (push command oc*already-com)
  (flet ((get-words (string)
	  (let ((newstring (substitute #\newline #\space 
				       string :test #'char=)))
	    (with-input-from-string (in newstring)
	      (do ((res nil)
		   (word (read-line in nil :eof) (read-line in nil :eof)))
		  ((eq word :eof) (nreverse res))
		(push word res))))))
    (let ((help-string (help~help-string command)))
    (pprint-logical-block (nil nil)
      (write-string (string (keim~name command)))
      (write-char #\:)
      (write-char #\space)
      (pprint-indent :current 0)
      (pprint-logical-block (nil (get-words help-string))
	(loop (pprint-exit-if-list-exhausted)
	      (write (pprint-pop))
	      (write-char #\space)
	      (pprint-newline :fill)))))))


(defun pprint-category-top (category)
  (let ((oc*already-com nil)(oc*already-cat nil))
    (declare (special oc*already-com oc*already-cat))
    (pp~with-style 'command-print (pp~pprint category))))


(defun report-clause (clause)
  (if *current-interface*
    (inter~output-object 
     *current-interface*
     (pp~pprint-to-string clause 'resprover))
    (format t "~A~%" (pp~pprint-to-string clause 'resprover)))
  )

(defun report-message (string)
  (if *current-interface*
      (progn (inter~terpri *current-interface*)
	     (inter~output-object 
	      *current-interface*
	      string))
    (format t "~A~%" string)))

