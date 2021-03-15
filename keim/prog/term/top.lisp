;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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


(mod~defmod top :uses (abstr appl keim mod pos sym term )
	    :documentation "Extended functionality for simple terms."
	    :exports (
		      top~term-linear-p
		      top~free-variables
		      top~=free-variables
		      top~replace-term
		      top~replace-term!
		      top~replace-term!!
		      top~replace-terms
		      top~replace-terms!
		      top~replace-terms!!
		      top~replace-at-position
		      top~replace-at-position!
		      top~replace-at-position!!
		      top~replace-free-variables
		      top~replace-free-variables!
		      top~replace-free-variables!!
		      top~rename
		      top~rename!
		      top~rename!!
		      top~replace-free-variables-and-rename
		      top~replace-free-variables-and-rename!
		      top~replace-free-variables-and-rename!!
		      top~bind-term!
		      top~bind-terms!
		      top~unbind-terms!
		      top~clear-bindings!
		      top~let-terms-bindings!
		      top~new-binding-list!
		      top~unbind-binding-list!
		      top~binding-list!
		      top~push-bindings!
		      top~pop-bindings!
		      top~unbind-stack!
		      top~insert-bindings!
		      top~insert-bindings!!
		      top~swap-bindings
		      top~swap-bindings!
		      top~all-unbound-symbols
		      )
	    )


#{\section{Operations on terms}\label{mod:top}
In this module you will a lot of useful procedures for manipulating  terms.
#}

(defvar top*label-list nil)
;;where is the comment???????

(defvar top*label-stack nil)
;;where is the comment???????

(defun top=label-terms (terms labels)
  (declare (edited  "31-1-1987")
	   (authors Richts)
	   (input   "A list of terms and a list of LISP-objects of equal length.")
	   (effect  "The label cells of TERMS are set to LABELS.")
	   (value   "Undefined."))
  (setf top*label-list (append terms top*label-list))
  (mapc #'(lambda (term label)
	    (term~set-label! term label))
	terms labels))

(defun top=label-symbols (symbols labels)
  (declare (edited  "31-1-1987")
	   (authors Richts)
	   (input   "A list of symbols and a list of LISP-objects of equal length.")
	   (effect  "The symbol-label cells of SYMBOLS are set to LABELS.")
	   (value   "Undefined."))
  (setf top*label-list (append symbols top*label-list))
  (mapc #'(lambda (symbol label)
	    (sym~set-label! symbol label))
	symbols labels))


(defun top=label-term (term label)
  (declare (edited  "31-1-1987")
	   (authors Richts)
	   (input   "A term and an object.")
	   (effect  "The label cell of TERM is set to LABEL.")
	   (value   "LABEL."))
  (push term top*label-list)
  (term~set-label! term label))

(defun top=label-symbol (symbol label)
  (declare (edited  "31-1-1987")
	   (authors Richts)
	   (input   "A symbol and an object.")
	   (effect  "The label cell of SYMBOL is set to LABEL.")
	   (value   "LABEL."))
  (push symbol top*label-list)
  (sym~set-label! symbol label))


(defun top=unlabel-terms (terms)
  (declare (edited  "31-1-1987")
	   (authors Richts)
	   (input   "A list of terms.")
	   (effect  "The label cells of TERMS are set to NIL.")
	   (value   "Undefined."))
  (mapc #'(lambda (term)
	    (term~set-label! term nil)
	    (if (sym~p term)
		(sym~set-label! term nil)))
	terms))


(defun top=clear-term-labels (term)
  (declare (edited  "19-SEP-1991 15:22")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "The label cells of all subterms of TERM are set to NIL.")
	   (value   "Undefined."))
  (if (listp term)
      (mapc #'(lambda (subterm) (top=clear-term-labels subterm)) term)
      (progn (term~set-label! term nil)
	     (if (sym~p term)
		 (term~set-label! term nil)
		 (mapc #'(lambda (subterm) (top=clear-term-labels subterm)) (term~subterms term))))))


(defun top=unlabel-label-list ()
  (declare (edited  "19-SEP-1991 15:28")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "The label cells of all terms in the current (top=label-list) are set to NIL and the (top=label-list)"
		    "is destroyed. The current (top=label-list) becomes the previous one or a new one if there isn't one.")
	   (value   "Undefined."))
  (top=unlabel-terms top*label-list)
  (setf top*label-list nil))

(defun top=label-list ()
  top*label-list)

#{\subsection{Term replacement functions}
The following procedures use the labelling mechanism of \keim\ as defined
in \ref{mod:term}. Hence they work properly only if the label slot
is not yet set. The procedures ending with {\vb !!} may only be applied to
terms with unshared terms. Unshared terms are terms, in which each subterm object
occurs exactly once and nowhere else. E.g. {\vb (P x)} is unshared if {\vb P}
and {\vb x} do not occur elsewhere; {\vb TERM = (Q x x)} is unshared if {\vb Q} and the two
{\vb x} do not occur elsewhere and {\vb (EQ (FIRST (APPL~ARGUMENTS TERM))
					    (SECOND (APPL~ARGUMENTS TERM))) --> NIL}.
Unshared terms can be obtained by {\vb TERM~COPY}, for instance.
#}

(defun top~term-linear-p (term)
   (declare (edited  "15-2-1988")
	    (authors Richts)
	    (input   "A term or a nested termlist.")
	    (effect  "None.")
	    (value   "True iff each free variable in {\vb TERM} occurs only once.")
	    (example "(P x y) --> T"
		     "(P x x) --> NIL"))
   (unwind-protect
       (top=term-linear-p term)
     (top=unlabel-label-list)))

(defgeneric top=term-linear-p (term)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A term or nested termlist.")
	   (effect  "None.")
	   (value   "True iff each free variable in TERM occurs only once."))
  (:method ((termlist list))
   (every #'top=term-linear-p termlist))
  (:method ((variable sym+var))
   (let ((label (sym~label variable)))
     (unless label (top=label-symbol variable t))
     (not (eq label t))))
  (:method ((constant sym+const))
   t)
  (:method ((application appl+appl))
   (every #'top=term-linear-p (term~subterms application)))
  (:method ((abstraction abstr+abstr))
   (let* ((bound-variable (abstr~bound-variable abstraction))
	  (old-label (sym~label bound-variable)))
     (unwind-protect
	 (progn
	   (sym~set-label! bound-variable :bound)
	   (every #'top=term-linear-p (term~subterms abstraction)))
       (sym~set-label! bound-variable old-label)))))


(defun top~free-variables  (term)
  (declare (edited  "05-oct-1990 17:57")
	   (authors Richts)
	   (input   "A term or nested termlist.")
	   (effect  "None.")
	   (value   "A list of all free variables in TERM,"
		    "i.e. all variables not in the scope of an abstraction.")
	   (example "(P x y) --> (Y X)"
		     "(P x x) --> (X)"))
  (unwind-protect
      (progn
	(top~=free-variables term)
	(top=label-list))
    (top=unlabel-label-list)))

(term~defgeneric top~=free-variables ((term)) ;the recursive part of top~free-variables.
		 (declare (edited  "31-OCT-1991 13:19")
			  (authors RICHTS)
			  (input   "A term or nested termlist.")
			  (effect  "All free variables are labeled with T and collected in the label-list.")
			  (value   "Undefined."))
		 (:method ((termlist list))
			  (mapc #'top~=free-variables termlist))
		 (:method ((variable sym+var))
			  (unless (sym~label variable)
			    (top=label-symbol variable t)))
		 (:method ((constant sym+const))
			  nil)
		 (:method ((application appl+appl))
			  (mapc #'top~=free-variables (term~subterms application)))
		 (:method ((abstraction abstr+abstr))
			  (let* ((bound-variable (abstr~bound-variable abstraction))
				 (old-label (sym~label bound-variable)))
			    (unwind-protect
				(progn
				  (sym~set-label! bound-variable t)
				  (top~=free-variables (abstr~scope abstraction)))
			      (sym~set-label! bound-variable old-label))))
		 )



(defun top~replace-term (object old-term new-term &key (test #'keim~equal))
  (declare (edited  "31-OCT-1991 11:38")
	   (authors RICHTS)
	   (input   "A structure containing terms, two terms and a test-function.")
	   (effect  "None.")
	   (value   "A new structure with new, unshared terms where all occurrences of  OLD-TERM"
		    "are replaced by NEW-TERM.")
	   (example "(G X Y)   X   C --> (G C Y)"
		    "(G X X)   X   C --> (G C C)"
		    "(FORALL [X].(Q X)   X   C --> (FORALL [X].(Q X)"
		    "(FORALL [X].(Q X)   X   C :test #'term~equal-p-ab --> (FORALL [X].(Q C)"))
  (top~replace-terms object (list old-term) (list new-term) :test test))

(defun top~replace-term! (object old-term new-term &key (test #'keim~equal))
  (declare (edited  "31-OCT-1991 11:38")
	   (authors RICHTS)
	   (input   "A structure containing terms, two terms and a test-function.")
	   (effect  "Non-term objects are changed destructively.")
	   (value   "If OBJECT is a term, a new, unshared term or else"
		    "the changed OBJECT with new, unshared terms, where"
		    "all occurrences of OLD-TERM are replaced by NEW-TERM.")
	   (example "see TOP~REPLACE-TERM"))
  (top~replace-terms! object (list old-term) (list new-term) :test test))

(defun top~replace-term!! (object old-term new-term &key (test #'keim~equal))
  (declare (edited  "31-OCT-1991 11:48")
	   (authors RICHTS)
	   (input   "A structure containing unshared terms, two terms and a test-function.")
	   (effect  "All occurrences of OLD-TERM are replaced destructively by NEW-TERM.")
	   (value   "The changed OBJECT, still unshared.")
	   (example "see first example in  TOP~REPLACE-TERM"
		    "Pay attention! The second example may lead to non-defined states"
		    "since the shared variable X in the binder can be replaced by an"
		    "arbitrary term, what leads out of \post."))
  (top~replace-terms!! object (list old-term) (list new-term) :test test))

(defun top=assoc (object domain codomain test)
  (declare (edited  "26-JUN-1992 11:45")
	   (authors KOHLHASE )
	   (input   "An term, two lists of terms and a function to compare terms.")
	   (effect  "None.")
	   (value   "The term-lists can be seen as the domain and co-domain of a table defining a function."
		    "The value is the image of OBJECT in this table, where OBJECT is compared to the terms in DOMAIN by TEST."))
  (some #'(lambda (object1 object2)
	    (if (funcall test object object1)
		object2))
	domain codomain))


(defgeneric top~replace-terms (object old-terms new-terms &key test)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A structure containing terms, two lists of terms and a test-function.")
	   (effect  "None.")
	   (value   "A new structure with new, unshared term where all occurrences of OLD-TERMS in TERM"
		    "(tested for equality with TEST) are replaced by the corresponding terms in NEW-TERMS.")
	   (example "(P X Y) (LIST X Y)  (LIST A B) -->  (P A B)"))
  (:method ((termlist list) old-terms new-terms &key (test #'keim~equal))
   (mapcar #'(lambda (term)
	       (top~replace-terms term old-terms new-terms :test test))
	   termlist))
  (:method ((variable sym+var) old-terms new-terms &key (test #'keim~equal))
   (let ((new-term (top=assoc variable old-terms new-terms test)))
     (if new-term (term~copy new-term) (term~copy variable))))
  (:method ((constant sym+const) old-terms new-terms &key (test #'keim~equal))
   (let ((new-term (top=assoc constant old-terms new-terms test)))
     (if new-term (term~copy new-term) (term~copy constant))))
  (:method ((application appl+appl) old-terms new-terms &key (test #'keim~equal))
   (let ((new-term (top=assoc application old-terms new-terms test)))
     (if new-term
	 (term~copy new-term)
	 (appl~create (top~replace-terms (appl~function application) old-terms new-terms :test test)
			   (top~replace-terms (appl~arguments application) old-terms new-terms :test test)))))
  (:method ((abstraction abstr+abstr) old-terms new-terms &key (test #'keim~equal))
   (let ((new-term (top=assoc abstraction old-terms new-terms test)))
     (if new-term (term~copy new-term)
	 (abstr~create (term~copy (abstr~bound-variable abstraction))
			    (top~replace-terms (abstr~scope abstraction)
						(cons (abstr~bound-variable abstraction) old-terms)
						(cons nil new-terms)
						:test test))))))

(defgeneric top~replace-terms! (object old-terms new-terms &key test)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A structure containing terms, two lists of terms and a test-function.")
	   (effect  "Non-term objects are changed destructively.")
	   (value   "If OBJECT is a term, a new, unshared term or else"
		    "the changed OBJECT with new, unshared terms, where all occurrences of OLD-TERMS in TERM"
		    "(tested for equality with TEST) are replaced by the corresponding terms in NEW-TERMS.")
	   (example "see {\\vb TOP~REPLACE-TERM}"))
  (:method ((termlist list) old-terms new-terms &key (test #'keim~equal))
   (mapl #'(lambda (termlist-tail)
	     (setf (car termlist-tail)
		   (top~replace-terms! (car termlist-tail) old-terms new-terms :test test)))
	 termlist)
   termlist)
  (:method ((term term+term) old-terms new-terms &key (test #'keim~equal))
   (top~replace-terms term old-terms new-terms :test test)))


(defgeneric top~replace-terms!! (object old-terms new-terms &key test)
  (declare (edited  "31-OCT-1991 11:54")
	   (authors RICHTS)
	   (input   "A structure containing unshared terms, two lists of terms and a test-function.")
	   (effect  "All occurrences of OLD-TERMS are replaced by the corresponding terms in NEW-TERMS destructively.")
	   (value   "The changed OBJECT, with still unshared terms.")
	   (example "see {\\vb TOP~REPLACE-TERM}"
		    "Pay attention! The shared terms may lead to non-defined states"
		    "since the shared symbols may be inconsistently replaced."
		    "Cp. {\\vb TOP~REPLACE-TERM!!}"))
  (:method ((termlist list) old-terms new-terms &key (test #'keim~equal))
	   (mapc #'(lambda (term)
		     (top~replace-terms!! term old-terms new-terms :test test))
		 termlist)
	   termlist)
  (:method ((variable sym+var) old-terms new-terms &key (test #'keim~equal))
	   (let ((new-term (top=assoc variable old-terms new-terms test)))
	     (if new-term (term~set-term variable new-term)))
	   variable)
  (:method ((constant sym+const) old-terms new-terms &key (test #'keim~equal))
	   (let ((new-term (top=assoc constant old-terms new-terms test)))
	     (if new-term (term~set-term constant new-term)))
	   constant)
  (:method ((application appl+appl) old-terms new-terms &key (test #'keim~equal))
	   (let ((new-term (top=assoc application old-terms new-terms test)))
	     (if new-term
		 (term~set-term application new-term)
		 (progn (top~replace-terms!! (appl~function application) old-terms new-terms :test test)
			(top~replace-terms!! (appl~arguments application) old-terms new-terms :test test))))
	   application)
  (:method ((abstraction abstr+abstr) old-terms new-terms &key (test #'keim~equal))
	   (let ((new-term (top=assoc abstraction old-terms new-terms test)))
	     (if new-term (term~set-term abstraction new-term)
		 (top~replace-terms!! (abstr~scope abstraction)
				      (cons (abstr~bound-variable abstraction) old-terms)
				      (cons nil new-terms)
				      :test test)))
	   abstraction))

(defgeneric top~replace-at-position (term position new-term)
  (declare (edited  "20-7-1988")
	   (authors Richts)
	   (input   "A term, a position and another term.")
	   (effect  "None.")
 	   (value   "The new, unshared term where the subterm at {\\vb POSITION}"
		    "is replaced by {\\vb NEW-TERM}. {\\vb NEW-TERM} must have the same type"
		    "as the replaced subterm.")
	   (example "some typical examples would be:"
		    "\begin{lisp}"
		    "(a (0) b)                   -> b"
		    "((f a b) (0) g)             -> (g a b)"
		    "((f a b) (2) c)             -> (f a c)"
		    "((f a (g b)) (2 1) (g b))   -> (f a (g (g b)))"
		    "(ALL (x y) (f x y)) (1) a)  -> (ALL (x y) a)"
		    "\end{lisp}"
		    "\begin{latex}"
		    "\\parbox[t]{4cm}{(a nil b)} \\hfill $\\rightarrow$ \\hfill"
		    "  \\parbox[t]{7.1cm}{b}"
		    "\\parbox[t]{4cm}{((f a b) (0) g)} \\hfill $\\rightarrow$ \\hfill"
		    "  \\parbox[t]{7.1cm}{(g a b)}"
		    "\\parbox[t]{4cm}{((f a b) (2) c)} \\hfill $\\rightarrow$ \\hfill"
		    "  \\parbox[t]{7.1cm}{(g a c)}"
		    "\\parbox[t]{4cm}{((f a (g b)) (2 1) (g b))} \\hfill $\\rightarrow$ \\hfill"
		    "  \\parbox[t]{7.1cm}{(f a (g (g b)))}"
		    "\end{latex}"))
  (:method ((variable sym+var) position new-term)
   (if (pos~empty-p position)
       (term~copy new-term)
       (error "Position ~A is not in variable ~A" position variable)))
  (:method ((constant sym+const) position new-term)
   (if (pos~empty-p position)
       (term~copy new-term)
       (error "Position ~A is not in constant ~A" position constant)))
  (:method ((application appl+appl) position new-term)
   (cond ((pos~empty-p position)
	  (term~copy new-term))
	 ((zerop (pos~first position))
	  (appl~create (top~replace-at-position (term~top application) (pos~rest position) new-term)
				   (mapcar #'term~copy (appl~arguments application))))
	 (t (let ((number (pos~first position)))
	      (prog1
		(appl~create (term~copy (term~top application))
				  (mapcar #'(lambda (argument)
					      (decf number)
					      (if (zerop number)
						  (top~replace-at-position argument (pos~rest position) new-term)
						  (term~copy argument)))
					  (appl~arguments application)))
		(when (plusp number) (error "Position ~A not in term ~A" position application)))))))
  (:method ((abstraction abstr+abstr) position new-term)
   (cond ((pos~empty-p position)
	  (term~copy new-term))
	 ((zerop (pos~first position))
	  (abstr~create (term~copy (abstr~bound-variable abstraction))
			     (top~replace-at-position (abstr~scope abstraction) (pos~rest position) new-term))))))

(defgeneric top~replace-at-position! (term position new-term)
  (declare (edited  "03-APR-1993 14:17")
	   (authors RICHTS)
	   (input   "A term, a position and another term.")
	   (effect  "The subterm in {\\vb TERM} at {\\vb POSITION} is replaced by"
		    "{\\vb NEW-TERM} destructively.")
 	   (value   "The new, unshared term where the subterm at {\\vb POSITION} is"
		    "replaced by {\\vb NEW-TERM}.")
	   (example "cp. {\\vb TOP~REPLACE-AT-POSITION}"))
  (:method ((term term+term) position new-term)
	   #+doof(top~replace-at-position! term position new-term)
	   (top~replace-at-position term position new-term)))          ;ohne ! ist hier richtig, joern

(defgeneric top~replace-at-position!! (term position new-term)
  (declare (edited  "03-APR-1993 14:17")
	   (authors RICHTS)
	   (input   "An unshared term, a position and another term.")
	   (effect  "The subterm in {\\vb TERM} at {\\vb POSITION} is replaced by a copy of {\\vb NEW-TERM}"
		    "destructively.")
 	   (value   "The changed TERM.")
	   (example "see {\\vb TOP~REPLACE-AT-POSITION}."
		    "Attention, the term must be unshared, otherwise an undefined state may occur."))
  (:method ((term term+term) position new-term)
	   (if (pos~empty-p position)
	       new-term
	       (progn
		 (term~set-term (term~at-position term position) (term~copy new-term))
		 term))))



#{\subsection{Renaming and Replacing Variables}
#}

(defun top~replace-free-variables (object variables new-terms)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A structure containing terms, a list of {\\vb VARIABLES} and a list of terms"
		    "of equal length and types.")
	   (effect  "None.")
	   (value   "A new structure with new, unshared terms where all occurrences of {\\vb VARIABLES}"
		    "in {\\vb OBJECT} are replaced by the corresponding terms in {\\vb NEW-TERMS}.")
	   (example "(G X Y)  (LIST X)  (LIST C) --> (G C Y)"
		    "(G X Y)  (LIST X Y) (LIST C (F D)) --> (G C (F D)))"))
  (unless (and (listp variables) (listp new-terms) 
	       (= (length variables) (length new-terms)))
    (error "~S and ~S must be lists of same length" variables new-terms))
  (top~replace-free-variables-and-rename object variables new-terms
					  :return-renaming nil :variables nil :not-variables nil))

(defun top~replace-free-variables! (object variables new-terms)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A structure containing terms, a list of variables and a list of terms of equal length.")
	   (effect  "Non-term objects are changed destructively.")
	   (value   "If {\\vb OBJECT} is a term, a new, unshared term or else"
		    "the changed {\\vb OBJECT} with new, unshared terms, where all occurrences of"
		    "{\\vb VARIABLES} in {\\vb OBJECT} are replaced by the corresponding terms"
		    "in {\\vb NEW-TERMS}.")
	   (example "see {\\vb TOP~REPLACE-FREE-VARIABLE}"))
  (unless (and (listp variables) (listp new-terms) 
	       (= (length variables) (length new-terms)))
    (error "~S and ~S must be lists of same length" variables new-terms))
  (top~replace-free-variables-and-rename! object variables new-terms
					   :return-renaming nil :variables nil :not-variables nil))

(defun top~replace-free-variables!! (object variables new-terms)
  (declare (edited  "31-OCT-1991 11:40")
	   (authors RICHTS)
	   (input   "A structure containing unshared terms, a list of variables and a list of terms of equal length.")
	   (effect  "All occurrences of VARIABLES are replaced by the corresponding terms in NEW-TERMS destructively.")
	   (value   "The changed OBJECT, still unshared.")
	   (example "see {\\vb TOP~REPLACE-FREE-VARIABLE}"
		    "Attention, the term must be unshared, otherwise an undefined state may occur."))
  (unless (and (listp variables) (listp new-terms) 
	       (= (length variables) (length new-terms)))
    (error "~S and ~S must be lists of same length" variables new-terms))
  (top~replace-free-variables-and-rename!! object variables new-terms
					    :return-renaming nil :variables nil :not-variables nil))


(defun top~rename (object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "13-DEC-1991 13:06")
	   (authors RICHTS)
	   (input   "A structure containing terms, a flag, a list of variables or t, another list of variables.")
	   (effect  "None.")
	   (value   "A free variable of {\\vb OBJECT} is renamed with a new variable"
		    "if it is in {\\vb VARIABLES} or {\\vb VARIABLES} = T and if it is not in {\\vb NOT-VARIABLES}:"
		    "  x --> x   ,if (not x in {\\vb VARIABLES}) or x in {\\vb NOT-VARIABLES}"
		    "  x --> z   ,if ({\\vb VARIABLES} = T  or  x in {\\vb VARIABLES})  and  (not x in {\\vb NOT-VARIABLES})"
		    "                where z is a new variable."
		    "If {\\vb RETURN-RENAMING} = T, three values are returned:"
		    "1. A new structure with new, unshared and renamed terms."
		    "2. The list of variables which have been renamed."
		    "3. The corresponding list of new variables."
		    "If RETURN-RENAMING = NIL only the renamed structure with unshared term is returned.")
	   (example "(G X Y)                         -->  (G x-5 x-6)           (Y X)         (x-6 x-5)"
		    "(G X Y) :variable (list X)      -->  (G x-9 Y)             (X)           (x-9)"
		    "(G X Y) :not-variables (list X) -->  (G X x-10)            (Y)           (x-10)"))
  (top~replace-free-variables-and-rename object nil nil :return-renaming return-renaming
					  :variables variables :not-variables not-variables))


(defun top~rename! (object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "13-DEC-1991 13:03")
	   (authors RICHTS)
	   (input   "A structure containing terms, a flag, a list of variables or t, another list of variables.")
	   (effect  "Non-term objects are changed destructively.")
	   (value   "A free variable of OBJECT is renamed with a new variable"
		    "if it is in VARIABLES or VARIABLES = T and if it is not in NOT-VARIABLES:"
		    "  x --> x   ,if (not x in VARIABLES) or x in NOT-VARIABLES"
		    "  x --> z   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                where z is a new variable."
		    "If RETURN-RENAMING = T, three values are returned:"
		    "1. If OBJECT is a term, a new, unshared and renamed term or else"
		    "   the changed OBJECT with new, unshared and renamed terms."
		    "2. The variables which have been renamed."
		    "3. The corresponding list of new variables."
		    "If RETURN-RENAMING = NIL only the renamed structure with unshared term is returned.")
	   (example "see {\\vb TOP~RENAME}"))
  (top~replace-free-variables-and-rename! object nil nil :return-renaming return-renaming
					   :variables variables :not-variables not-variables))


(defun top~rename!! (object &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "13-DEC-1991 13:03")
	   (authors RICHTS)
	   (input   "A structure containing unshared terms, a flag, a list of variables or t, another list of variables.")
	   (effect  "The free variables in {\\vb OBJECT} are renamed destructively.")
	   (value   "A free variable of {\\vb OBJECT} is renamed with a new variable"
		    "if it is in {\\vb VARIABLES} or {\\vb VARIABLES} = T and if it is not in {\\vb NOT-VARIABLES}."
		    "If {\\vb RETURN-RENAMING} = T, three values are returned:"
		    "1. The renamed {\\vb OBJECT}, with still unshared terms."
		    "2. The variables which have been renamed."
		    "3. The corresponding list of new variables."
		    "If {\\vb RETURN-RENAMING} = NIL only the renamed {\\vb OBJECT} is returned.")
	   (example "see {\\vb TOP~RENAME}"
		    "Attention, the term must be unshared, otherwise an undefined state may occur."))
  (top~replace-free-variables-and-rename!! object nil nil :return-renaming return-renaming
					    :variables variables :not-variables not-variables))


(defun top~replace-free-variables-and-rename (object domain codomain &key (return-renaming t) (variables t) not-variables)
  ;;;??????????? stimmt der Kommentar???????
  (declare (edited  "13-DEC-1991 13:03")
	   (authors RICHTS)
	   (input   "A structure containing terms, a flag, a list of variables or t, another list of variables"
		    "and two lists of variables of equal length.")
	   (effect  "None.")
	   (value   "A free variable of OBJECT is renamed if it is in VARIABLES or VARIABLES = t and if it is not"
		    "in NOT-VARIABLES. It is renamed by a new variable if it does not occur in OLD-VARIABLES"
		    "and it is renamed by the corresponding variable in NEW-VARIABLES if it does:"
		    "  x --> x   ,if (not x in VARIABLES) or x in VARIABLES"
		    "        y   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  OLD-VARIABLES[i] = x  and  NEW-VARIABLES[i] = y"
		    "        z   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  (not x in OLD-VARIABLES)  and  z is a new variable"
		    "If RETURN-RENAMING = T, three values are returned:"
		    "1. A new structure with new, unshared and renamed terms."
		    "2. The variables which have been renamed (including OLD-VARIABLES)."
		    "3. The corresponding list of new variables (including NEW-VARIABLES)."
		    "If RETURN-RENAMING = NIL only the renamed structure with unshared term is returned."))
  (unwind-protect
       (progn
	 (top=label-symbols not-variables not-variables)
	 (let ((renamed-codomain (if variables
				     (top=replace-free-variables-and-rename codomain variables)
				     codomain)))
	   (multiple-value-bind (old-variables1 new-variables1)
	       (if return-renaming
		   (top=return-renaming domain))
	     (top=label-symbols domain renamed-codomain)
	     (let ((result (top=replace-free-variables-and-rename object variables)))
	       (top=unlabel-terms domain)
	       (if return-renaming
		   (multiple-value-bind (old-variables2 new-variables2)
		       (top=return-renaming (top=label-list))
		     (values result (nconc old-variables1 old-variables2) (nconc new-variables1 new-variables2)))
		   result)))))
    (top=unlabel-label-list)))

(defgeneric top=replace-free-variables-and-rename (object variables)
  (declare (edited  "17-DEC-1991 13:16")
	   (authors RICHTS)
	   (input   "An object where some variables are labeled with terms and a list of variables or T.")
	   (effect  "Unlabeled variables which are not member of VARIBALES are labeled with a new variable.")
	   (value   "A new object with new, unshared terms where the labeled variables are replaced by their label."))
  (:method ((object-list list) variables)
   (mapcar #'(lambda (object)
	       (top=replace-free-variables-and-rename object variables))
	   object-list))
  (:method ((variable sym+var) variables)
   (let ((label (sym~label variable)))
     (if label
	 (term~copy label)
	 (if (or (eq variables t) (member variable variables :test #'keim~equal))
	     (top=label-symbol variable (sym~variable-create variable (term~type variable)))
	     (term~copy variable)))))
  (:method ((constant sym+const) variables)
   (declare (ignore variables))
   (term~copy constant))
  (:method ((application appl+appl) variables)
   (appl~create (top=replace-free-variables-and-rename (appl~function application) variables)
		     (mapcar #'(lambda (argument)
				 (top=replace-free-variables-and-rename argument variables))
			     (appl~arguments application))))
  (:method ((abstraction abstr+abstr) variables)
   (let* ((bound-variable (abstr~bound-variable abstraction))
	  (old-label (sym~label bound-variable)))
     (unwind-protect
	 (progn (sym~set-label! bound-variable bound-variable)
		(abstr~create (term~copy bound-variable)
				   (top=replace-free-variables-and-rename (abstr~scope abstraction) variables)))
       (sym~set-label! bound-variable old-label)))))


(defun top~replace-free-variables-and-rename! (object domain codomain &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "13-DEC-1991 13:04")
	   (authors RICHTS)
	   (input   "A structure containing terms, a flag, a list of variables or t, another list of variables"
		    "and two lists of variables of equal length.")
	   (effect  "None.")
	   (value   "A free variable of OBJECT is renamed if it is in VARIABLES or VARIABLES = t and if it is not"
		    "in NOT-VARIABLES. It is renamed by a new variable if it does not occur in OLD-VARIABLES"
		    "and it is renamed by the corresponding variable in NEW-VARIABLES if it does:"
		    "  x --> x   ,if (not x in VARIABLES) or x in VARIABLES"
		    "        y   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  OLD-VARIABLES[i] = x  and  NEW-VARIABLES[i] = y"
		    "        z   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  (not x in OLD-VARIABLES)  and  z is a new variable"
		    "If RETURN-RENAMING = T, three values are returned:"
		    "1. A new structure with new, unshared and renamed terms."
		    "2. The variables which have been renamed (including OLD-VARIABLES)."
		    "3. The corresponding list of new variables (including NEW-VARIABLES)."
		    "If RETURN-RENAMING = NIL only the renamed structure with unshared term is returned."))
  (unwind-protect
      (progn
	(top=label-symbols not-variables not-variables)
	(let ((renamed-codomain (if variables
				    (top=replace-free-variables-and-rename! codomain variables)
				    codomain)))
	  (multiple-value-bind (old-variables1 new-variables1)
	      (if return-renaming
		  (top=return-renaming domain))
	    (top=label-symbols domain renamed-codomain)
	    (let ((result (top=replace-free-variables-and-rename! object variables)))
	      (top=unlabel-terms domain)
	      (if return-renaming
		  (multiple-value-bind (old-variables2 new-variables2)
		      (top=return-renaming (top=label-list))
		    (values result (nconc old-variables1 old-variables2) (nconc new-variables1 new-variables2)))
		  result)))))
    (top=unlabel-label-list)))

(defgeneric top=replace-free-variables-and-rename! (object variables)
  (declare (edited  "17-DEC-1991 13:16")
	   (authors RICHTS)
	   (input   "An object where some variables are labeled with terms and a list of variables or T.")
	   (effect  "Objects which are not terms are replaced destructively."
		    "Unlabeled variables which are not member of VARIBALES are labeled with a new variable.")
	   (value   "A new term if OBJECT is a term or the changed OBJECT else."
		    "Both with new, unshared terms where the labeled variables are replaced by their label."))
  (:method ((object-list list) variables)
   (mapl #'(lambda (list-tail)
	     (setf (car list-tail) (top=replace-free-variables-and-rename! (car list-tail) variables)))
	 object-list)
   object-list)
  (:method ((term term+term) variables)
   (top=replace-free-variables-and-rename term variables)))


(defun top~replace-free-variables-and-rename!! (object domain codomain &key (return-renaming t) (variables t) not-variables)
  (declare (edited  "13-DEC-1991 13:04")
	   (authors RICHTS)
	   (input   "A structure containing terms, a flag, a list of variables or t, another list of variables"
		    "and two lists of variables of equal length.")
	   (effect  "None.")
	   (value   "A free variable of OBJECT is renamed if it is in VARIABLES or VARIABLES = t and if it is not"
		    "in NOT-VARIABLES. It is renamed by a new variable if it does not occur in OLD-VARIABLES"
		    "and it is renamed by the corresponding variable in NEW-VARIABLES if it does:"
		    "  x --> x   ,if (not x in VARIABLES) or x in VARIABLES"
		    "        y   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  OLD-VARIABLES[i] = x  and  NEW-VARIABLES[i] = y"
		    "        z   ,if (VARIABLES = T  or  x in VARIABLES)  and  (not x in NOT-VARIABLES)"
		    "                and  (not x in OLD-VARIABLES)  and  z is a new variable"
		    "If RETURN-RENAMING = T, three values are returned:"
		    "1. A new structure with new, unshared and renamed terms."
		    "2. The variables which have been renamed (including OLD-VARIABLES)."
		    "3. The corresponding list of new variables (including NEW-VARIABLES)."
		    "If RETURN-RENAMING = NIL only the renamed structure with unshared term is returned."))
  (unwind-protect
      (progn
	(top=label-symbols not-variables (make-list (length not-variables) :initial-element t))
	(let ((renamed-codomain (if variables
				    (top=replace-free-variables-and-rename!! codomain variables)
				    codomain)))
	  (multiple-value-bind (old-variables1 new-variables1)
	      (if return-renaming
		  (top=return-renaming domain))
	    (top=label-symbols domain renamed-codomain)
	    (top=replace-free-variables-and-rename!! object variables)
	    (top=unlabel-terms domain)
	    (if return-renaming
		(multiple-value-bind (old-variables2 new-variables2)
		    (top=return-renaming (top=label-list))
		  (values object (nconc old-variables1 old-variables2) (nconc new-variables1 new-variables2)))
		object))))
    (top=unlabel-label-list)))

(defgeneric top=replace-free-variables-and-rename!! (object variables)
  (declare (edited  "17-DEC-1991 13:16")
	   (authors RICHTS)
	   (input   "An object where some variables are labeled with terms and a list of variables or T.")
	   (effect  "Unlabeled variables which are not member of VARIBALES are labeled with a new variable."
		    "Free variables are replaced destructively with their (possibly new) label.")
	   (value   "The changed OBJECT."))
  (:method ((object-list list) variables)
   (mapl #'(lambda (list-tail)
	     (setf (car list-tail) (top=replace-free-variables-and-rename!! (car list-tail) variables)))
	 object-list)
   object-list)
  (:method ((variable sym+var) variables)
   (let ((label (sym~label variable)))
     (if label 
	 (unless (eq label t)
	   (term~set-term variable label))
	 (when (or (eq variables t) (member variable variables :test #'keim~equal))
	   (let ((new-variable (sym~variable-create variable (term~type variable))))
	     (top=label-symbol (term~copy variable) new-variable)
	     (term~set-term variable new-variable))))
     variable))
  (:method ((constant sym+const) variables)
   (declare (ignore variables))
   constant)
  (:method ((application appl+appl) variables)
   (mapc #'(lambda (argument)
	     (top=replace-free-variables-and-rename!! argument variables))
	 (term~subterms application))
   application)
  (:method ((abstraction abstr+abstr) variables)
   (let* ((bound-variable (abstr~bound-variable abstraction))
	  (old-label (sym~label bound-variable)))
     (sym~set-label! bound-variable t)
     (top=replace-free-variables-and-rename!! (abstr~scope abstraction) variables)
     (sym~set-label! bound-variable old-label)
     abstraction)))

(defun top=return-renaming (variable-list)
  (if variable-list
      (multiple-value-bind (old-variables new-variables)
	  (top=return-renaming (cdr variable-list))
	(let* ((variable (car variable-list))
	       (label (sym~label variable)))
	  (if (or (null label) (keim~equal variable label))
	      (values old-variables new-variables)
	      (values (cons variable old-variables) (cons label new-variables)))))
      (values nil nil)))


;; Test-Functions

(defun top==unshared-p (object)
  (declare (edited  "29-OCT-1991 13:29")
	   (authors RICHTS)
	   (input   "A structure containing terms.")
	   (effect  "None.")
	   (value   "True, iff the terms in OBJECT are unshared, i.e. each term-box appears only once."))
  (unwind-protect
      (top=unshared-p object)
    (top=unlabel-label-list)))

(defgeneric top=unshared-p (object)
  (:method ((termlist list))
	   (every #'top=unshared-p termlist))
  (:method ((variable sym+var))
	   (unless (term~label variable)
	     (top=label-term variable t)))
  (:method ((constant sym+const))
	   (unless (term~label constant)
	     (top=label-term constant t)))
  (:method ((application appl+appl))
	   (unless (term~label application)
	     (top=label-term application t)
	     (every #'top=unshared-p (term~subterms application))))
  (:method ((abstraction abstr+abstr))
	   (unless (term~label abstraction)
	     (top=label-term abstraction t)
	     (and (top=unshared-p (abstr~bound-variable abstraction))
		  (top=unshared-p (abstr~scope abstraction)))))) 
 

#{\subsection{Term bindings}\label{bindings}
For the manipulation of terms it is possible to use so called bindings,
which can be considered as a set of labels. These bindings can be set by
{\vb TOP~BIND-TERM}, withdrawn by {\vb TOP~UNBIND-TERMS}. Since several
binding lists can be manipulated recursively (using {\vb TOP~POP-BINDINGS}
and {\vb TOP~PUSH-BINDINGS}), users of these facilities should be very careful
to unbind the terms properly before leaving a procedure, since otherwise other
usages of term bindings might be disturbed.
#}


(defvar top*binding-lists (list nil))
;;variable for collecting lists of all bindings associated with a term

(defvar top*binding-stack nil)
;;???????Kommentar

(defun top~bind-term! (term binding)
  (declare (edited  "05-NOV-1991 10:56")
	   (authors RICHTS)
	   (input   "Two terms of equal type.")
	   (effect  "The binding cell of {\\vb TERM} is set to {\\vb BINDING}.")
	   (value   "Undefined.")
	   (example "X   (F C) --> binding (F C) is attached at X"))
  (push term (car top*binding-lists))
  (term~set-binding! term binding))

(defun top~bind-terms! (terms bindings)
  (declare (edited  "05-NOV-1991 10:56")
	   (authors RICHTS)
	   (input   "Two lists of terms of equal length and equal types.")
	   (effect  "The binding cells of {\\vb TERMS} are set to {BINDINGS}.")
	   (value   "Undefined.")
	   (example "(LIST X Y)   (LIST A B) --> the binding A is attached at X and B at Y."))
  (setf (car top*binding-lists) (append terms (car top*binding-lists)))
  (mapc #'(lambda (term binding)
	    (term~set-binding! term binding))
	terms bindings))

(defun top~unbind-terms! (terms)
  (declare (edited  "05-NOV-1991 10:56")
	   (authors RICHTS)
	   (input   "A list of terms.")
	   (effect  "The binding cells of {\\vb TERMS} are set to NIL.")
	   (value   "Undefined."))
  (mapc #'(lambda (term)
	    (term~set-binding! term nil))
	terms))


(defun top~clear-bindings! (term)
  (declare (edited  "19-SEP-1991 15:22")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "The binding cells of all subterms of {\\vb TERM} are set to NIL.")
	   (value   "Undefined."))
  (if (listp term)
      (mapc #'(lambda (subterm) (top~clear-bindings! subterm)) term)
      (progn (term~set-binding! term nil)
	     (mapc #'(lambda (subterm) (top~clear-bindings! subterm)) (term~subterms term)))))

(defmacro top~let-terms-bindings! (terms bindings &body body)
  ;;??? WAS TUT DAS z.b. (top~let-terms-bindings! (list x y) (list a c) (term~type x)) -->
  ;;Error: Attempt to take the value of the unbound variable `EVALED-TERMS'.
  (declare (edited  "15-AUG-1991 18:06")
	   (authors RICHTS)
	   (input   "Two list of terms of equal length and a body of LISP forms.")
	   (effect  "The body is evaluated with {\\vb BINDINGS} inserted in the binding-slots of TERMS.")
	   (value   "The value of the last form of {\\vb BODY.}"))
  `(let ((evaled-terms ,terms))
    (prog2 (top~bind-terms! evaled-terms ,bindings)
	(progn ,@body)
      (top~unbind-terms! evaled-terms))))


(defun top~new-binding-list! ()
;;?? PROBLEM: Was passiert hier und wie kann das dokumentiert werden?????????
  (declare (edited  "19-SEP-1991 15:25")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "A new and empty list is created where all terms the binding cells of"
		    "which are set with top~bind-terms! are gathered.")
	   (value   "Undefined."))
  (push nil top*binding-lists))

(defun top~unbind-binding-list! ()
    (declare (edited  "19-SEP-1991 15:28")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "The binding cells of all terms in the current binding-list are set to NIL and the binding-list"
		    "is destroyed. The current binding-list becomes the previous one or a new one if there isn't one.")
	   (value   "Undefined."))
  (top~unbind-terms! (pop top*binding-lists))
  (unless top*binding-lists (push nil top*binding-lists)))

(defun top~binding-list! ()
  (declare
   (authors RICHTS NESMITH)
   (input "none")
   (value "returns the first of current binding lists"))
  (car top*binding-lists))


(defun top~push-bindings! (terms bindings)
  (declare (edited  "19-SEP-1991 15:44")
	   (authors RICHTS)
	   (input   "Two list of terms of equal length.")
	   (effect  "The binding cells of TERMS are set to BINDINGS and TERMS is pushed on the binding-stack.")
	   (value   "Undefined."))
  (push terms top*binding-stack)
  (mapc #'(lambda (term binding)
	    (term~set-binding! term binding))
	terms bindings))

(defun top~pop-bindings! ()
  (declare (edited  "19-SEP-1991 15:44")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "The binding-stack is popped and the binding cells of the terms on the top are set to NIL.")
	   (value   "Undefined."))
  (top~unbind-terms! (pop top*binding-stack)))

(defun top~unbind-stack! ()
  (declare (edited  "19-SEP-1991 15:44")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "The binding-stack is emptied and the binding cells of all terms in it are set to NIL.")
	   (value   "Undefined."))
  (mapc #'(lambda (terms) (top~unbind-terms! terms))
	top*binding-stack)
  (setf top*binding-stack nil))


(defun top~insert-bindings! (term)
  (declare (edited  "05-NOV-1991 10:55")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "A new term where the bindings of all subterms of TERM are inserted into the term recursively."))
  (cond ((listp term) (mapcar #'top~insert-bindings! term))
	((term~binding term) (top~insert-bindings! (term~binding term)))
	((sym~p term) (term~copy term))
	((appl~p term) (appl~create (top~insert-bindings! (appl~function term))
					      (top~insert-bindings! (appl~arguments term))))
	((abstr~p term) (abstr~create (top~insert-bindings! (abstr~bound-variable term))
						(top~insert-bindings! (abstr~scope term))))))


(defun top~insert-bindings!! (term)
  (declare (edited  "21-SEP-1991 15:40")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "The bindings of all subterms of TERM are inserted into TERM destructively."
		    "In a chain of bindings only the first term is replaced by the last one.")
	   (value   "The changed TERM."))
  (cond ((listp term) (mapc #'top~insert-bindings! term))
	((term~binding term)
	 (term~set-term term (term~binding term))
	 (top~insert-bindings! term))
	(t (top~insert-bindings! (term~subterms term))))
  term)

(defun top~swap-bindings (term)
  (declare (edited  "19-SEP-1991 15:32")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "None.")
	   (value   "A new term where the bindings of all subterms of TERM are inserted into the term recursively and"
		    "where the binding cells of the inserted bindings contain the terms they were bound to."))
  (declare (ignore term)))

(defun top~swap-bindings! (term)
  (declare (edited  "19-SEP-1991 15:32")
	   (authors RICHTS)
	   (input   "A term or termlist.")
	   (effect  "All subterms of TERM are replaced by the term in their binding cells if there is one"
		    "and all terms in the binding cells are replaced by the terms they are bound to."
		    "In a chain of bindings only the first term is swapped with the last one and the binding cell"
		    "of the changed first term contains the changed last term.")
	   (value   "The changed TERM."))
  (declare (ignore term)))


 
(defgeneric top~all-unbound-symbols (term)
  (declare (edited  "17-Feb-1993 15:32")
	   (authors nesmith)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "A list of all symbols that are not lambda-bound in 
this term."))
  (:method ((term sym+sym))
      (list term))
  (:method ((term appl+appl))
      (delete-duplicates
       (nconc
	(top~all-unbound-symbols (appl~function term))
	(mapcan #'top~all-unbound-symbols
		(appl~arguments term)))
       :test #'term~equal))
  (:method ((term abstr+abstr))
      (delete (abstr~bound-variable term)
       (top~all-unbound-symbols (abstr~scope term))
       :test #'term~equal)))





