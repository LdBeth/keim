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

(in-package :keim)
(mod~defmod appl :uses (env keim mod post term type )
	    :documentation 
	    "Applications: applying a function to arguments"
	    :exports (
		      appl+appl
		      appl~function
		      appl~set-function!
		      appl~arguments
		      appl~set-arguments!
		      appl~create
		      appl~p
		      appl~poly-create
		      appl~env-lookup
		      )
	    )

#{\section{Applications} \label{mod:appl}
This model handels the terms which are applications.
#}

;; renaming term+appl -> appl+appl

(eval-when (load compile eval)
(defclass appl+appl (term+term)
  ((function :initarg :function :reader appl=function 
	     :writer appl=write-function!) ;doc
   (arguments :initarg :arguments :reader appl=arguments 
	      :writer appl=write-arguments!)) ;doc
  (:documentation "The class of all applications that are simple terms.
Applications are terms of the form $F(T_1 T_2 ... T_n$), such that the 
types of the arguments $T_i$ are equal to the argument types of $F$.")))

(term~warning-rename-fn term~application-function appl~function)


(term~defgeneric appl~function ((term)) 
		 (declare (edited  "16-JUN-1992 16:40" )
			  (authors KOHLHASE )
			  (input   "A term." )
			  (effect  "None." )
			  (value   "If TERM is an application of the form (f t1,..., tn), then the function f, else error.")
			  (example "(QQ X X) --> QQ"))
		 (:method ((term appl+appl))
			  (appl=function term)))

(term~warning-rename-fn term~set-application-function! appl~set-function!)

(term~defgeneric appl~set-function! ((term) function) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "Two terms." )
	   (effect  "None." )
	   (value   "If TERM is an application of the form (f t1,..., tn), then FUNCTION is substituted for f, else error.")
	   (example "Let PP be a binary function, (QQ X X) PP --> (PP X X)"))
  (:method ((term appl+appl) (function term+term))
	   (let ((func (appl=function term)))
	     (if (keim~equal (term~type func) (term~type function))
		 (appl=write-function! function term)
		 (error "~A has a different type than the ~
application function of ~A" function term)))))

(term~warning-rename-fn term~application-arguments appl~arguments)

(term~defgeneric appl~arguments ((term)) 
  (declare (edited  "16-JUN-1992 16:40" )
	   (authors KOHLHASE )
	   (input   "A term." )
	   (effect  "None." )
	   (value   "If TERM is an application of the form (f t1,..., tn), then the list of terms (t1,...,tn), else error.")
	   (example "(QQ X X) --> (X X)"))
  (:method ((term appl+appl))
	   (appl=arguments term)))

(term~warning-rename-fn term~set-application-arguments! appl~set-arguments!)

(term~defgeneric appl~set-arguments! ((term) arguments) 
		 (declare (edited  "16-JUN-1992 16:40" )
			  (authors huang KOHLHASE )
			  (input   "A term and a list of terms." )
			  (effect  "If TERM is an application of the form (f t1,..., tn) and"
				   "ARGUMENTS is a list (s1,...,sn) of terms of the same types as ti,"
				   "then si are substituted for ti in TERM." )
			  (value   "the changed term"))
		 (:method ((term appl+appl) arguments)
			  (error "~A is not a list of argumnents" arguments))
		 (:method ((term appl+appl) (arguments list))
			  (let ((args (appl=arguments term)))
			    (cond ((> (length arguments) (length args)) 
				   (error "~A contains more terms than ~A has arguments." arguments term))
				  ((> (length args) (length arguments)) 
				   (error "~A needs more arguments than ~A contains." term arguments))
				  ((not (every #'(lambda (alt neu) 
						   (keim~equal (term~type alt) 
							       (term~type neu))) 
					       args arguments))
				   (error "the types of the arguments of ~A~
 and the list ~A of new arguments do not match" term arguments))	 
				  (t (progn (appl=write-arguments! arguments term)
					    term))))))


(defmethod term~copy ((application appl+appl))
   (appl~create 
    (term~copy (appl~function application))
    (mapcar #'term~copy (appl~arguments application))))

(defmethod term~set-term ((term1 term+term) (application appl+appl))
  (declare (edited  "12-SEP-1991 15:03")
	   (authors RICHTS)
	   (input   "Two terms.")
	   (effect  "All slots of TERM1 are set to those of TERM2 (the property list is copied).")
	   (value   "The changed TERM1."))
  (change-class term1 'appl+appl)
  (appl=write-function! (appl~function application) term1)
  (appl=write-arguments! (appl~arguments application) term1)
  term1)


(defmethod term~top ((application appl+appl))
  (term~top (appl~function application)))


(defmethod term~subterms ((application appl+appl))
   (cons (appl~function application) 
	 (appl~arguments application)))

(term~warning-rename-fn term~application-create  appl~create)

(term~defgeneric appl~create ((function) arguments &key)
  (declare (edited  "15-AUG-1991 13:41")
	   (authors RICHTS)
	   (input   "A term with arity n and a list of terms 
with length <= n.")
	   (effect  "None.")
	   (value   "The term (function . arguments) if arguments is not nil,"
		    "function else.")
	   (example "Let QQ be a predicate of the type (i,i)->o"
		    "QQ (X) --> (QQ X)""(QQ X) (y) -->(QQ X Y)"))
  (:method ((function term+term) arguments &key)
	   (if arguments
	       (make-instance 
		'appl+appl
		:function function
		:arguments arguments
		:type (type~apply (term~type function) 
				  (mapcar #'term~type arguments)))
	       function))
  (:method ((function appl+appl) arguments &key)
	   (if arguments
	       (make-instance 
		'appl+appl
		:function (appl=function function)
		:arguments (append (appl~arguments function) arguments)
		:type 
		(type~apply (term~type function) 
			    (mapcar #'term~type arguments)))
	     function)))

(term~warning-rename-fn term~application-p appl~p)

(defun appl~p (object)
  (declare (edited  "12-SEP-1991 14:25")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff OBJECT is an application."))
  (typep object 'appl+appl))

(term~warning-rename-fn term~poly-application-create appl~poly-create)

(defun appl~poly-create (function arguments)
  (declare (edited  "31-AUG-1992 13:24")
	   (authors RICHTS)
	   (input   "A term and a list of terms.")
	   (effect  "The types of the ARGUMENTS are unified with the 
domain types of FUNCTION.  The resulting type substitution is applied 
destructively to the terms. If a new instance of a constant with 
polymorphic type is needed, it is added to the environment stored in 
the property list of the shared constant symbol.")
	   (value   "The term (FUNCTION ..ARGUMENTS) where the types 
are unified.")
	   (example "Let QQ be a binary function, QQ (X Y) --> (Q X Y)"))
  (when (< (length (type~n-domain (term~type function)))
	   (length arguments))
    (error "KEIM: Too many arguments for an application: the type of ~S ~
            is ~S and therefore cannot be applied to arguments ~S" 
	   function (term~type function) arguments))
  (let ((sigma (type~unify (subseq (type~n-domain (term~type function))
				   0 (length arguments))
			   (mapcar #'term~type arguments))))
    (unless sigma
      (error "KEIM: The symbol ~A with type ~A does not apply to the ~
              arguments ~A with types ~A because the types cannot be unified."
	      function (term~type function) arguments 
	      (mapcar #'term~type arguments)))
    (appl~create (type~subst-apply sigma function)
		 (type~subst-apply sigma arguments))))

(defmethod print-object ((appl appl+appl) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   )
	   (special *print-length*))
  (cond	((or (null *print-length*) 
	     (> *print-length* (length (appl~arguments appl))))
	 (format stream "(~A~{ ~A~})" (appl~function appl) 
		 (appl~arguments appl)))
	(t (format stream "(~A~{ ~A~} ...)"
		   (appl~function appl)
		   (subseq (appl~arguments appl) 0 (1- *print-length*))))))

(defmethod term~equal ((application1 appl+appl) (application2 appl+appl))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two applications.")
	   (effect  "None.")
	   (value   "True iff the applications' heads and arguments are pairwise term~equal."))
  (or (eq application1 application2)
      (and (= (length (appl~arguments application1)) 
	      (length (appl~arguments application2)))
	   (if (every #'term~equal 
		  (term~subterms application1) 
		  (term~subterms application2))
	       t
	     nil))))

(defmethod term~equal-p ((application1 appl+appl) (application2 appl+appl))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two applications.")
	   (effect  "None.")
	   (value   "True iff the applications' heads and arguments are pairwise term~equal-p."))
  (or (eq application1 application2)
      (and (= (length (appl~arguments application1)) 
	      (length (appl~arguments application2)))
	   (if (every #'term~equal-p 
		  (term~subterms application1) 
		  (term~subterms application2))
	       t
	     nil))))

(defmethod term~=equal-p-ab-aux ((application1 appl+appl)
				 (application2 appl+appl)
				 (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two applications and an alist of symbols.")
	   (effect  "None.")
	   (value   "True iff the applications' heads and arguments are
pairwise term~equal-p-ab-aux."))
  (or (eq application1 application2)
      (and (= (length (appl~arguments application1)) 
	      (length (appl=arguments application2)))
	   (if (every #'(lambda (x y) (term~=equal-p-ab-aux x y varalist))
		  (term~subterms application1) 
		  (term~subterms application2))
	       t
	     nil))))

(defmethod term~=equal-ab-aux ((application1 appl+appl) 
			       (application2 appl+appl) (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two applications and an alist of symbols.")
	   (effect  "None.")
	   (value   "True iff the applications' heads and arguments are pairwise term~equal-ab-aux."))
  (or (eq application1 application2)
      (and (= (length (appl~arguments application1)) 
	      (length (appl~arguments application2)))
	   (if (every #'(lambda (x y) (term~=equal-ab-aux x y varalist))
		  (term~subterms application1) 
		  (term~subterms application2))
	       t
	     nil))))


(defmethod type~subst-apply ((subst type+substitution) (term appl+appl))
  (if (or (type~subst-empty-p subst) (type~ground-p (term~type term)))
      term
      (appl~create (type~subst-apply subst (appl~function term))
			       (mapcar #'(lambda (argument)
					   (type~subst-apply subst argument))
				       (appl~arguments term)))))

(term~warning-rename-fn term~env-lookup-application appl~env-lookup)

(defun appl~env-lookup (term env)
  (declare
   (authors nesmith)
   (input "A list representing a term and an environment.")
   (effect "none")
   (value "Attempts to parse the list in the environment as an application."
	   "If it maps to a term, the term is returned, otherwise an error is signaled.")
   (example "((qq x x)) env --> (QQ X X)"))
  (appl~poly-create 
   (term~env-lookup (car term) env)
   (mapcar #'(lambda (x) (term~env-lookup x env)) (cdr term))))

(defmethod post~print ((application appl+appl) stream)
  (declare (edited  "27-JAN-1993 17:37")
	   (authors RICHTS)
	   (input   "A application, a stream and a list of 
                     keyword parameters.")
	   (effect  "(<function> <argument>*)")
	   (value   "Like format."))
  (format stream "(")
  (post~print (term~subterms application) stream)
  (format stream ")"))



(defmethod env~post-print (key (ap appl+appl) stream)
  (declare (ignore key stream))
  (values))

(defmethod post~read-object ((term list) (env env+environment)
			     (indicator (eql :existing-term)))
  (appl~env-lookup term env))


(defmethod term~poly-subterm-find ((term appl+appl))
  (declare (authors nesmith)
	   (input "A term")
	   (effect "none")
	   (value "NIL if all symbols in the term have ground type, 
otherwise the first subterm that has polymorphic type."))
  (if (type~ground-p (term~type term))
      (or (term~poly-subterm-find (appl~function term))
	  (some #'term~poly-subterm-find
		(appl~arguments term)))
    term))

