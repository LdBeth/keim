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


(mod~defmod sym :uses (env keim mod post shsym term type )
	    :documentation "Term symbols"
	    :exports (
		      sym+sym
		      sym+var
		      sym+const
		      sym~symbol-binding
		      sym~set-symbol-binding!
		      sym~shared-symbol
		      sym~set-shared-symbol!
		      sym~p
		      sym~name
		      sym~plist
		      sym~label
		      sym~set-label!
		      sym~variable-p
		      sym~variable-list-p
		      sym~constant-p
		      sym~read-variable
		      sym~env-enter-variable
		      sym~env-enter-constant
		      sym~constant-create
		      sym~variable-create
		      sym~rename-var
		      )
	    )


#{
\section{Term symbols}\label{mod:sym}
Symbols are instances of {\vb SYM+SYM}.  These always have 
 a slot which contain an instance of {\vb SHSYM+SHAREDSYMBOL}.
#}


; renamings term+sym -> sym+sym
; renamings term+var -> sym+var
; renamings term+const -> sym+const

(eval-when (load compile eval)
(defclass sym+sym (term+term)
  ((symbol :initarg :symbol :reader sym=symbol :writer sym=write-symbol!
	   :documentation "A shared symbol."))
  (:documentation "The class of logical symbols."))

(defclass sym+var (sym+sym)
  ()
  (:documentation "The class of all logical variables"))

(defclass sym+const (sym+sym)
  ()
  (:documentation "The class of all logical constants"))
)

(term~warning-rename-fn term=symbol sym=symbol)
(term~warning-rename-fn term=write-symbol! sym=write-symbol!)

(defmethod keim~copy ((sym sym+sym))
  (term~copy sym))

(defmethod term~set-term ((term1 term+term) (sym sym+sym))
  (change-class term1 (class-of sym))
  (sym=write-symbol! (sym=symbol sym) term1)
  term1)


(defmethod term~top ((sym sym+sym))
  sym)


(defmethod term~subterms ((term sym+sym))
   nil)

(term~warning-rename-fn term~symbol-binding sym~symbol-binding)

(term~defgeneric sym~symbol-binding ((term))
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A logical symbol, e.g., a variable or constant.")
	   (effect  "None.")
	   (value   "The binding of the shared symbol contained in {\vb TERM}.")
	   (example "Let x be defined as a variable and {\vb TERM} as a term"
		    "(sym~set-symbol-binding! x TERM))"
		    "x --> TERM"))
  (:method ((term term+term))
	   (error "~A is not a logical symbol." term))
  (:method ((sym sym+sym))
	   (shsym~binding (sym=symbol sym))))


(term~warning-rename-fn term~set-symbol-binding! sym~set-symbol-binding!)

(term~defgeneric sym~set-symbol-binding! ((term) binding)
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A term {\vb TERM} and a logical symbol {\vb BINDING}.")
	   (effect  "None.")
	   (value   "The binding of the shared symbol contained in {\vb TERM} is set to {\vb BINDING}."))
  (:method ((term term+term) binding)
	   (declare (ignore binding))
	   (error "~A is not a logical symbol." term))
  (:method ((sym sym+sym) binding)	   
	   (shsym~set-binding! (sym=symbol sym) binding)))


(defmethod term~set-binding! ((term sym+sym) new-binding)
  (sym~set-symbol-binding! term new-binding))


(term~warning-rename-fn term~shared-symbol sym~shared-symbol)

(term~defgeneric sym~shared-symbol ((term))
  (declare (edited  "24-JUN-1992 16:47" )
	   (authors KOHLHASE )
	   (input   "A logical symbol.")
	   (effect  "None.")
	   (value   "The sharedsymbol in TERM.")
	   (example "term X --> sharedsymbol X"))
  (:method ((term sym+sym))
	   (sym=symbol term)))

(term~warning-rename-fn term~set-shared-symbol! sym~set-shared-symbol!)

(term~defgeneric sym~set-shared-symbol! ((term) symbol)
  (declare (edited  "24-JUN-1992 16:47" )
	   (authors KOHLHASE )
	   (input   "A variable or constant, and a sharedsymbol.")
	   (effect  "If the types are equal, 
the sharedsymbol in TERM is replaced by SYMBOL.")
	   (value   "The sharedsymbol in TERM.")
	   (example "A   (sym~shared-symbol X) --> X"
		    "e.g. A is now printed as X"))
  (:method ((term sym+sym) (symbol shsym+sharedsymbol))
	   (if (keim~equal (term~type term) (shsym~type symbol))
	       (sym=write-symbol! symbol term)
	       (error "The types of the term symbol ~A and the shared symbol ~A do not match" term symbol))))


(term~warning-rename-fn term~symbol-p sym~p)

(defun sym~p (term)
  (declare (edited  "12-SEP-1991 13:48")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff {\vb TERM} is a logical symbol")
	   (example "X --> T"
		    "C --> T"
		    "(P x) --> NIL"))
  (typep term 'sym+sym))

(term~warning-rename-fn term~symbol-name sym~name)

(term~defgeneric sym~name ((term))
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A term symbol, e.g., a variable or constant.")
	   (effect  "None.")
	   (value   "The name of the symbol contained in TERM: a string.")
	   (example " X --> \"X\""))
  (:method ((sym sym+sym))
	   (keim~name (sym=symbol sym))))

(eval-when (load eval compile)
(term~warning-rename-fn term~symbol-plist sym~plist)

(defmacro sym~plist (symbol)
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A term symbol")
	   (effect  "None.")
	   (value   "The property list of the shared symbol contained in SYMBOL."
		    "This is the shared plist of all occurrences of this shared symbol."))
  `(keim~plist ,symbol))
)

(term~warning-rename-fn term~symbol-label sym~label)

(term~defgeneric sym~label ((term))
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A logical symbol, e.g., a variable or constant.")
	   (effect  "None.")
	   (value   "The label of the shared symbol of TERM.")
	   (example "(sym~set-label! X 'L)"
		    "X --> L"))
  (:method ((sym sym+sym))
	   (shsym~label (sym=symbol sym))))

(term~warning-rename-fn term~set-symbol-label! sym~set-label!)

(term~defgeneric sym~set-label! ((term) label)
  (declare (edited  "12-SEP-1991 13:50")
	   (authors RICHTS)
	   (input   "A label and a logical symbol, e.g., a variable or constant.")
	   (effect  "None.")
	   (value   "The label of the shared symbol of TERM is set to LABEL."))
  (:method ((sym sym+sym) label)
	   (shsym~set-label! (sym=symbol sym) label)))

(term~warning-rename-fn term=symbol-print sym=print)

(defun sym=print (symbol stream)
  (declare (edited  "08-AUG-1991 13:30")
	   (authors RICHTS)
	   (input   "A term symbol SYMBOL and an output stream STREAM.")
	   (effect  "SYMBOL is printed to STREAM")
	   (value   "Undefined."))
  (princ (sym~name symbol) stream))


(defmethod print-object ((sym sym+sym) stream)
  (declare (edited  "02-AUG-1991 21:44")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   ))
  (sym=print sym stream))



#{\subsection{Variables} #}


(defvar term*variable-counter 0)
;;This variable is incremented each time when a new variable is create, hence guaranteeing that all
;;variables have different names.

(term~warning-rename-fn term~variable-p sym~variable-p)

(defun sym~variable-p (object)
  (declare (edited  "12-SEP-1991 13:42")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff {\vb OBJECT} is a variable.")
	   (example "X --> T"
		    "C --> NIL"))
  (typep object 'sym+var))

(term~warning-rename-fn term~variable-list-p sym~variable-list-p)

(defun sym~variable-list-p (object)
  (declare (edited  "08-AUG-1991 13:30")
	   (authors RICHTS)
	   (input   "An object.")
	   (effect  "None.")
	   (value   "True iff {\vb OBJECT} is a non-empty list of variable.")
	   (example "(LIST X Y) --> T"))
  (and (consp object) 
       (sym~variable-p (car object)) 
       (or (null (cdr object)) 
	   (sym~variable-list-p (cdr object)))))


(defmethod term~copy ((sym sym+sym))
  (make-instance (class-of sym) :symbol (sym=symbol sym)
		 :type (shsym~type (sym=symbol sym))))

#{\subsection{Constants} #}

(defvar term*constant-counter 0)
;;This variable is incremented each time when a new constant is create, hence guaranteeing that all
;;constants have different names.

(term~warning-rename-fn term~constant-p sym~constant-p)

(defun sym~constant-p (object)
  (declare (edited  "12-SEP-1991 13:42")
	   (authors RICHTS)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "True iff TERM is a constant.")
	   (example "C --> T"
		    "X --> NIL"))
  (typep object 'sym+const))

(defmethod term~equal ((variable1 sym+var) (variable2 sym+var))
  (declare (edited  "28-OCT-1992 16:30")
	   (authors nesmith)
	   (input   "Two variables.")
	   (effect  "None.")
	   (value   "True iff the variables have the same shared symbol."))
  (or (eq variable1 variable2)
      (eq (sym=symbol variable1) (sym=symbol variable2))))

(defmethod term~equal ((term1 sym+const) (term2 sym+const))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two constants.")
	   (effect  "None.")
	   (value   "True iff the constants have the same shared symbol."))
  (or (eq term1 term2)
      (eq (sym=symbol term1) (sym=symbol term2))))

(defmethod term~equal-p ((term1 sym+sym) (term2 sym+sym))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two sym+syms.")
	   (effect  "None.")
	   (value   "True iff the terms' types are keim~equal and the
names of their shared symbols are string=.")
	   (example "X   X   --> T"
		    "X   C   --> NIL"
		    "But: (sym~set-shared-symbol! c (sym~shared-symbol x))"
		    "     X   C --> T"))
  (or (eq term1 term2)
      (and (keim~equal (term~type term1) (term~type term2))
	   (string= (keim~name (sym=symbol term1))
		    (keim~name (sym=symbol term2))))))

(defmethod term~=equal-p-ab-aux ((term1 sym+sym) (term2 sym+sym) (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two sym+syms and an alist of symbols.")
	   (effect  "None.")
	   (value   "True iff the terms are term~equal-p, or else"
		    "they are are associated in the VARALIST.")
	   (example "X  Y  (list (cons X A)) --> NIL"
		    "X  Y  (list (cons X Y)) --> T"))
  (or (term~equal-p term1 term2)
      (dolist (pair varalist nil)
	(when (term~equal (car pair) term1)
	  (return (term~equal-p (cdr pair) term2)))
	(when (eq (cdr pair) term2) (return nil)))))


(defmethod term~=equal-ab-aux ((term1 sym+sym) (term2 sym+sym) (varalist list))
  (declare (edited  "26-OCT-1991 16:30")
	   (authors NESMITH)
	   (input   "Two sym+syms and an alist of symbols.")
	   (effect  "None.")
	   (value   "True iff the terms are term~equal, or else
they are are associated in the VARALIST.")
	   (example "see {\vb TERM~=EQUAL-P-AB-AUX}"))
  (or (term~equal term1 term2)
      (dolist (pair varalist nil)
	(when (term~equal (car pair) term1)
	  (return (term~equal (cdr pair) term2)))
	(when (eq (cdr pair) term2) (return nil)))))


(defmethod type~subst-apply ((subst type+substitution) (sym sym+sym))
  sym)


#{ \subsection{Post Input} #}


(defmethod post~read-object ((vars list) (env env+environment) 
			    (indicator (eql :variables)))
  (dolist (var vars nil)
    (post~read-object var env :variable)))
	    
(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :variable)))
  (let ((type (type~env-lookup (cdr var) env)))
    (sym~env-enter-variable (car var) type env nil)))


(defmethod post~read-object ((var cons) (env env+environment) 
			    (indicator (eql :variable-multiple)))
  (let ((type (type~env-lookup (cdr var) env)))
    (sym~env-enter-variable (car var) type env t)))


(defmethod post~read-object ((consts list) (env env+environment) 
			    (indicator (eql :constants)))
  (dolist (const consts nil)
    (post~read-object const env :constant)))
	    
(defmethod post~read-object ((const cons) (env env+environment) 
			    (indicator (eql :constant)))
  (let ((type (type~env-lookup (cdr const) env)))
    (sym~env-enter-constant (car const) type env)))

(term~warning-rename-fn term~read-variable sym~read-variable)

(defun sym~read-variable (term env)
  (declare 
   (authors nesmith)
   (effect "")
   (input "A POST representation of a variable, and an environment.")
   (value "If a variable matching the input is found in the environment,"
	  "it is returned, otherwise an error is signaled.")
   (example "Let X be a variable in the environment ENV"
	    "'X ENV  --> X"))
  (let ((obj
	 (or (and (symbolp term) (env~p env)
		  (sym=env-lookup-symbol term env))
	     (and (consp term) (null (cdr term)) (symbolp (car term))
		  (env~p env) (sym=env-lookup-symbol (car term) env)))))
    (if (sym~variable-p obj)
	obj
      (post~error "Term read: ~S is not a variable as expected." obj))))

(term~warning-rename-fn term~env-enter-variable sym~env-enter-variable)

(defun sym~env-enter-variable (symbol type env &optional allow-multiple)
  (declare
   (authors nesmith)
   (input  "A lisp symbol, a type+type and an environment.  Optional argument"
	   "is boolean ALLOW-MULTIPLE.")
   (effect "If the symbol has no association in environment, then a new"
	   "term variable with the symbol as name and given type is created and entered "
	   "in the environment.  If the symbol is associated with a non-variable or with"
	   "a variable of a differing type, an error is signaled.  If the symbol is"
	   "already associated with a variable of the proper type, "
	   "then depending on whether ALLOW-MULTIPLE is non-NIL, either a new variable "
	   "is made and entered in the environment or the existing variable is returned.")
   (value "The variable, if one is successfully made or found.")
   (example "Let ENV be an environment NOT containing Z"
	    "'Z  (I I I) ENV  --> Z   and Z is inserted in ENV"
	    "'Z  (I I I) ENV  T --> Z   and Z is inserted in ENV a second time"
	    "'Z  (I I I) ENV  --> Z   and Z is NOT inserted in ENV a third time"))
  (when (not (symbolp symbol))
    (error "~S is not a symbol." symbol))
  (when (not (type~p type))
    (error "~S is not a type." type))
  (when (not (env~p env))
    (error "~S is not an environment." env))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((or allow-multiple (null thing))
	   (env~enter symbol (sym~variable-create symbol type) env))
	  ((not (sym~variable-p thing))
	   (post~error "Can't declare ~A as a  variable, because it already exists in the environment as a ~A. " 
		       symbol (class-name (class-of thing))))
	  ((not (keim~equal (term~type thing) type))
	   (post~error "Can't declare ~A as a variable of type ~A, because it already exists in the environment with type ~S." 
		       symbol type (term~type thing)))
	  (t thing))))

(term~warning-rename-fn term~env-enter-constant sym~env-enter-constant)

(defun sym~env-enter-constant (symbol type env)
  (declare
   (authors nesmith)
   (input "A lisp symbol, a type+type and an environment.")
   (effect "If the symbol has no association in environment, then a new"
	   "term constant with the symbol as name and given type is created and "
	   "entered in the environment.  If the symbol is associated with a "
	   "non-constant or with a constant of a differing type, an error is signaled."
	   "If the symbol is already associated with a constant of the proper type, "
	   "then that constant is returned.")
   (value "The constant, if one is successfully made.")
   (example "Let ENV be an environment NOT containing C"
	    "'C  (I I I) ENV  --> C   and C is inserted in ENV"
	    "'C  (I I I) ENV  --> C   and C is NOT inserted in ENV a second time"))
  (when (not (symbolp symbol))
    (error "~S is not a symbol." symbol))
  (when (not (type~p type))
    (error "~S is not a type." type))
  (when (not (env~p env))
    (error "~S is not an environment." env))
  (let ((thing (env~lookup-object symbol env)))
    (cond ((null thing)
	   (let ((constant (sym~constant-create symbol type)))
	     (env~enter symbol constant  env)
	     constant))
	  ((not (sym~constant-p thing))
	   (post~error "Can't declare ~A as a  constant, because it already exists in the environment as a ~A. " 
		       symbol (class-name (class-of thing))))
	  ((not (keim~equal (term~type thing) type))
	   (post~error "Can't declare ~A as a constant of type ~A, because it already exists in the environment with type ~S." 
		       symbol type (term~type thing)))
	  (t thing))))

(term~warning-rename-fn term~constant-create sym~constant-create)

(defgeneric sym~constant-create (thing type &optional refsym)
  (declare 
   (authors nesmith)
   (input   "A shared symbol or a symbol, and a type. Optionally a reference"
	    "symbol (currently ignored).")
   (effect  "none")
   (value   "Creates a new constant using the name of the given symbol and"
	    "the type, possibly using the properties of the optional reference symbol,"
	    "and returns it.")
   (example "'C I --> C"
	    "'C I X  --> C"))
  (:method ((symbol symbol) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (string symbol) 
			    :type type)))
	(make-instance 'sym+const :symbol shsym :type type)))
  (:method ((symbol shsym+sharedsymbol) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (keim~name symbol)
			    :type type)))
	(make-instance 'sym+const :symbol shsym :type type)))
  (:method ((symbol sym+sym) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (keim~name symbol)
			    :type type)))
	(make-instance 'sym+const :symbol shsym :type type))))

(defvar sym*ren-var-hashtable (make-hash-table :test #'equal))

(defun sym=rename-ctr (var)
  ; var must either be a sym+var or a string 
  (let* ((oldname (copy-seq (if (stringp var) var (string (keim~name var)))))
	 (oldbasename (string-right-trim (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
					 oldname))
	 (old-int
	  (if (< (length oldbasename) (length oldname))
	      (parse-integer oldname :start (length oldbasename))))
	 (ctr (gethash oldbasename sym*ren-var-hashtable 0)))
    (incf ctr)
    (when (and old-int (= old-int ctr))
      (incf ctr))
    (setf (gethash oldbasename sym*ren-var-hashtable) ctr)
    (values oldbasename ctr)))

(term~defgeneric sym~rename-var ((var) &optional type)
  (declare
   (authors nesmith)
   (value "The new variable.")
   (input "A variable or string, and optional type (defaults to the type of the variable.")
   (effect "Creates a new variable with the same base name as the input variable or string, but a new
            integer suffix"))
  (:method ((var sym+var) &optional (type (term~type var)))
    (multiple-value-bind (oldbasename ctr)
	(sym=rename-ctr var)
    (sym~variable-create 
     (make-symbol (format nil "~A~D" oldbasename ctr))
     type
     var)))
  (:method ((var string) &optional type)
    (multiple-value-bind (oldbasename ctr)
	(sym=rename-ctr var)
    (sym~variable-create 
     (make-symbol (format nil "~A~D" oldbasename ctr))
     type
     var))))



(term~warning-rename-fn term~variable-create sym~variable-create)

(defgeneric sym~variable-create (thing type &optional refsym)
  (declare 
   (authors nesmith)
   (input   "A term symbol, a shared symbol or a symbol, and a type."
	    "Optionally a reference symbol (currently ignored).")
   (effect  "none")
   (value   "Creates a new variable using the name of the given symbol and the"
	    "type, and possibly using the properties of the optional reference symbol,"
	    "and returns it.")
   (example "'X I --> X, "
	    "'X I C  --> X"))
  (:method ((symbol symbol) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (string symbol) 
			    :type type)))
	(make-instance 'sym+var :symbol shsym :type type)))
  (:method ((symbol shsym+sharedsymbol) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (keim~name symbol)
			    :type type)))
	(make-instance 'sym+var :symbol shsym :type type)))
  (:method ((symbol sym+sym) (type type+type) &optional refsym)
      (declare (ignore refsym))
      (let ((shsym 
	     (make-instance 'shsym+sharedsymbol :name (format nil "x-~A" (incf term*variable-counter))   ;(keim~name symbol)
			    :type type)))
	(make-instance 'sym+var :symbol shsym :type type))))

; POST output

(defmethod post~print ((sym sym+sym) stream)
  (format stream "~A" (keim~name sym)))

(defmethod post~print-declaration ((variable sym+var) stream)
  (format stream "(~A " (keim~name variable))
  (post~print (term~type variable) stream)
  (format stream ")"))

(defmethod keim~name ((term sym+sym))
  (keim~name (sym~shared-symbol term)))

(defmethod env~post-print (key (var sym+var) stream)
  (declare (ignore key))
  (format stream "~&(variables (~A " (keim~name var))
  (post~print (term~type var) stream)
  (format stream "))~%")
  (values))

(defmethod env~post-print (key (con sym+const) stream)
  (declare (ignore key))
  (format stream "~&(constants (~A " (keim~name con))
  (post~print (term~type con) stream)
  (format stream "))~%")
  (values))

(defmethod term~binding ((term sym+sym))
  (let ((binding (term~term-binding term)))
    (if binding
	(if (shsym~binding (sym=symbol term))
	    (error "~A has the term-binding ~A and the symbol-binding ~A-~%Only one of these slots is allowed to be filled.")
	  binding)
      (shsym~binding (sym=symbol term)))))


(defmethod term~set-binding! ((term sym+sym) new-binding)
  (sym~set-symbol-binding! term new-binding))

(defmethod keim~plist ((sym sym+sym))
  (declare (edited  "06-DEC-1991 14:20")
	   (authors RICHTS)
	   (input   "A constant.")
	   (effect  "None.")
	   (value   "The unshared property-list of CONSTANT.")
	   (remark  "This definition overwrites the method inherited from KEIM+OBJECT which would deliver the unshared plist."
		    "The unshared plist can be used for all terms with TERM~PLIST."))
   (keim~plist (sym=symbol sym)))

(defmethod keim~set-plist! ((sym sym+sym) new-plist)
  (declare (edited  "06-DEC-1991 14:20")
	   (authors RICHTS)
	   (input   "A logical symbol and a property list.")
	   (effect  "The shared property-list of SYM is set to NEW-PLIST.")
	   (value   "NEW-PLIST.")
	   (remark  "This definition overwrites the method inherited from KEIM+OBJECT"
		    "which would change the unshared plist."
		    "The unshared plist can be changed for all terms with TERM~PLIST."))
  (keim~set-plist! (sym=symbol sym) new-plist))


(defmethod term~poly-subterm-find ((term sym+sym))
  (declare (authors nesmith)
	   (input "A term")
	   (effect "none")
	   (value "NIL if all symbols in the term have ground type, otherwise"
		  "the first subterm that has polymorphic type.")
	   (example "Let FORALL have polymorphic type ((AA) -> o) -> o,  e.g."
		    "FORALL --> FORALL"
		    "(P x)  --> NIL"))
  (unless (type~ground-p (term~type term))
    term))

(term~warning-rename-fn term=env-lookup-symbol sym=env-lookup-symbol)

(defun sym=env-lookup-symbol (term env)
  (declare
   (authors nesmith)
   (input  "A Lisp symbol representing a term and an environment.")
   (effect "none")
   (value  "Looks up the symbol in the environment.  If it maps to a term,
the term is returned, otherwise an error is signaled."))
  (let ((obj (env~lookup-object term env)))
    (cond ((not obj) 
	   (post~error "~A is not an existing term." term))
	  ((not (term~p obj))
	   (post~error "~A is declared in the environment as a ~A, 
not as a term." term (class-of obj)))
	  (t obj))))

(defmethod post~read-object ((term symbol) (env env+environment) 
			    (indicator (eql :existing-term)))
  (sym=env-lookup-symbol term env))






