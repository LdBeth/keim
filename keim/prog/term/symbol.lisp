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


(IN-PACKAGE "KEIM")

(mod~defmod shsym :uses (keim mod type )
	    :documentation "Shared symbols form a layer below the terms."
	    :exports (
		      shsym+sharedsymbol
		      shsym~create
		      shsym~type
		      shsym~binding
		      shsym~set-binding!
		      shsym~label
		      shsym~set-label!
		      shsym~p
		      )
	    )



#{\section{Shared Symbols}\label{mod:shsym}
\begin{warning} 
Shared Symbols are very internal objects of \keim\, they should not appear outside of the term module
anyone messing around with them does so at his or her own risk.
\end{warning}

Symbols are that part of \keim\ terms that are shared by all terms in which they occur, thus it is
efficient to keep all symbols in one data structure. Object variables, however, tend to be so numerous
in automated deduction that they would clog the signature and hamper efficiency. In order to take
advantage of the shared symbols the class {\tt shsym+sharedsymbol} has a binding cell (see \ref{bindings}).
#}

(eval-when (load compile eval)
  (defclass shsym+sharedsymbol (keim+name keim+term)
    ((type :initarg :type :reader shsym=type) ; ?? brauchen wir?
     (binding :initform nil :reader shsym=binding :writer shsym=write-binding!)
     (label :initform nil :reader shsym=label :writer shsym=write-label!))
    (:documentation "This is the class of all shared symbols of terms. All instances of the classes
                     SYM+VAR and SYM+CONST contain instances of this class")))


(defgeneric shsym~create (symbol name type &key)
  (declare (edited  "08-AUG-1991 13:30")
	   (authors RICHTS)
	   (input   "A reference symbol {\vb SYMBOL}, a {\vb NAME}, a {\vb TYPE}"
		    "and further keyword pairs.")
	   (effect  "None.")
	   (value   "A shared-symbol of the same class as {\vb SYMBOL} with"
		    "name {\vb NAME} and type {\vb TYPE}"
		    "as specified by the keyword pairs.")
	   (example "(make-instance 'term+term)  'sh1  I    --> SH1"
		    "SH1                         'SH2   O   --> SH2" ))
  (:method ((symbol keim+term) name type &key)
	   (make-instance 'shsym+sharedsymbol :name (string name) :type type)))

(defgeneric shsym~type (symbol)
  (declare (edited  "12-JUN-1992 14:58" )
	   (authors KOHLHASE )
	   (input   "A symbol.")
	   (effect   "None.")
	   (value    "The type of the symbol.")
	   (example "(shsym~create sh1 'sh2 (type~O)) --> O"))
  (:method ((symbol shsym+sharedsymbol))
	   (shsym=type symbol)))

(defgeneric shsym~binding (symbol)
  (declare (edited  "12-JUN-1992 14:58" )
	   (authors KOHLHASE )
	   (input   "A symbol.")
	   (effect   "None.")
	   (value    "The value of the binding cell of the symbol."))
  (:method ((symbol shsym+sharedsymbol))
	   (shsym=binding symbol)))

(defgeneric shsym~set-binding! (symbol binding)
  (declare (edited  "12-JUN-1992 14:58" )
	   (authors KOHLHASE )
	   (input   "A shared symbol and a new binding.")
	   (effect   "The binding-cell of the {\vb SYMBOL} is replaced by {\vb BINDING}.")
	   (value    "Undefined.")
	   (example "sh   'a   --> the binding of sh is set to 'a, i.e. (shsym~binding sh) --> 'a"))
  (:method ((symbol shsym+sharedsymbol) binding)
	   (shsym=write-binding! binding symbol)))

(defgeneric shsym~label (symbol)
  (declare (edited  "12-JUN-1992 14:58" )
	   (authors KOHLHASE )
	   (input   "A symbol.")
	   (effect   "None.")
	   (value    "The label of the symbol."))
  (:method ((symbol shsym+sharedsymbol))
	   (shsym=label symbol)))

(defgeneric shsym~set-label! (symbol label)
  (declare (edited  "12-JUN-1992 14:58" )
	   (authors KOHLHASE )
	   (input   "A shared symbol and a new label.")
	   (effect   "The name of the {\vb SYMBOL} is replaced by {\vb LABEL}.")
	   (value    "Undefined.")
	   (example "sh   'a   --> the binding of sh is set to 'a, i.e. (shsym~label sh) --> 'a"))
  (:method ((symbol shsym+sharedsymbol) label)
	   (shsym=write-label! label  symbol)))


(defun shsym~p (object)
  (declare (edited  "12-JUN-1992 15:01" )
	   (authors KOHLHASE )
	   (input    "A symbol.")
	   (effect   "None.")
	   (value    "True iff {\vb OBJECT} is a shared symbol."))
  (typep object 'shsym+sharedsymbol))


(defmethod print-object ((sharedsymbol shsym+sharedsymbol) stream)
  (format stream "~A" (keim~name sharedsymbol)))

