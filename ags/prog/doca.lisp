;;; -*- MODE: LISP; PACKAGE: AGS; SYNTAX: COMMON-LISP; BASE: 10 -*-
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

(IN-PACKAGE "AGS")

(mod~defmod doca :uses (mod )
	    :documentation "Documentation in readable ASCII format."
	    :exports (DOCA~PRINT-FUNCTIONS))
#{
\section{ASCII Format Documentation}
\label{mod:doca}
 This module contains functions to document functions in ASCII format.
 The functions correspond to the functions in the \LaTeX module {\tt DOCL}.
 The prefix is {\tt DOCA} for Documentation-ASCII.
#}

(DEFUN DOCa=REMASSOC (KEY ALIST)
  (declare (edited  "14-AUG-1992 19:36")
	   (authors PRCKLN)
	   (input   "AN ITEM AND AN ASSOCIATIONLIST")
	   (effect  "THE KEY-ELEMENT OF ALIST IS DESTRUCTIVELY"
	            "REMOVED. ")
	   (value   " THE NEW ALIST."))
  (IF (eQL KEY (CAAR ALIST))
      (SETQ ALIST (CDR ALIST))
      (LET ((ALIST1 ALIST)
	    (ALIST2 (CDR ALIST)))
	(IF (eql KEY (CAAR ALIST2))
	    (SETF (CDR ALIST1) (CDR ALIST2)
		  ALIST2 NIL)
	    (if alist2
		(DOCa=REMASSOC key (CDR ALIST2))
		nil))))
  ALIST)

(DEFVAR DOCA*COMMENT-FORMAT :BOTH
	;; the variable is used to control the printing of comments. If it is :lisp, only :lisp comments will be printed,
	;; if it is :latex, only :latex comments will be printed, if it is :both, all comments are printed.
	":lisp, :latex or :both")

(DEFUN DOCA=GET-USAGE (FUNCTION)
  (declare (edited  "14-AUG-1992 20:17")
	   (authors PRCKLN)
	   (input   "'function' is symbol: the name of a function macro or subst")
	   (effect  )
	   (value   "one of the symbols:"
		    "  interface:   ~ or +"
		    "  internal:    ="
		    "  unknown:     in all other cases."))
  (CASE (FIND-IF (FUNCTION (LAMBDA (CHARACTER) (MEMBER CHARACTER '(#\- #\~ #\+ #\=))))
		 (SYMBOL-NAME FUNCTION))
    (#\= 'INTERNAL)
    ((#\- #\~ #\+) 'INTERFACE)
    (OTHERWISE 'unknown)))

(DEFUN DOCA=SYS-DECLARATIONS (BODY)
  (declare (edited  "14-AUG-1992 19:54")
	   (authors PRCKLN)
	   (input   "The body of a DEFUN, DEFMACRO, DEFSUBST expression.")
	   (effect  "None.")
	   (value   "The input of the first expression if it is a declare else nil"))
  (let ((declares nil) assoc (comments nil))
    (do ((lisp-form-p nil (or lisp-form-p
			      (AND (CONSP (CAR BODY))
				   (not (eq 'DECLARE (CAAR BODY)))
				   (not (eq 'doc=c (caar body)))))))
	((not body)) 
      (cond ((AND (not lisp-form-p)
		  (CONSP (CAR BODY))
		  (EQ 'DECLARE (CAAR BODY)))
	     (mapc (function (lambda (pair)
			       (cond ((consp pair)
				      (setq declares (if (setq assoc (assoc (car pair) declares))
							 (prog1 declares (rplacd assoc (union (cdr assoc) (cdr pair))))
							 (cons pair declares)))))))
		   (CDAR BODY)))
	    ((and (not lisp-form-p)
		  (consp (car body))
		  (eq 'ana=c (caar body)))
	     (push (second (first body)) comments))
	    (t nil))      
      (pop body))
    (if comments
	(cons (cons 'comments (nreverse comments)) declares)
	declares)))

(defun DOCA=documentation-strings (body)
  (declare (edited  "14-AUG-1992 19:55")
	   (authors PRCKLN)
	   (input   "BODY is definition body.")
	   (effect  "None.")
	   (value   "The list of documentation strings of BODY."))
  (do ((bodylist body (cdr bodylist))
       (doc-strings nil (if (and (consp bodylist)
				 (stringp (car bodylist)))
			    (cons (car bodylist) doc-strings)
			    doc-strings)))
      ((or (atom bodylist)
	   (and (consp (car bodylist))
		(not (eq 'declare (caar bodylist)))))
       doc-strings)))

(defun doca=print-definitions (PRINT-FILEINFO DOC-STREAM DOC-KEYS doc-type)
  (declare (input "DOC-TYPE . a keyword (:interface or :complete)")
	   (effect "DOC-TYPE controls which functions etc. will be documented"))
  (MAPC #'(LAMBDA (DEFINITION)
	    (COND ((CONSP DEFINITION)
		   (CASE (CAR DEFINITION)
		     (eval-when (DOCL=PRINT-DEFINITIONS-LATEX (rest (rest definition)) DOC-STREAM DOC-KEYS DOC-TYPE))
		     ((DEFUN DEFMACRO)						; Object to be printed is functional.
		      (setq doca*comment-format :both)
		      (LET ((DEFNAME (CADR DEFINITION))
			    (DEFBODY (CDDDR DEFINITION))
			    (DEFLL (CADDR DEFINITION)))
			(WHEN (NOT (GET DEFNAME 'NO-DOCALYSE))
			  (WHEN (AND DOC-STREAM
				     (or (eq :complete doc-type)
					 (EQ 'INTERFACE (DOCA=GET-USAGE DEFNAME))))
			    ;; Documentation
			    (DOCA=PRINT-DOC-FUN DEFNAME DEFLL (doca=SYS-DECLARATIONS DEFBODY) DOC-STREAM DOC-KEYS
					       (doca=DOCUMENTATION-STRINGS DEFBODY))))))
		     ((defclass)
		      (DOCa=PRINT-CLASS-LATEX DEFINITION DOC-STREAM))
		     ((defun-documentation defclass-documentation defsubst-documentation defmacro-documentation)
		      (setq doca*comment-format :both)
		      (when (and doc-stream
				 (or (eq :complete doc-type)
				     (eq 'interface (DOCA=GET-USAGE (cadr definition)))))
			(doca=print-documentation definition doc-stream doc-keys)))
		     (defstruct
		       (setq doca*comment-format :both)
		       (when (and doc-stream
				  (or (eq :complete doc-type)
				      (EQ 'INTERFACE (DOCA=GET-USAGE
						       (if (consp (second definition))	; name of structure
							   (car  (second definition))
							   (second definition))))))
			 (doca=print-structure definition doc-stream))
		       )
		     (DOCA=CCC
		       (DOCA=PRINT-DOC-COM (CDR DEFINITION) DOC-STREAM))
		     (DOCA=CCCC
		       (unless (eq doca*comment-format :latex)
			 (LET ((NUMBER-OF-LEADING-BLANKS 0))
			   (SOME #'(LAMBDA (CHAR) (IF (CHAR= #\SPACE CHAR)
						      (INCF NUMBER-OF-LEADING-BLANKS) 
						      NIL))
				 (CADR DEFINITION))
			   (FORMAT DOC-STREAM	
				   "~%~A~%~V@{~A~:*~}~*~V@{~C~:*~}~%"
				   (CADR DEFINITION)
				   NUMBER-OF-LEADING-BLANKS
				   " "
				   (- (LENGTH  (CADR DEFINITION)) NUMBER-OF-LEADING-BLANKS)
				   #\-))))	    
		     (OTHERWISE
		       (COND (nil ; To Do: print generators
			      (doca=print-definitions (macroexpand-1 definition) DOC-STREAM DOC-KEYS doc-type))
			     ((EQL 0 (SEARCH "DEF" (symbol-name (CAR DEFINITION))
					     :END2 (min (length (symbol-NAME (CAR DEFINITION))) 3)))
			      (setq doca*comment-format :both)
			      (LET ((defoutput (CADR DEFINITION))
				    (defname (if (symbolp (CADR DEFINITION))
						 (CADR DEFINITION)
						 (if (and (consp (CADR DEFINITION))
							  (consp (CdADR DEFINITION))
							  (symbolp (CadADR DEFINITION)))
						     (CadADR DEFINITION)
						     (cadr definition))))
				    (DEFBODY (CDDR DEFINITION))
				    ARGS)
				(COND ((or (symbolp (CADR DEFINITION))
					   (and (consp (CADR DEFINITION))
						(consp (CdADR DEFINITION))
						(symbolp (CadADR DEFINITION))))
				       (UNLESS (AND (CONSP (CAR DEFBODY))
						    (EQ 'DECLARE (CAAR DEFBODY)))
					 (SETQ ARGS (CAR DEFBODY))
					 (SETQ DEFBODY (CDR DEFBODY)))
				       (WHEN (AND DOC-STREAM
						  (or (eq :complete doc-type)
						      (EQ 'INTERFACE
							  (DOCA=GET-USAGE DEFNAME))))
					 (DOCA=PRINT-DOC-FUN DEFoutput ARGS (doca=SYS-DECLARATIONS DEFBODY)
							    DOC-STREAM DOC-KEYS (doca=DOCUMENTATION-STRINGS DEFBODY)))))))))))))
	PRINT-FILEINFO))

(DEFUN DOCA~PRINT-FUNCTIONS (info DOC-STREAM DOC-KEYS doc-type)
  (declare (edited  "14-AUG-1992 22:03")
	   (authors PRCKLN)
	   (INPUT  "INFO is a pathname for the input lisp file."
		   "DOC-STREAM is the stream where the information is printed."
		   "DOC-KEYS are the declare keys to be documented."
		   "DOC-TYPE is a keyword (:interface or :complete),"
		   "controlling which functions are documented.")
	   (EFFECT "prints information from INFO on DOC-STREAM.")
	   (VALUE  "Undefined."))
  (doca=print-definitions INFO DOC-STREAM DOC-KEYS doc-type)) 

(defun doca=print-cassoc (key decls)
  (rest (ASSOC (symbol-name key) DECLS :test #'string= :key #'symbol-name)))

(DEFUN DOCA=PRINT-DOC-FUN (NAME LAMBDALIST DECLARATIONS DOC-STREAM DOC-KEYS DOC-STRINGS)
  (DECLARE (INPUT "LAMBDALIST is the lambda list of the definition NAME to be documented"
		  "on the open output stream DOC-STREAM. DECLARATIONS is the list of its declarations."
		  "DOC-STRINGS is the list of documentation strings.")
	   (EFFECT "Prints documentation for a definition on the stream DOC-STREAM."))
  (LET ((INPUT (doca=print-cassoc 'INPUT DECLARATIONS))
	(EFFECT (DOCA=PRINT-CASSOC 'EFFECT DECLARATIONS))
	(VALUE (DOCA=PRINT-CASSOC 'VALUE DECLARATIONS))
	(VALUES (DOCA=PRINT-CASSOC 'VALUES DECLARATIONS))
	(REMARK (DOCA=PRINT-CASSOC 'REMARK DECLARATIONS))
	(EXAMPLE (DOCA=PRINT-CASSOC 'EXAMPLE DECLARATIONS))
	(EXAMPLES (DOCA=PRINT-CASSOC 'EXAMPLES DECLARATIONS))
	(comments (DOCA=PRINT-CASSOC 'comments DECLARATIONS))
	(LAMBDALIST (LET ((ARGLIST (DOCA=PRINT-CASSOC 'ARGS DECLARATIONS)))
		      (COND (ARGLIST (LIST ARGLIST))
			    ((listp LAMBDALIST) lambdalist)
			    (t nil)))))

    (WHEN (OR INPUT effect VALUE VALUES REMARK EXAMPLE EXAMPLES comments
	      (DOCA=PRINT-CASSOC 'AUTHORS DECLARATIONS)
	      (DOCA=PRINT-CASSOC 'EDITED DECLARATIONS))
      (FORMAT DOC-STREAM "~%     ~@(~A~):~%     ~V@{~C~:*~}~%" NAME (1+ (LENGTH (symbol-name NAME))) #\-)
      (FORMAT DOC-STREAM "  Arguments: ~{~:A ~}~%" LAMBDALIST)      
      (format doc-stream "~{ ~A~%~}" comments)
      (unless (some #'(lambda (string) (search "Author" string)) comments)
	(FORMAT DOC-STREAM "  Authors: ~:[~:* Nobody edited this.~;~:*~{ ~A~}~]~&" (DOCA=PRINT-CASSOC 'AUTHORS DECLARATIONS)))
      (unless (some #'(lambda (string) (search "Edited" string)) comments)
	(FORMAT DOC-STREAM "  Edited:  ~:[~:* This function doesn't exist.~;~:*~{ ~A~}~]~&"
		(DOCA=PRINT-CASSOC 'EDITED DECLARATIONS)))
      (unless (some #'(lambda (string) (search "Input" string)) comments)
	(FORMAT DOC-STREAM "  Input:    ")
	(COND ((AND LAMBDALIST (NOT INPUT))
	       (FORMAT DOC-STREAM "These bad authers didn't comment the input.~&"))
	      (INPUT (do ()
			 ((not (member (first input) '("end{latex}" "end{lisp}" "begin{lisp}" "begin{latex}"
						       "end{verbatim}" "begin{verbatim}")
				       :test #'string=)))
		       (doca=print-doc-com (list (pop input)) doc-stream))
		     (doca=print-doc-com (list (first input)) doc-stream)
		     (doca=print-doc-com (rest input) doc-stream "            "))
	      (T (FORMAT DOC-STREAM " No input.~&"))))
      (unless (some #'(lambda (string) (search "Effect" string)) comments)
	(FORMAT DOC-STREAM "  Effect:   ")
	(COND ((AND LAMBDALIST (NOT EFFECT))
	       (FORMAT DOC-STREAM "These bad authers didn't comment the effect.~&"))
	      (EFFECT (do ()
			  ((not (member (first effect) '("end{latex}" "end{lisp}" "begin{lisp}" "begin{latex}"
							 "end{verbatim}" "begin{verbatim}")
					:test #'string=)))
			(doca=print-doc-com (list (pop effect)) doc-stream))
		      (doca=print-doc-com (list (first effect)) doc-stream)
		      (doca=print-doc-com (rest effect) doc-stream "            "))
	      (T (FORMAT DOC-STREAM " No effect.~&"))))
      (unless (some #'(lambda (string) (search "Value" string)) comments)
	(FORMAT DOC-STREAM "  ~A  ~:[~:* Undefined.~;~:* ~A~&~{            ~A~&~}~]"
		(COND (VALUES "Values:")
		      (T "Value: "))
		(COND (VALUE (CAR VALUE))
		      (VALUES (CAR VALUES)))
		(COND (VALUE (CDR VALUE))
		      (VALUES (CDR VALUES)))))
      (WHEN REMARK (FORMAT DOC-STREAM "  Remark:   ~A~&~{            ~A~&~}" (CAR REMARK) (CDR REMARK)))
      (setq example (OR EXAMPLES EXAMPLE))
      (WHEN example
	(FORMAT DOC-STREAM "  ~A" (if EXAMPLES "Examples: " "Example:  "))
	(do ()
	    ((not (member (first example) '("end{latex}" "end{lisp}" "begin{lisp}" "begin{latex}"
					 "end{verbatim}" "begin{verbatim}") :test #'string=)))
	  (doca=print-doc-com (list (pop example)) doc-stream))
	(doca=print-doc-com (list (first EXAMPLE)) doc-stream)
	(doca=print-doc-com (rest EXAMPLE) doc-stream "            "))
      (MAPC #'(LAMBDA (KEY)
		(LET ((KEYVALUE (Cdr (ASSOC KEY DECLARATIONS :KEY #'SYMBOL-NAME :TEST #'STRING=))))
		  (WHEN KEYVALUE
		    (FORMAT DOC-STREAM "  ~A:   ~A~&~{            ~A~&~}"
			    (STRING-CAPITALIZE (symbol-NAME KEY))
			    (CAR KEYVALUE) (CDR KEYVALUE)))))
	    DOC-KEYS)
      (WHEN DOC-STRINGS
	(FORMAT DOC-STREAM "  ~A ~A~&~{            ~A~&~}"
		"Documentation:"
		(CAR DOC-STRINGS) (CDR DOC-STRINGS)))
      (FORMAT DOC-STREAM "~%"))))

(defconstant doca*COUNT-STANDARD-RIGHT-MARGIN 117 "default right margin, especially for streams not in the hash table")

(defparameter
  doca*COUNT-PRINT-BUFFER (make-array doca*COUNT-STANDARD-RIGHT-MARGIN :element-type 'character :adjustable T :fill-pointer 0)
  "a buffer to hold output, in order to determine its print length")

(defun doca=print-length (obj &optional printflag)
  (with-output-to-string (string doca*count-print-buffer)
    (if printflag (prin1 obj string) (princ obj string)))
  (prog1 (length doca*count-print-buffer)
	 (setf (fill-pointer doca*count-print-buffer) 0)))

(defun doca=print-structure (definition doc-stream)
  (declare (edited  "03-JUL-1990 08:56")
	   (authors DEISS)
	   (input   "DEFINITION is the definition of a defstruct."
		    "DOC-STREAM is a stream open for output.")
	   (effect  "information about DEFINITION is written on DOC-STREAM.")
	   (value   "undefined"))
  (let* ((structure-name (if (consp (second definition))
			     (first (second definition))
			     (second definition)))
	 (structure-options (if (consp (second definition))
				(cdr (second definition))
				nil))
	 doc-strings
	 slotnames
	 structure-slots	
	 )
    (if (member :simple structure-options :test #'eq)
	(Setq doc-strings nil
	      structure-slots (remove-if-not #'symbolp (cddr definition)))
	(progn
	  (setq doc-strings nil
		slotnames nil ; To Do
		structure-slots (remove-if-not #'(lambda (expr)
						   (cond ((symbolp expr) (member (symbol-name expr) slotnames :test #'string=))
							 ((consp expr)
							  (member (symbol-name (car expr)) slotnames :test #'string=))
							 (t nil)))
					       (cddr definition)))
	  (when (stringp doc-strings) (setq doc-strings (list doc-strings)))
	  ))
    (format doc-stream
	    "~%     ~@(~A~):~%     ~V@{~C~:*~}~%"
	    structure-name
	    (1+ (doca=print-length structure-name))
	    #\-)
    (when structure-options
      (format doc-stream "  options:~%")
      (mapc #'(lambda (option)
		(if (consp option)
		    (case (car option)
		      ((:type :write-demons)
		       (format doc-stream "    ~(~A~)~{~(~S ~)~}~%"
			       (if (eq (car option) :type)
				   ":type         "
				   ":write-demons ")
			       (cdr option)))
		      ((:conc-name :alterant :delete :copier :with :predicate :include :extension)
		       (format doc-stream "    ~(~S~)~V{~C~:*~}~(~A~)~%"
			       (car option)
			       (- 13 (doca=print-length (car option))) (list #\space)
			       (second option)))
		      ((:optimized :external)
		       (format doc-stream "    ~(~S~)    ~(~{~S ~}~)~%"
			       (car option)
			       (cdr option)))
		      (:pprint
			(format doc-stream "    :pprint       ~[dummy~;non-standard~;~(~A~) non-standard~]~%"
				(length (cdr option))
				(cadr option)))
		      (:constructor
			(format doc-stream "    :constructor  ~(~A~)~{~:A~}~%"
				(cadr option)
				(caddr option))
			(when (cdddr option)
			  (format doc-stream "                  ~:[preforms defined~%~;pre- and postforms defined~%~]"
				  (cddddr option))))
		      ((:additional-functions :additional-macros :additional-defsubsts)
		       (format doc-stream "    ~(~S~)~:{ ~(~A~)~}~%"
			       (car option)
			       (cdr option)))
		      )
		    (format doc-stream "    ~(~S~)~%" option)	; options without arguments
		    ))
	    structure-options))
    (format doc-stream "  slots:~%")
    (mapc #'(lambda (slot)
	      (if (consp slot)
		  (progn
		    (format doc-stream "    ~(~A~)~%" (car slot))
		    (mapc #'(lambda (option)
			      (if (consp option)
				  (case (car option)
				    (:default (format doc-stream "      :default    ~(~:A~)~%" (second option)))
				    (:functional (format doc-stream "      :functional ~(~A~)~%" (second option)))
				    (:pprint (format doc-stream "      :pprint     defined by user~%"))
				    ((:copier :linearizer)
				     (format doc-stream "      ~(~A~) ~:[~;~(~A~) ~]defined by user~%"
					     (if (eq (car option) :linearizer)
						 ":linearizer"
						 ":copier    ")
					     (symbolp (second option))
					     (second option)))
				    (:doc-string (format doc-stream "      :doc-string ~A~%" (second option)))
				    (:check (format doc-stream "      :check      ~:[defined by user~;~(~A~)~]~%"
						    (symbolp (second option))
						    (second option)))
				    (:type (format doc-stream "      :type       ~(~A~)~%" (second option)))
				    (:slot-type (format doc-stream "      :slot-type  ~(~A~)~%" (second option)))
				    )
				  (format doc-stream "      ~(~S~)~%" option)))
			  (cdr slot))
		    )
		  (format doc-stream "    ~(~A~)~%" slot)))
	  structure-slots)
    (when doc-strings
      (format doc-stream "  documentation:~%~{    ~A~%~}~%" doc-strings))      
    )
  )


(defun doca=print-documentation (definition doc-stream doc-keys)
  (declare (edited  "28-JUN-1990 13:24")
	   (authors DEISS)
	   (input   "DEFINITION is the definiton of a defun-documentation, etc."
		    "DOC-STREAM is a stream open for output."
		    "DOC-KEYS are additional documenation keys.")
	   (effect  "The name, arguments, declarations etc. of DEFINITION are computed and printed on DOC-STREAM"
		    "using DOCA=PRINT-DOC-FUN.")
	   (value   "undefined."))
  (let* ((defname (cadr definition))
	 (defbody (cddr definition))
	 arguments
	 (declarations (doca=sys-declarations defbody))
	 (doc-strings (doca=documentation-strings defbody))
	 )
    (setq arguments (doca=print-cassoc 'args declarations)
	  declarations (doca=remassoc 'args declarations))
    (doca=print-doc-fun defname arguments declarations doc-stream doc-keys doc-strings))
  )

(defun doca=print-doc-com (comlist doc-stream &optional (prefix ""))
  (declare (edited  "26-AUG-1990 18:44")
	   (authors PRCKLN)
	   (input   "COMLIST is a list of strings."
		    "DOC-STREAM is a stream open for output."
		    "PREFIX is printed at the beginning of each line.")
	   (effect  )
	   (value   ))
  (unless (or (eql (search " -*- " (car comlist)) 0)
	      (and (every (function (lambda (char) (or (char= #\; char)))) (car comlist))
		   (string/= (car comlist) "")))
    (mapc #'(lambda (comment)
	      (cond ((eq doca*comment-format :latex)
		     (when (search "end{latex}" comment)
		       (setq doca*comment-format :both)))
		    ((search "begin{latex}" comment)
		     (setq doca*comment-format :latex))
		    ((search "begin{lisp}" comment))	; do nothing
		    ((search "end{lisp}" comment))
		    ((search "begin{verbatim}" comment))
		    ((search "end{verbatim}" comment))
		    (t (format doc-stream "~A~A~%" prefix comment))
		    ))
	  comlist))
  )

(defun doca=print-doc-top (Type name doc-stream doc-type)
  (declare (input "DOC-TYPE . a keyword (:interface or :complete), controls whether only interface functios or all functions"
		  "           will be documented"))
    (format doc-stream "~%~%~:[Documentation~;Interface~] of ~@(~A~) ~A~%~V@{~C~:*~}~%"
	    (eq doc-type :interface) type name
	    (+ (length (symbol-name type))
	       (length (symbol-name name))
	       (case doc-type
		 (:interface 9)
		 (:complete 13))
	       5)
	    #\=))









(DEFUN DOCa=PRINT-CLASS-LATEX (DEFINITION DOC-STREAM)
  (DECLARE (EDITED  "20-MAY-1992 08:56")
	   (AUTHORS PRCKLN)
	   (INPUT   "DEFINITION is the definition of a defstruct."
		    "DOC-STREAM is a stream open for output.")
	   (EFFECT  "Information about DEFINITION is written on DOC-STREAM.")
	   (VALUE   "Undefined."))
  (LET* ((STRUCTURE-NAME (IF (CONSP (SECOND DEFINITION))
			     (FIRST (SECOND DEFINITION))
			     (SECOND DEFINITION)))
	 DOC-STRING)
    (SETQ DOC-STRING (SECOND (ASSOC :DOCUMENTATION (REMOVE-IF #'SYMBOLP (CDDDR DEFINITION)))))
    (FORMAT DOC-STREAM "~%     ~@(~A~):~%     ~V@{~C~:*~}~%" structure-NAME (1+ (LENGTH (symbol-name structure-NAME))) #\-)
    (PRINC (IF DOC-STRING (DOCL=STRING-LATEX DOC-STRING) "No documentation string available!!!!!!!") DOC-STREAM)
    )
  )
