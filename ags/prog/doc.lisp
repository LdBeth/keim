;;; -*- Mode: LISP; PACKAGE: AGS; SYNTAX: COMMON-LISP; BASE: 10 -*-
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

(mod~defmod doc :uses (mod)
	    :exports (doc~latex doc~extract))

#{\section{Extracting documentation}
We try to put most of our documentation directly in the \lisp\ source
files themselves.  This makes it easier to keep the documentation up to
date, because when we change a function, we can immediately change its
documentation.

But we also want to read this documentation in the form of a \LaTeX\ document.
In order to do this, we run a collection of programs over the \lisp\ files,
extracting a {\vb .tex} file for each.  We automatically document
things like {\vb defclass}, {\vb defun}, {\vb defmacro}, and {\vb defgeneric}
forms.  They are documented using the special declarations at the beginning
of the form. For example:
\begin{code}
(defun foo (num lis fun)
  (declare
   (input "An integer NUM, a list LIS, and a function FUN")
   (value "A random number between 0 and NUM")
   (effect "Maps FUN NUM times over the list LIS.")
   (example "(foo 3 '(a b c) #'princ) -> 17"))
  (dotimes (n num (random num))
    (mapc fun lis)))
\end{code}
shows such a function definition. The {\vb input}, {\vb effect} and
{\vb value} declarations take any number of strings, which will in effect
be concatenated together when documentation is extracted).

One can also directly write \LaTeX\ documentation into the file by starting
it by \#\{ and ending it by \#\}, for example:
\begin{code}
\#\{
\\subsection\{The Turing Machine\}
In the following section, we define our implementation of a 
\{\\sc Turing Machine\}. 
\#\}
\end{code}
#}

(defmacro doc~latex (&rest doc)
  (declare 
   (ignore doc)
   (value "none")
   (authors nesmith)
   (effect "Ignores its arguments, returns no value")
   (input "You can input anything, it'll be ignored.  Actually, this
macro is defined only because the form \#\\{ blah blah blah \#\\} is
defined in our readtable to expand to (doc~latex \"blah blah blah\"). So
doc~latex has to evaluate to something when we're loading a lisp file.
When we're extracting documentation, we'll just write the string to
the .tex file."))
  (values))

(DEFUN DOC=GET-PATHNAME (NAME)
  (DECLARE (EDITED  "14-AUG-1992 14:50")
	   (AUTHORS PRCKLN)
	   (INPUT   "OBJECT  is a pathname, a symbol, or a string (corresponding to a pathname).")
	   (EFFECT  "Asks the user if no pathname can be associated with NAME.")
	   (VALUE   "A pathname, in which the documentation should be printed."))
  (COND ((PATHNAMEP NAME)
	 NAME)
	((SYMBOLP NAME)
	 (pathname (symbol-name name)))
	((STRINGP NAME)
	 (PATHNAME NAME))
	(T ;; Error case: Ask user
	 (CERROR "Enter another name." "~S is not a pathname or string for a pathname." NAME)
	 (DOC=GET-PATHNAME (READ)))))

(DEFUN DOC=OPEN-DOC-STREAM (FILE DOC-STREAM doc-type)
  (declare (edited  "14-AUG-1992 17:56")
	   (authors PRCKLN)
	   (input  "FILE is a pathname."
		   "DOC-STREAM is a stream, to print documentation, if the user had specified one, nil else."
		   "DOC-TYPE is a keyword (:INTERFACE, :SIMPLE-TEX, :INTERFACE-TEX, :COMPLETE or :COMPLETE-TEX).")
	   (effect "The doc-stream of FILE is set to DOC-STREAM if not nil. If DOC-STREAM is nil the "
		   "documentation stream of FILE is set to a stream corresponding to a file with the same filename "
		   "as FILE and extension doc or tex."
		   "DOC-TYPE controls the extension (doc or tex) of the documentation file, if one"
		   "has to be created.")
	   (value  "The list of opened streams")) 
  (or DOC-STREAM (OPEN (MAKE-PATHNAME :TYPE (case doc-type
					      ((:interface :complete) #+symbolics "DOC" #-symbolics "doc")
					      ((:simple-tex :interface-tex :complete-tex) #+symbolics "TEX" #-symbolics "tex"))
				      :DEFAULTS file)
		       :DIRECTION :OUTPUT
		       :if-exists :supersede
		       :if-does-not-exist :create)))

(DEFUN DOC=READ-COMMENT (INPUT-STREAM SEMICOLON-CHAR)
  (LIST (COND ((CHAR/= SEMICOLON-CHAR (PEEK-CHAR NIL INPUT-STREAM)) 'DOC=C)
	      (T (READ-CHAR INPUT-STREAM)
		 (COND ((CHAR/= SEMICOLON-CHAR (PEEK-CHAR NIL INPUT-STREAM)) 'DOC=CC)
		       (T (READ-CHAR INPUT-STREAM)
			  (COND ((CHAR/= SEMICOLON-CHAR (PEEK-CHAR NIL INPUT-STREAM)) 'DOC=CCC)
				(T (READ-CHAR INPUT-STREAM)
				   (COND ((CHAR/= SEMICOLON-CHAR (PEEK-CHAR NIL INPUT-STREAM)) 'DOC=CCCC)
					 (T (READ-CHAR INPUT-STREAM)
					    (COND ((CHAR/= SEMICOLON-CHAR (PEEK-CHAR NIL INPUT-STREAM)) 'DOC=CCCCC)
						  (T (READ-CHAR INPUT-STREAM)
						     'DOC=CCCCCC))))))))))
	(COND ((CHAR/= (PEEK-CHAR NIL INPUT-STREAM) #\RETURN) (READ-LINE INPUT-STREAM))
	      (T                                              (READ-CHAR INPUT-STREAM) ""))))

(DEFPARAMETER DOC*READTABLE-PRINT 
	      (LET ((RT (COPY-READTABLE *ags-READTABLE*)))
		(SET-MACRO-CHARACTER #\, (FUNCTION (LAMBDA (INPUT-STREAM COMMA-CHAR)
						     (DECLARE (IGNORE COMMA-CHAR))
						     (IF (CHAR/= (PEEK-CHAR NIL INPUT-STREAM) #\@)
							 (LIST 'DOC=UNQUOTE (READ INPUT-STREAM))
							 (PROGN (READ-CHAR  INPUT-STREAM)
								(LIST 'DOC=UNQUOTE-SPLICE (READ INPUT-STREAM))))))
				     NIL RT)
		(SET-MACRO-CHARACTER #\` (FUNCTION (LAMBDA (INPUT-STREAM WQUOTE-CHAR)
						     (DECLARE (IGNORE WQUOTE-CHAR))
						     (LIST 'DOC=WQUOTE (READ INPUT-STREAM))))
				     NIL RT)
		(SET-MACRO-CHARACTER #\; (FUNCTION DOC=READ-COMMENT) NIL RT)
		RT)
  "Readtable to be used for reading info to be printed from files.")


(defun doc=read-file (pathname)
  (declare (edited  "14-AUG-1992 21:11")
	   (authors PRCKLN)
	   (input   "A pathname.")
	   (effect  "None.")
	   (Value   "1. The list of lisp expressions on the file"
		    "2. TRUE iff (in-package ...) is on the file as first expression"))
  (let ((current-package *package*)
	(second-value nil))
    (with-open-file (stream pathname)
      (unwind-protect
	  (progn (in-package "USER")
		 (do ((expression (let ((expression (list (read stream nil 'eof-reached))))
				    (when (and (consp (car expression)) (eq (caar expression) 'in-package))
				      (setq second-value (cadar expression)))
				    expression)
				  (cons (read stream nil 'eof-reached) expression)))
		     ((eq (car expression) 'eof-reached) (values (nreverse (cdr expression)) second-value))))
	(setq *package* current-package)))))

(DEFUN DOC=GET-PRINT-LIST (FILE)
  (declare (edited  "14-AUG-1992 21:02")
	   (authors PRCKLN)
	   (INPUT "'file' is a pathname")
	   (VALUE "the print list of this file, i.e. the list of top "
		  "level expressions on the file."))
  (IF (AND (PATHNAMEP file)
	   (OPEN FiLE :DIRECTION :PROBE))
      (LET ((*READTABLE* DOC*READTABLE-PRINT))
	(doc=READ-FILE FILE))
      (progn (cerror "Enter new pathname." "~A cannot be opened" file)
	     (DOC=GET-PRINT-LIST (read)))))

(DEFUN DOC=PRINT-FILE (file doc-stream doc-keys doc-type)
  (declare (edited  "14-AUG-1992 18:48")
	   (authors PRCKLN)
	   (INPUT  "FILE is a pathname."
		   "DOC-KEYS are the declare keys to be documented."
		   "DOC-TYPE is a keyword (:interface, :interface-tex, :simple-tex, :complete or :complete-tex).")
	   (EFFECT "Prints information of declare statements of file FILE on the DOC-STREAM."
		   "DOC-TYPE controls which functions etc. will be documented"))
  (let ((current-package *package*))
    (unwind-protect
	(progn (in-package "USER")
	       (case doc-type
		 ((:interface :complete)
		  (DOCA~PRINT-FUNCTIONS (doc=get-print-list file) doc-stream doc-keys doc-type))
		 ((:interface-tex :simple-tex :complete-tex)
		  (DOCL~print-functions file (doc=get-print-list file) doc-stream doc-keys doc-type))))
      (setq *package* current-package))))

(DEFUN DOC=FILE (pathname EXPAND-ON
			     DOC-STREAM DOC-KEYS doc-type)
  (declare (edited  "14-AUG-1992 17:53")
	   (authors PRCKLN)
	   (input "PATHNAME is a pathname."
		  "DOC-TYPE is a keyword (:INTERFACE, :SIMPLE-TEX, :INTERFACE-TEX, :COMPLETE or :COMPLETE-TEX)."
		  "DOC-STREAM is a stream open for output or nil."
		  "DOC-KEYS are list of additional keys used in declare.")
	   (effect "The amount of printed documentation depends on DOC-TYPE."))
  (let (DOC*EXPAND-FLAG stream)
    (DECLARE (SPECIAL DOC*EXPAND-FLAG))
    (UNWIND-PROTECT
	(PROGN
	  (SETQ DOC*EXPAND-FLAG EXPAND-ON)
	  (setq stream (DOC=OPEN-DOC-STREAM pathname DOC-STREAM doc-type))
	  (DOC=PRINT-FILE pathname stream DOC-KEYS doc-type))
      (close STREAM))))

(DEFUN DOC=DO (OBJECT EXPAND-ON DOC-PATHNAME DOC-KEYS
	       &OPTIONAL (DOC-TYPE :INTERFACE))
  (DECLARE (EDITED  "14-AUG-1992 14:32")
	   (AUTHORS PRCKLN)
	   (INPUT "OBJECT  is a pathname, a string (corresponding to a pathname), or a symbol (the name of a file)."
		  "DOC-TYPE is a keyword (:interface, :interface-tex, :simple-tex, :complete, or :complete-tex)."
		  "DOC-PATHNAME is a pathname or nil."
		  "DOC-KEYS are list of additional keys used in declare.")
	   (EFFECT "if DOC-ON is T, then the amount of printed documentation depends on DOC-TYPE.")
	   (VALUE  "Undefined."))
  (let ((file (DOC=GET-PATHNAME OBJECT))
	(doc-stream nil))
    (when doc-pathname (SETQ DOC-PATHNAME (DOC=GET-PATHNAME doc-pathname)))
    (UNWIND-PROTECT
	(progn (WHEN DOC-PATHNAME
		 (SETQ DOC-STREAM (OPEN DOC-PATHNAME :DIRECTION :OUTPUT
					:if-exists :supersede
					:if-does-not-exist :create))) 
	       (DOC=file file EXPAND-ON DOC-STREAM DOC-KEYS DOC-TYPE))
      (WHEN DOC-STREAM (CLOSE DOC-STREAM)))
    NIL))

(DEFUN DOC~EXTRACT (OBJECT &OPTIONAL
		    (DOC-TYPE :INTERFACE)
		    (PATHNAME NIL)
		    (DOC-KEYS NIL))
  "OBJECT  is a SYMBOL THE NAME OF A FILE, MODULE, OR SYSTEM.                   
                       DOC-TYPE : a keyword (:interface, :interface-tex, :simple-tex, :complete or :complete-tex)
                       PATHNAME is a pathname or nil.
                       DOC-KEYS is a list of additional keywords used in declare."
  (DECLARE (EDITED  "14-AUG-1992 14:29")
	   (AUTHORS PRCKLN)
	   (INPUT  "     OBJECT  is a pathname, a string (corresponding to a pathname), or a symbol (the name of a module)."
		   "     DOC-TYPE is a keyword (:interface, :interface-tex, :complete, or :complete-tex)"
		   "     PATHNAME is a pathname or a string (corresponding to a pathname)."
		   "     DOC-KEYS is a list of additional keywords used in declares.")
	   (EFFECT "     Writes the documentation of OBJECT into the file associated    "
		   "     with PATHNAME or into files associated with each file of the documented module"
                   "     if PATHNAME is nil. When printing in LaTeX format :INTERFACE-TEX or :COMPLETE-TEX"
		   "     the preamble is printed into all created files."
		   "     If DOC-TYPE is :INTERFACE, :SIMPLE-TEX, or :INTERFACE-TEX, then only external"
		   "     functions will be documented,"
		   "     if it is :complete, all functions will be documented, the suffix of the keywords determine"
		   "     the printed format."
		   "     Comments can be declared as :LISP or :LATEX comments by enclosing them with comment"
		   "     lines containing BEGIN{LISP} and END{LISP} or with BEGIN{LATEX} and"
		   "     END{LATEX}, respectively. :lisp comments will only be printed on .doc files and :latex comments"
		   "     will only be printed on .tex files. Each begin should match a corresponding end. Lisp"
		   "     definitions (DEFUN, DEFSTRUCT etc.) also end the scope of a BEGIN. If the comments are"
		   "     not declared in this way, then they are printed always. But with some treatment of special"
		   "     characters like ., <, _ etc. when printing latex acceptable output. This special treatment"
		   "     is not done for :LATEX comments. Therefore :LATEX comments should only consist of text"
		   "     which could directly understood by LaTeX."))
  (CCASE DOC-TYPE
    ((:INTERFACE :INTERFACE-TEX :COMPLETE :COMPLETE-TEX :SIMPLE-TEX)
     NIL))					      ; check on correct input
  (DOC=DO OBJECT T PATHNAME DOC-KEYS DOC-TYPE))


