;;; -*- syntax: common-lisp; package: ags; base: 10; mode: lisp -*-
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

(in-package :ags)


(mod~defmod ch :uses (mod)
	       :documentation "Checking of the module structure of a module system."
	       :exports (ch~check-system ch~check-module-file
					 ch~check-module-structure))

#{\section{Checking Module Consistency}
\label{mod:ch}
This is a collection of functions that check the consistency of the module system as
 defined in \ref{module-conventions}.
#}



(defun ch~check-module-file(file &optional (outputfile nil))
    (declare (edited  "10-AUG-1992 18:42")
	   (authors KERBER)
	   (input   "A file corresponding to the module."
		    "Optionally an outputfile.")
	   (effect  "If the module conventions are disregarded warning are produced and written on outputfile."
		    "If outputfile exists: error.")
	   (value   "t iff the module conventions are regarded.")
	   (example "(ch~check-module-file \"/home/omega/sys/prog/check-modules.lisp\") "))
    (if outputfile
	(with-open-file 
	    (outputstream outputfile :direction :output :if-exists :error)
	  (ch=check-module-file file outputstream))
	(ch=check-module-file file *standard-output*)))
      
(defun ch=check-module-file(file &optional (outputstream *standard-output*))
  (declare (edited  "28-JUL-1992 12:03")
	   (authors KERBER)
	   (input   "A file corresponding to the module."
		    "Optionally an outputstream")
	   (effect  "If the module conventions are disregarded warning are produced")
	   (value   "t iff the module conventions are regarded."))
  (let* ((moduledecl (ch=read-modules file outputstream)) ;the module declaration is searched
	 (modulename (ch=string-coerce (second moduledecl))) ;the modulename of the current module
	 (modules    (cons modulename (mapcar #'ch=string-coerce (ch=search-key moduledecl :uses)))
					;the modulename of the current module is added to the list of allowed modules
	   ))
    (if moduledecl
	(let ((bool t))
	  (with-open-file (stream file :direction :input)
	    (setq bool (and (ch=check-in-package stream modulename outputstream) bool)))
	  (with-open-file (stream file :direction :input)
	    (do ((newread (read stream nil "END_OF_FILE_REACHED") (read stream nil "END_OF_FILE_REACHED")))
		((equal newread "END_OF_FILE_REACHED") (if bool t nil))
	      (setq bool (and (ch=check-occurences newread modules modulename outputstream) bool))
					;order in ``and'' important, that ch=check-occurences is evaluated
	      (setq bool (and (ch=check-defun newread modulename outputstream) bool))
					;order in ``and'' important, that ch=check-defun is evaluated
	      (setq bool (and (ch=check-defvar newread modulename outputstream) bool))
					;order in ``and'' important, that ch=check-defvar is evaluated
	      (setq bool (and (ch=check-defclass newread modulename outputstream) bool))
					;order in ``and'' important, that ch=check-defclass is evaluated
					;defmethod is not checked, since here all modules in modules are allowed, hence
					;already checked in ch=check-occurences
	      (setq bool (and (ch=check-load newread modulename outputstream) bool))
					;order in ``and'' important, that ch=check-defclass is evaluated
	      )))
	nil)))

(defun ch~check-module-structure(modulestructure &optional (outputfile nil))
  (declare (edited  "07-AUG-1992 16:20")
	   (authors KERBER)
	   (input   "a modulestructure as produced by ch=list-modules"
		    "an outputfile")
	   (effect  "warnings iff the structure is not ok. Error if outputfile exists.")
	   (value   "t iff every module uses only already known modules and no module is introduced twice.")
	   (example "(ch~check-module-structure (append *ags-modules* *keim-modules*))"))
  (if outputfile
      (with-open-file 
	  (outputstream outputfile :direction :output :if-exists :error)
	(ch=check-module-structure modulestructure outputstream))
      (ch=check-module-structure modulestructure *standard-output*)))

(defun ch=check-module-structure(modulestructure outputstream)
  (declare (edited  "07-AUG-1992 16:20")
	   (authors KERBER)
	   (input   "a modulestructure as given by the global variable *keim-modules*"
		    "an outputstream")
	   (effect  "warnings if the structure is not ok.")
	   (value   "t iff every module uses only already known modules and no module is introduced twice.")
	   (example "(ch=check-module-structure (ch=list-modules (append *ags-modules* *keim-modules*)) *standard-output*)"))
  (let ((modules (ch=list-modules modulestructure)))
    (do ((bool t)
	 (known-modules nil             (cons actual-module known-modules))
	 (rest-modules  (rest modules) (rest rest-modules))
	 (actual-module (ch=module-structure-module (first modules))
			(ch=module-structure-module (first rest-modules)))
	 (actual-used-modules (ch=module-structure-used-modules (first modules))
			      (ch=module-structure-used-modules (first rest-modules))))
	((null rest-modules) (if bool t nil))
      (cond ((member actual-module known-modules)
	     (format outputstream "Error: module ~A is already defined.~%" 
		     actual-module (setq bool nil)))
	    ((not (subsetp actual-used-modules known-modules))
	     (format outputstream "Error in module ~A, not introduced module(s) ~A used.~%"
		     actual-module (set-difference actual-used-modules known-modules))
	     (setq bool nil))
	    (t t)))))

(defun ch=check-in-package(stream modulename outputstream)
  (declare (edited  "05-AUG-1992 12:27")
	   (authors KERBER)
	   (input   "a stream, a file, and a outputstream")
	   (effect  "a warning iff the first element of the stream is not an in-package list")
	   (value   "t iff the first element in the stream is an in-package list"))
  (let ((first-expr (read stream nil "END_OF_FILE_REACHED")))
    (if (or (equal first-expr "END_OF_FILE_REACHED")
	    (atom first-expr)
	    (not (and (atom (first first-expr)) (string-equal (ch=string-coerce (first first-expr)) "IN-PACKAGE"))))
	(progn (ch=warning outputstream modulename "no \"IN-PACKAGE\" found.") nil)
	t)))

(defun ch=check-load (exp modulename outputstream)
  (declare (edited  "05-AUG-1992 11:22")
	   (authors KERBER)
	   (input   "an expression, a modulename, and a stream")
	   (effect  "a warning if the expression is a list beginning with a load")
	   (value   "t iff expression is not a list beginning with a load"))
  (cond ((atom exp) t)
	((listp exp) (if (string-equal (first exp) "load")
			 (progn (ch=warning outputstream modulename "no \"load\" allowed.") nil)
			 t))
	(t t)))

(defun ch=read-modules(file &optional (outputstream *standard-output*))
  (declare (edited  "28-JUL-1992 17:32")
	   (authors KERBER)
	   (input   "a file of a module structure")
	   (effect  "warning if no mod~defmod expression on file")
	   (value   "the mod~defmod expression on the file."))
  (with-open-file (stream file :direction :input)
    (do* ((newread (read stream nil "END_OF_FILE_REACHED") (read stream nil "END_OF_FILE_REACHED"))
	  (bool (and (listp newread) (string-equal (ch=string-coerce (first newread)) "mod~defmod"))
		(and (listp newread) (string-equal (ch=string-coerce (first newread)) "mod~defmod"))))
	 ((or bool (equal newread "END_OF_FILE_REACHED"))
	  (if bool
	      newread
	      (progn (format outputstream "No mod~~defmod expression on file ~A . No further check.~%" file) nil))))))

(defun ch=check-occurences (exp mplist mp outputstream)
  (declare (edited  "28-JUL-1992 16:30")
	   (authors KERBER)
	   (input  "an expression, a list of module prefixes, one module prefix")
	   (effect "warnings if the expression has a module prefix not being allowed by mp (+ ~ = *) or mplist (+ ~)")
	   (value  "t iff the module prefix is allowed"))
  (ch=recursion-application exp #'(lambda(x)(ch=check-atom exp x (cons "" mplist) mp outputstream))))

(defun ch=check-defun (exp mp outputstream)
  (declare (edited  "03-AUG-1992 13:36")
	   (authors KERBER)
	   (input  "an expression, a module prefix and an outputstream" )
	   (effect "warning if it is a defun expression and it does not begin with mp= or mp~"
		   "if begins with mp~ a comment is necessary")
	   (value  "t iff it is no defun expression or the module prefixes are ok.")
	   (example "(ch=check-defun '(defun ab=c) \"ab\" *standard-output*) --> t "))
  (let ((bool t))
    (cond ((atom exp) t)
	  ((string-equal (ch=string-coerce (first exp)) "defun")
	   (multiple-value-bind (stringbeg stringrest character) (ch=check-char-in '(#\~ #\=) (ch=string-coerce (second exp)))
	     (declare (ignore stringrest))
	     (if (string-equal stringbeg mp)
		 (if (char= character #\~)
		     (setq bool (ch=check-comment exp mp outputstream)))
		 (progn (ch=warning outputstream mp "unallowed defun in expression ~S" exp) (setq bool nil)))))
	  (t t))
    (if bool t nil)))

(defun ch=check-comment(exp mp outputstream)
  (let ((dec (fourth exp))
	(inputok nil)
	(outputok nil))
    (if (and (listp dec) (string-equal (ch=string-coerce (first dec)) "declare") )
	(dolist (element dec) (progn (if (and (listp element)
					      (string-equal (ch=string-coerce (first element)) "input")
					      (stringp (second element))
					      (not (string= (second element) "")))
					 (setq inputok t))
				     (if (and (listp element)
					      (string-equal (ch=string-coerce (first element)) "value")
					      (stringp (second element))
					      (not (string= (second element) "")))
					 (setq outputok t)))))
    (if (and inputok outputok)
	t
	(progn (ch=warning outputstream mp "comment not okay in expression ~S" exp) nil))))
	
(defun ch=check-internal-function-name (atom mp outputstream)
  (declare (edited  "03-AUG-1992 11:09")
	   (authors KERBER)
	   (input "an atom, a moduleprefix, and an outputstream")
	   (effect "none")
	   (value  "t iff atom is of the form mp=... else nil.")
	   (ignore outputstream))
  (if (atom atom)
      (string-equal (ch=check-char-in '(#\=) (ch=string-coerce atom)) mp)
      nil))

(defun ch=check-defclass (exp mp outputstream)
  (declare (edited  "10-AUG-1992 16:09")
	   (authors KERBER)
	   (input   "an expression, a module prefix and an outputstream")
	   (effect  "warnings if defclass is not according to naming conventions.")
	   (value   "non-nil iff naming convention in defclass are observed."))
  (cond ((atom exp) t)
	((string-equal (ch=string-coerce (first exp)) "defclass")
	 (if (string-equal (ch=check-char-in '(#\+) (ch=string-coerce (second exp))) mp)
	     (eval (cons 'and (mapcar #'(lambda(element) (ch=check-defclass-slot-spec (second exp) element mp outputstream))
				      (fourth exp))))
	     (progn (ch=warning outputstream mp "unallowed defclass in expression ~S" exp) nil)))
	(t t)))

(defun ch=check-defclass-slot-spec(name exp mp outputstream)
  (declare (edited  "10-AUG-1992 16:10")
	   (authors KERBER)
	   (input   "the name of an expression, an expression, a module prefix, and an outputstream.")
	   (effect  "warnings if writer or reader are not internal function, warning iff accessor is used.")
	   (value   "t iff no accessor and writer as well as reader are internal functions."))
  (let ((writer (ch=search-key exp :writer))
	(reader (ch=search-key exp :reader))
	(accessor (ch=search-key exp :accessor))
	(bool t))
    (if (and writer (not (ch=check-internal-function-name writer mp outputstream)))
	(progn (ch=warning outputstream mp "unallowed writer ~A in expression ~A" writer name) (setq bool nil))
	bool)
    (if (and reader (not (ch=check-internal-function-name reader mp outputstream)))
	(progn (ch=warning outputstream mp "unallowed reader ~A in expression ~S" reader exp) (setq bool nil))
	bool)
    (if accessor
        (progn (ch=warning outputstream mp "accessor ~A not allowed in general in expression ~S" accessor exp) (setq bool nil)))
    bool))

(defun ch=search-key(list keyword)
  (declare (edited  "03-AUG-1992 10:55")
	   (authors KERBER)
	   (input   "a list and a keyword")
	   (effect  "none")
	   (value   "the expression after the keyword if in list, else nil")
	   (example "(ch=search-key '(a b c :d e :f g) :d) --> e "))
  (let ((pos (position keyword list)))
    (if pos (nth (1+ pos) list) nil)))

(defun ch=check-defvar (exp mp outputstream)
  (declare (edited  "10-AUG-1992 17:08")
	   (authors KERBER)
	   (input   "an expression, a moduleprefix and an outputstream.")
	   (effect  "warning if a variable is introduced by defvar not of the form mp*.....")
	   (value   "t iff no defvar expression or variable has name mp*...."))
  (cond ((atom exp) t)
	((string-equal (ch=string-coerce (first exp)) "defvar")
	 (if (string-equal (ch=check-char-in '(#\*) (ch=string-coerce (second exp))) mp)
	     t
	     (progn (ch=warning outputstream mp "unallowed defvar in expression ~S" exp) nil)))
	(t t)))

(defun ch=check-atom (exp atom mplist mp outputstream)
  (declare (edited  "10-JUL-1992 11:55")
	   (authors KERBER)
	   (input   "an atom (which is thought to contain one atom of a module)"
		    "a list of module prefixes which is checked that each occur in the expression only in
                     interface functions"
		    "the prefix of the module belonging to the expression, in the rest of the string only number, characters or #\-")
	   (effect  "none")
	   (value   "t iff the module prefixes occurs only in interface functions and no others"
		    "and only the module prefix of the module itself occurs in other functions."))
  (if (or (stringp atom) (characterp atom))
      t
      (multiple-value-bind (stringbeg stringrest character) (ch=check-char-in '(#\~ #\= #\* #\+) (ch=string-coerce atom))
	(cond ((or (null character) (string= stringrest "") (string= stringbeg "")) t)
	      ((member stringbeg mplist :test #'string-equal)
	       (if (ch=allowed-simple-name stringrest character)
		   t
		   (progn (ch=warning outputstream mp "unallowed atom ~A in expression ~S"
				      atom
				      exp)
			  nil)))
	      (t (progn (ch=warning outputstream mp "unallowed atom ~A in expression ~S"
				    atom
				    exp)
			nil))))))

(defun ch=recursion-application(exp proc)
  (declare (edited  "31-JUL-1992 13:46")
	   (authors KERBER)
	   (input   "an expression and a procedure")
	   (effect  "the procedure is recursively applied to all sublist and sublist of these")
	   (value   "conjunction of all results"))
  (let ((bool t))
    (cond ((atom exp) (funcall proc exp))
	  ((null exp) t)
	  (t (setq bool (and (ch=recursion-application (first exp) proc) bool))    ;order important so that evaluation is garanteed
	     (setq bool (and (ch=recursion-application (rest exp) proc) bool)))))) ;order important so that evaluation is garanteed

(defun ch=allowed-simple-name(inputstring &optional (character nil))
  (declare (edited  "31-JUL-1992 16:18")
	   (authors KERBER)
	   (input   "a string and a character")
	   (effect  "none")
	   (value   "t iff the string consists only of digits, letters or #\-."
		    "If the character is #\= or #\~,  the string can have as first character"
		    "another #\= too. 
#\! are allowed in any number if they are rightmost. nil if emptystring or only #\!s."))
  (let ((string (string-right-trim '(#\!) inputstring)))
    (cond ((string= string "") nil)
	  ((not character)
	    (every #'ch=standard-char string))
	  ((char= character #\=)
	   (and (or (char= (aref string 0) #\=)
			 (ch=standard-char (aref string 0)))
		     (every #'ch=standard-char (subseq string 1))))
	  ((char= character #\~)
	   (and (or (char= (aref string 0) #\=)
		    (ch=standard-char (aref string 0)))
		(every #'ch=standard-char (subseq string 1))))
	  (t (every #'ch=standard-char string)))))
  
(defun ch=standard-char(char)
  (declare (edited  "03-AUG-1992 10:24")
	   (authors KERBER)
	   (input   "a character")
	   (effect  "none")
	   (value   "T iff char alphanumeric or equal to #\-"))
  (or (alphanumericp char)
      (char= char #\-)))

(defun ch=check-char-in(characterlist string)
  (declare (edited  "10-JUL-1992 15:27")
	   (authors KERBER)
	   (input   "a list of characters (thought of as the special character ~ = * -) "
		    "an arbitrary string")
	   (effect  "none")
	   (value   "Three values are returned.
                    the emptystring if char does not occur in string, else string of prefix exclusive this character"
		    "second value the rest of the string, excl character"
		    "third value the character")
	   (example "(ch=check-char-in '(#\~ #\*) \"ab~cd\") --> \"ab\" & \"cd\" & #\~ "
		    "(ch=check-char-in '(#\~ #\*) \"abcd\")  --> \"\"   & \"abcd\" & nil "))
  (let ((pos (position-if #'(lambda(x) (member x characterlist :test #'char=)) string)))
    (if pos
	(values (subseq string 0 pos)
		(subseq string (1+ pos))
		(aref string pos))
	(values "" string nil))))

(defun ch=string-coerce(atom)
  (declare (edited  "10-JUL-1992 15:06")
	   (authors KERBER)
	   (input   "an atom")
	   (effect  "none")
	   (value   "corresponding string")
           (example "(ch=string-coerce 'a) --> \"A\" "))
 (format nil "~S" atom))

(defstruct (ch=module-structure (:constructor ch=make-module-structure)) module used-modules)


(defun ch=warning(outputstream modulename errortext &rest more-error-arguments)
  (declare (edited  "14-AUG-1992 19:22")
	   (authors KERBER)
	   (input   "an outputstream, a string (for a modulname) the errortext"
		    "and an arbitrary number of arguments of the errortext in format syntax")
	   (effect  "prints warnings to the outputstream")
	   (value   "not of interest")
	   (example "(ch=warning *standard-output* \"ch\" \"moduleprefix ~A not allowed in function ~A\" 'bla 'fct)"))
  (let ((*print-length* 3)) ; give just enough of a form to show what it is
    (format outputstream "Warning in module ~6A " modulename)
    (apply #'format (cons outputstream (cons errortext more-error-arguments)))
    (terpri outputstream)))

; define interface to defsystem operations.

(mk::component-operation :check-system     'ch=check-mod-operation)
(mk::component-operation 'check-system     'ch=check-mod-operation)

(defun ch=check-mod-operation (component force)
  (declare (ignore force))
  (let ((source-pname (mk::component-full-pathname component :source)))
    (ch=check-module-file source-pname)))

(defun ch~check-system (system &optional output-file)
  (declare 
   (edited  "20-JUL-1993 17:25")
   (authors NESMITH)
   (input   "A system name (symbol or string) and optional filename")
   (effect  "The files in the given system will be checked.
             The warnings will be written to the specified outputfile.  
             If none is specified, the warnings will be written to *standard-output*, error if outputfile exists.")
   (value   "t iff system has correct module structure and in each module the naming conventions are observed")
   (example "{\vb (ch~check-system 'keim \"/home/omega/error-messages\")}"))
  (if output-file
      (with-open-file (*standard-output* output-file :direction :output
					 :if-does-not-exist :create
					 :if-exists :error)
	(mk:operate-on-system system :check-system :verbose t))
    (mk:operate-on-system system :check-system :verbose t)))
