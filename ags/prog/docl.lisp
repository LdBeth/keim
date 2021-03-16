;;; -*- Syntax: Common-lisp; Mode: LISP; Package: AGS; Base: 10 -*-
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

(mod~defmod docl :uses (mod doc)
	    :documentation "Documentation in the LaTeX format"
	    :exports (docl~separators DOCL~PRINT-FUNCTIONS))
#{\section{The LaTeX Format}

\label{mod:docl}
 This module contains functions to document functions in \LaTeX\ format.
 The functions correspond to the functions in the ASCII module {\tt DOCA}.
 The prefix is {\tt DOCL} for Documentation-Latex.
#}

(defvar docl*separators nil
  "List for separating of complicated identifiers. A list of two element lists (from to).")

(defun docl~separators (seps)
  (declare (edited  "17-SEP-1992 22:17")
	   (authors PRCKLN)
	   (input   "SEPS is a list of two element lists (FROM TO)."
		    "They are converted to be understood by DOCL=REPLACE.")
	   (example "FROM:  DOCL=LONGUNSEPARABLE-IDENTIFIER."
		    "TO:    DOCL=LONG\\-UN\\-SE\\-PARABLE-IDEN\\-TIFIER.")
	   (REMARK  "Slows down the extraction process if the list is long."
		    "The function must be called before executing DOC-EXTRACT.")
	   (effect  "Informs the DOCL module about the specified separators.")
	   (value   "Undefined."))
  (labels ((conv (seps) (if seps
			    (cons (list (docl=symbol-latex (caar seps) nil) (symbol-name (cadar seps)))
				  (conv (rest seps)))
			    nil)))
    (setq docl*separators (conv seps))))


(defun docl=print-cassoc (key decls)
  (rest (ASSOC (symbol-name key) DECLS :test #'string= :key #'symbol-name)))

(DEFUN DOCL=GET-USAGE (FUNCTION)
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

(DEFUN DOCL=SYS-DECLARATIONS (BODY)
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

(defun DOCL=documentation-strings (body)
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

(DEFUN DOCL=REMASSOC (KEY ALIST)
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
		(DOCL=REMASSOC key (CDR ALIST2))
		nil))))
  ALIST)

(DEFVAR DOCL*COMMENT-FORMAT :BOTH
	;; the variable is used to control the printing of comments. If it is :lisp, only :lisp comments will be printed,
	;; if it is :latex, only :latex comments will be printed, if it is :both, all comments are printed.
	":lisp, :latex or :both")


(DEFUN DOCL~PRINT-FUNCTIONS (file info DOC-STREAM DOC-KEYS DOC-TYPE)
  (DECLARE (EDITED  "14-AUG-1992 18:59")
	   (AUTHORS PRCKLN DEISS)
	   (INPUT  "FILE is a pathname."
		   "DOC-STREAM is the stream where the information is printed."
		   "DOC.KEYS are the declare keys to be documented."
		   "DOC-TYPE is a keyword (:simple-tex, :interface-tex or :complete-tex),"
		   "controlling which functions are documented.")
	   (EFFECT "prints information from INFO on DOC-STREAM.")
	   (VALUE  "Undefined."))
  ;; the following part of code corresponds to the function DOCA=PRINT-DOC-TOP
  (UNLESS (EQ DOC-TYPE :SIMPLE-TEX)
    (LET ((FILE-NAME (PATHNAME-NAME FILE)))
      (DOCL=PRINT-PREAMBLE-LATEX DOC-STREAM)
      (FORMAT DOC-STREAM
	      "\\begin{document}~%\\pagenumbering{arabic}~2%")
      (FORMAT DOC-STREAM
	      "~2&\\title{~A}~%\\author{ }~%\\maketitle~%"
	      FILE-NAME)))
  ;; end of the corresponding part.
  (DOCL=PRINT-DEFINITIONS-LATEX info DOC-STREAM DOC-KEYS DOC-TYPE)
  (UNLESS (EQ DOC-TYPE :SIMPLE-TEX)
    (FORMAT DOC-STREAM "~%\\end{document}")))
   
(DEFPARAMETER DOCL*PREAMBLE 
  '("\\documentstyle[twoside]{report}"
    "\\pagestyle{myheadings}"
    ""
    "\\hoffset0cm"
    "\\textheight23.5cm"			;pageformat
    "\\textwidth17cm"
    ""
    "\\spaceskip=3.33pt plus 3.34pt minus 1.11pt"
    "\\tolerance=250"
    ""
    "\\oddsidemargin0.5cm"			;margins
    "\\evensidemargin-0.4cm"
    "\\topmargin-10mm"
    ""
    "\\parskip1ex"				;paragraph distances
    "\\parindent0cm"
    "\\tabcolsep5mm"
    "\\raggedbottom"
    "\\newcommand\\arrowline[4]{\\mbox{\\parbox[t]{#1}{#2\\vphantom{/y}}}"
    "  \\hfill\\rightarrow\\hfill"
    "  \\mbox{\\parbox[t]{#3}{#4\vphantom{/y}}}\vspace*{1mm}}"
    ""
    "\\newlength{\\argumentwidth}"		; length to format definition of a function etc.
    "\\newlength{\\minargwidth}"		; minimum amount of argument width, if there is less space, then
    "\\setlength{\\minargwidth}{15em}"		; the arguments are printed below the name and type of the defun etc.
    "\\newlength{\\namewidth}"
    "\\newlength{\\typewidth}"
    ""
    "\\newenvironment{lispdefinition}[3]%"	;environment lispdefinitions
    "   {\\goodbreak\\removelastskip\\bigskip"	;arguments : type name lambdalist
    "    \\index{#2}"
    "    \\settowidth{\\namewidth}{\\bf \\mbox{#2}}"
    "    \\settowidth{\\typewidth}{[#1]}"
    "    \\setlength{\\argumentwidth}{\\textwidth}"
    "    \\addtolength{\\argumentwidth}{-1\\namewidth}"
    "    \\addtolength{\\argumentwidth}{-1\\typewidth}"
    "    \\addtolength{\\argumentwidth}{-2em}"
    "    \\ifdim\\argumentwidth<\\minargwidth%"	; not enough space for printing arguments in the same row as name and type
    "           \\ifdim\\argumentwidth<-1.5em%"	; not enough space for name and type in one row
    "                  \\setlength{\\argumentwidth}{\\textwidth}%"
    "                  \\addtolength{\\argumentwidth}{-7em}%"
    "                  \\begin{bf}#2\\end{bf}\\newline%"
    "                  \\hspace*{\\fill}[#1]\\newline%"
    "                  \\hspace*{\\fill}\\parbox[t]{\\argumentwidth}{\\raggedright #3}%"
    "             \\else%"			; name and type in one row, arguments below them
    "                  \\setlength{\\argumentwidth}{\\textwidth}%"
    "                  \\addtolength{\\argumentwidth}{-7em}%"
    "                  \\begin{bf}#2\\end{bf}\\hspace{\\fill}[#1]\\newline%"
    "                  \\hspace*{\\fill}\\parbox[t]{\\argumentwidth}{\\raggedright #3}%"
    "             \\fi"
    "       \\else%"				; name, type and arguments in one row.
    "           \\begin{bf}#2\\end{bf}\\hspace{1em}\\parbox[t]{\\argumentwidth}{\\raggedright #3}\\hspace{1em}[#1]%"
    "       \\fi"
    "    \\begin{lispdeclarations}}%"
    "   {\\end{lispdeclarations}"
    "    \\removelastskip\\bigskip}"
    ""
    "\\newenvironment{lispdeclarations}%"	;environment lispdeclarations
    "   {\\begin{list}%"
    "           {}%"
    "           {\\itemsep1ex"
    "            \\topsep0ex"
    "            \\partopsep0mm"
    "            \\parsep0ex"
    "            \\leftmargin8em"
    "            \\rightmargin1.5em"
    "            \\labelwidth7em"
    "            \\labelsep1em}}%"
    "   {\\end{list}}"
    ""
    "\\newlength{\\slotoptionwidth}"
    "\\newlength{\\optionwidth}"
    "\\newenvironment{structure}[1]%"		; environment structure
    "    {\\parskip0ex"
    "     \\goodbreak"
    "     \\removelastskip"
    "     \\bigskip"
    "     \\begin{bf}#1\\end{bf}\\hspace*{\\fill}[Structure]"
    "     \\newline}%"
    "    {\\removelastskip"
    "     \\bigskip}"
    ""
    "\\newenvironment{structureoptions}%"	; environment structureoptions
    "    {\\hspace*{2em} options:"
    "     \\begin{list}%"
    "            {}%"
    "            {\\topsep0ex"
    "             \\partopsep0ex"
    "             \\parsep0ex"
    "             \\itemsep0ex"
    "             \\settowidth{\\labelwidth}{:additional-functions}"
    "             \\labelsep1em"
    "             \\setlength{\\leftmargin}{\\labelwidth}"
    "             \\addtolength{\\leftmargin}{4em}"
    "             \\addtolength{\\leftmargin}{\\labelsep}"
    "             \\rightmargin3em"
    "             \\setlength{\\optionwidth}{\\textwidth}"
    "             \\addtolength{\\optionwidth}{-\\rightmargin}"
    "             \\addtolength{\\optionwidth}{-\\leftmargin}}}%"
    "    {\\end{list}}"
    ""
    "\\newenvironment{structureslots}%"		; environment structureslots
    "    {\\goodbreak"
    "     \\hspace*{2em} slots:"
    "     \\begin{list}%"
    "            {}%"
    "            {\\topsep0ex"
    "             \\partopsep0ex"
    "             \\parsep0ex"
    "             \\itemsep0ex"
    "             \\setlength{\\labelwidth}{10em}"
    "             \\labelsep1em"
    "             \\setlength{\\leftmargin}{\\labelwidth}"
    "             \\addtolength{\\leftmargin}{4em}"
    "             \\addtolength{\\leftmargin}{\\labelsep}"
    "             \\rightmargin3em}}%"
    "    {\\end{list}}"
    ""
    "\\newenvironment{slotoptions}%"		; environment slotoptions
    "    {\\setlength{\\slotoptionwidth}{\\textwidth}"
    "     \\addtolength{\\slotoptionwidth}{-\\rightmargin}"
    "     \\addtolength{\\slotoptionwidth}{-\\leftmargin}"
    "     \\begin{minipage}[t]{\\slotoptionwidth}"
    "          \\raggedright"
    "          \\begin{list}%"
    "                 {}%"
    "                 {\\settowidth{\\labelwidth}{:parameter-access}"
    "                  \\labelsep1em"
    "                  \\setlength{\\leftmargin}{\\labelwidth}"
    "                  \\addtolength{\\leftmargin}{\\labelsep}"
    "                  \\rightmargin0em}}%"
    "    {     \\end{list}"
    "     \\end{minipage}}"
    ""
    "\\newenvironment{structuredocs}%"		; environment structuredocs
    "    {\\goodbreak"
    "     \\hspace*{2em} documentation:\\newline"
    "     \\addtolength{\\textwidth}{-4em}"
    "     \\addtolength{\\textwidth}{-\\rightmargin}"
    "     \\hspace*{4em}\\begin{minipage}[t]{\\textwidth}}%"
    "    {              \\end{minipage}}"

    ))

(DEFUN DOCL=PRINT-PREAMBLE-LATEX (DOC-STREAM)
  (DECLARE (EDITED  "14-AUG-1992 19:22")
	   (AUTHORS PRCKLN "deiss")
	   (INPUT "DOC-STREAM is a stream, open for output.")
	   (VALUE "undefined")
	   (EFFECT "The preamble for LATEX documentation files is printed on DOC-STREAM."))
  (FORMAT DOC-STREAM "~{~A~%~}" DOCL*PREAMBLE)
  )

;;;
;;; The following two functions convert symbols and strings to strings accepted by LaTeX.
;;; The problem are characters which are constituent in Lisp, but special characters in LaTeX.
;;; In symbols the following characters are protected by:
;;;begin{lisp}
;;; $ # _        printed with the corresponding LaTeX commands
;;; ^            printed as accent
;;; #\\          $#\\backslash$
;;; ! ? . :      $!$ etc.
;;; < >          $<$ etc.

;;; In Strings the same characters are protected, but arrows and underscore are treated in
;;; a special manner.
;;; An underscore initiates an underscoring in LaTeX if it is preceded by a symbol and followed by an integer.
;;; An arrow is something of the following forms:
;;; <->, <-, ->.  More than one - are also accepted. = may be used instead of -.
;;; <> is interpreted as unequal sign. The arrows must be enclosed with blanks. In all other cases
;;; the characters are treated in the same ways as if they were in symbols.
;;;end{lisp}

;;;begin{latex}
;;; \begin{tabbing}
;;; \hspace*{8em}\=\kill
;;; \$ \# \_     \>printed with the corresponding LaTeX commands\\
;;; \^{\ }       \>printed as accent\\
;;; $\backslash$ \>\$$\backslash$\$\\
;;; ! ? . :      \>\$!\$ etc.\\
;;; $<$ $>$      \>\$$<$\$ etc.
;;; \end{tabbing}
;;; In Strings the same characters are protected, but arrows, underscore and caret are treated in a special
;;; manner. An underscore initiates a subscript in LaTeX if it is preceded by a symbol and followed by
;;; an integer, caret is treated in a corresponding manner. An arrow is something of the following forms:
;;; -$>$ $<$-$>$ $<$-. More than one - are also accepted and = may be used instead of -. $<$$>$ is
;;; interpreted as $\neq$. In all other cases the
;;; characters are treated in the same way as if they were in symbols.
;;;end{latex}
;;;
;;; 

(DEFPARAMETER DOCL*SF-QUEUE "\\protect\\(\\sim\\protect\\)")

(DEFUN DOCL=SYMBOL-LATEX (SYMBOL &OPTIONAL (MBOX T))
  (DECLARE (EDITED  "14-AUG-1992 18:58")
	   (AUTHORS DEISS)
	   (INPUT   "a symbol.")
	   (EFFECT  "none")
	   (VALUE   "a string corresponding to the printname of the symbol, but the special characters"
		    "are protected- They are protected in a form accepteble by LaTeX. The symbol is put"
		    "in an mbox iff MBOX is true."))
  (LET* ((POINTER 0)
	 (SYMBOL-STRING (FORMAT NIL "~S" SYMBOL))
	 (END (LENGTH SYMBOL-STRING))
	 )
    (WITH-OUTPUT-TO-STRING (STRING-STREAM)
      (IF MBOX (FORMAT STRING-STREAM "\\mbox{"))
      (DO ((POS T)
	   )
	  ((OR (EQL POINTER END)
	       (NOT POS))
	   (UNLESS (EQL POINTER END)		;print the substring after the last special character
	     (FORMAT STRING-STREAM "~A" (SUBSEQ SYMBOL-STRING POINTER END)))
	   )
	(SETQ POS (POSITION-IF #'(LAMBDA (CHAR)	;first occurence or nil 
				 (MEMBER CHAR '(#\& #\~ #\$ #\_ #\^ #\# #\! #\? #\. #\: #\< #\>) :TEST #'CHAR=))
			     SYMBOL-STRING :START POINTER :END END))
	(WHEN POS	    
	  (FORMAT STRING-STREAM "~A" (SUBSEQ SYMBOL-STRING POINTER POS))
	  (SETQ POINTER (1+ POS))
	  (CASE (CHAR SYMBOL-STRING POS)
	    ((#\& #\$ #\_ #\#) (FORMAT STRING-STREAM "~C~C" #\\ (CHAR SYMBOL-STRING POS)))
	    (#\^ (FORMAT STRING-STREAM "\\^{\\ }"))
	    (#\~ (FORMAT STRING-STREAM DOCL*SF-QUEUE)) ;"\\raisebox{-1.2mm}{\\mbox{\\~~{}}}"
	    ((#\! #\? #\. #\:) (FORMAT STRING-STREAM "$~C$" (CHAR SYMBOL-STRING POS)))
	    ((#\< #\>) (FORMAT STRING-STREAM "$~C$" (CHAR SYMBOL-STRING POS)))
	    ))
	)
      (IF MBOX (FORMAT STRING-STREAM "}"))		; closing mbox
      )))

(defun docl=replace (plist st)
  (declare (edited  "10-SEP-1992 19:00")
	   (authors HADES)
	   (input   "A list of string pairs and a string ST.")
	   (effect  "Replace pairwise the strings of PLIST in ST.")
	   (value   "The new string."))
  (if (null plist)
      st
      (let ((res (search (first (first plist)) st)))
	(if res
	    (format nil "~A~A~A"
		    (docl=replace (rest plist) (subseq st 0 res))
		    (second (first plist))
		    (docl=replace plist (subseq st (+ res (length (first (first plist)))))))
	    (docl=replace (rest plist) st)))))

(DEFUN DOCL=STRING-LATEX (STRING)
  (DECLARE (EDITED  "14-AUG-1992 18:57")
	   (AUTHORS PRCKLN DEISS)
	   (INPUT   "a string")
	   (EFFECT  "none")
	   (VALUE   "a string which corresponds to the input string, but will be accepted by LaTex. Some special"
		    "characters will treated as described above."))
  (docl=replace (append docl*separators '(("i$.$e." "i.e.\\") ("e$.$g." "e.g.\\") ("i. e." "i.e.\\") ("e. g." "e.g.\\"))) 
  (LET ((POINTER 0)
	(END (LENGTH STRING))
	)
    (IF (STRING= "" STRING)
	""
	(WITH-OUTPUT-TO-STRING (STRING-STREAM)
	  (DO ((POS T)
	       CHAR
	       )
	      ((OR (EQL POINTER END)
		   (NOT POS))
	       (UNLESS (EQL POINTER END)	;print the substring after the last special character
		 (FORMAT STRING-STREAM "~A" (SUBSEQ STRING POINTER END)))
	       )
	    (SETQ POS (POSITION-IF #'(LAMBDA (CHAR)	;first occurence or nil 
				       (MEMBER CHAR '(;#\\ 
						      #\# #\&  #\$ 
						      #\_ #\~ #\^ #\! #\? #\. #\: #\< #\> #\- #\=) :TEST #'CHAR=))
				   STRING :START POINTER :END END))
	    (WHEN POS
	      (FORMAT STRING-STREAM "~A" (SUBSEQ STRING POINTER POS))
	      (SETQ POINTER POS
		    CHAR (CHAR STRING POS))
	      (COND ;((CHAR= CHAR #\\) (FORMAT STRING-STREAM "$\\backslash$") (INCF POINTER))
		    ((MEMBER CHAR '(#\# #\& #\$) :TEST #'CHAR=)
		     (FORMAT STRING-STREAM "\\~C" CHAR)
		     (INCF POINTER))
		    ((MEMBER CHAR '(#\~) :TEST #'CHAR=)
		     (FORMAT STRING-STREAM DOCL*SF-QUEUE)  ;"\\raisebox{-1.2mm}{\\mbox{\\~~{}}}"
		     (INCF POINTER))
		    ((MEMBER CHAR '(#\! #\? #\. #\:) :TEST #'CHAR=)
		     (IF (OR (EQL END (1+ POS))
			     (CHAR= (CHAR STRING (1+ POS)) #\sPACE))
			 (FORMAT STRING-STREAM "~C" CHAR)	;it's an ordinary punctuation character
			 (FORMAT STRING-STREAM "\\(~C\\)" CHAR))
		     (INCF POINTER))
		    ((CHAR= CHAR #\_)
		     (COND ((EQL (1+ POS) END)	; underscore at end of string can't be beginning of subscript
			    (FORMAT STRING-STREAM "\\_")
			    (INCF POINTER))
			   ((EQL POS 0)		;underscore at begin of line can't be the beginning of subscript
			    (FORMAT STRING-STREAM "\\_")
			    (INCF POINTER))
			   ((ALPHA-CHAR-P (CHAR STRING (1- POS)))	; a symbol must precede the begin of subscript
			    (LET (NUMBER)
			      (MULTIPLE-VALUE-SETQ (NUMBER POS)
				(PARSE-INTEGER STRING :START (1+ POINTER) :END END :JUNK-ALLOWED T :radix 36))
			      (IF NUMBER
				  (PROGN (FORMAT STRING-STREAM "\\(_~36R\\)" NUMBER)	; a number must follow
					 (SETQ POINTER POS))
				  (PROGN (FORMAT STRING-STREAM "\\_")	; if no number follows print an underscore
					 (INCF POINTER))
				  )
			      ))
			   (T (FORMAT STRING-STREAM "\\_")
			      (INCF POINTER))))
		    ((CHAR= CHAR #\^)
		     (COND ((EQL (1+ POS) END)	; caret at end of string can't be beginning of superscript
			    (FORMAT STRING-STREAM "\\^{\\ }")
			    (INCF POINTER))
			   ((EQL POS 0)		; caret at begin of line can't be the beginning of superscript
			    (FORMAT STRING-STREAM "\\^{\\ }")
			    (INCF POINTER))
			   ((ALPHA-CHAR-P (CHAR STRING (1- POS)))	; a symbol must precede the begin of superscript
			    (LET (NUMBER)
			      (MULTIPLE-VALUE-SETQ (NUMBER POS)
				(PARSE-INTEGER STRING :START (1+ POINTER) :END END :JUNK-ALLOWED T :radix 38))
			      (IF NUMBER
				  (PROGN (FORMAT STRING-STREAM "\\(^~D\\)" NUMBER)	; a number must follow
					 (SETQ POINTER POS))
				  (PROGN (FORMAT STRING-STREAM "\\^{\\ }")	; if no number follows print a caret
					 (INCF POINTER))
				  )
			      ))
			   (T (FORMAT STRING-STREAM "\\^{\\ }")
			      (INCF POINTER))))
		    ((MEMBER CHAR '(#\< #\> #\- #\=)  :TEST #'CHAR=)
		     (LET ((COUNTER 0)		; counting occurences of - and =
			   ARROW-CHAR		; first occured character of - and =
			   (OK T)		; is it possible to extend it to a correct arrow
			   (ARROW-DIRECTION :NONE)	; one out of :none :left :right or :double
			   )
		       (CASE CHAR
			 (#\< (SETQ ARROW-DIRECTION :LEFT))
			 (#\> (SETQ OK NIL))
			 (#\- (SETQ ARROW-CHAR #\- COUNTER 1))
			 (#\= (SETQ ARROW-CHAR #\= COUNTER 1))
			 )
		       (WHEN OK
			 (SETQ OK (OR (EQL 0 POS)	; left of the arrow there must be a blank or the arrow is
				      (CHAR= (CHAR STRING (1- POS)) #\sPACE))))	;the first character of the string
		       (WHEN OK
			 (INCF POS)
			 (DO ((END-ARROW-P NIL)
			      )
			     ((OR END-ARROW-P (EQL POS END) (NOT OK))
			      )
			   (CASE (CHAR STRING POS)
			     (#\< (SETQ OK NIL))
			     (#\> (SETQ ARROW-DIRECTION (IF (EQ ARROW-DIRECTION :LEFT) :DOUBLE :RIGHT)))
			     (#\- (SETQ OK (OR (EQL COUNTER 0) (CHAR= #\- ARROW-CHAR)) ARROW-CHAR #\- COUNTER (1+ COUNTER)))
			     (#\= (SETQ OK (OR (EQL COUNTER 0) (CHAR= #\= ARROW-CHAR)) ARROW-CHAR #\= COUNTER (1+ COUNTER)))
			     (#\sPACE (SETQ END-ARROW-P T))	;space ends an arrow
			     (OTHERWISE (SETQ OK NIL))	; if an other character follows, it was not an arrow
			     )
			   (INCF POS)))
		       (COND ((NOT OK)
			      (FORMAT STRING-STREAM "~:[\\(~C\\)~;~C~]"
				      (OR (CHAR= CHAR #\=) (CHAR= CHAR #\-))
				      CHAR)
			      (INCF POINTER))
			     ((AND (EQL 0 COUNTER) (EQ ARROW-DIRECTION :DOUBLE))	; unequal
			      (FORMAT STRING-STREAM "\\(\\neq\\)")
			      (INCF POINTER 2))
			     ((EQL 0 COUNTER)
			      (FORMAT STRING-STREAM "\\(~C\\)" CHAR)	; < or >
			      (INCF POINTER))
			     ((EQ ARROW-DIRECTION :NONE)	; --- or ===
			      (FORMAT STRING-STREAM "~V@{~C~:*~}" COUNTER CHAR)
			      (INCF POINTER COUNTER))
			     (T (CASE ARROW-DIRECTION	; real arrows
				  (:LEFT
				    (FORMAT STRING-STREAM "\\(\\~:[left~;Left~]arrow\\)" (CHAR= ARROW-CHAR #\=))
				    (INCF POINTER (1+ COUNTER)))
				  (:RIGHT
				    (FORMAT STRING-STREAM "\\(\\~:[right~;Right~]arrow\\)" (CHAR= ARROW-CHAR #\=))
				    (INCF POINTER (1+ COUNTER)))
				  (:DOUBLE
				    (FORMAT STRING-STREAM "\\(\\~:[left~;Left~]rightarrow\\)" (CHAR= ARROW-CHAR #\=))
				    (INCF POINTER (+ 2 COUNTER)))
				  )))
		       )))
	      )))
	  ))))

(DEFUN DOCL=ENDING-POINT-P (STRING)
  (DECLARE (EDITED  "21-JUN-1990 14:24")
	   (AUTHORS DEISS)
	   (INPUT   "STRING is a string-")
	   (EFFECT  "none")
	   (VALUE   "T if STRING is ending with a punctuation character (point, colon, exlamation or question"
		    "mark), blanks may follow, else NIL."))
  (LET ((TRIMMED-STRING (STRING-RIGHT-TRIM '(#\sPACE) STRING))
	)
    (AND (STRING/= TRIMMED-STRING "")
	 (MEMBER (CHAR TRIMMED-STRING (1- (LENGTH TRIMMED-STRING)))
		'(#\. #\: #\? #\!)
		:TEST #'CHAR=))))

(DEFUN DOCL=ONE-CHAR-P (STRING)
  (DECLARE (EDITED  "14-AUG-1992 20:12")
	   (AUTHORS PRCKLN DEISS)
	   (INPUT   "STRING is a string.")
	   (EFFECT  "none")
	   (VALUE   "If STRING consists of the same character, then this character is returned. The "
		    "character must occur with a minimum number of 10. Else Nil is returned. Leading"
		    "and following blanks are allowed."))
  (LET* ((TRIMMED-STRING (STRING-TRIM '(#\sPACE) STRING))
	 )
    (IF (AND (STRING/= TRIMMED-STRING "")
	     (EVERY #'(LAMBDA (CHARACTER)
			(CHAR= (CHAR TRIMMED-STRING 0) CHARACTER))
		    TRIMMED-STRING)
	     (<= 10 (LENGTH TRIMMED-STRING)))
	(CHAR TRIMMED-STRING 0)
	NIL)))

(DEFUN DOCL=PRINT-ONE-CHAR-STRING-LATEX (CHARACTER DOC-STREAM)
  (DECLARE (EDITED  "14-AUG-1992 20:12")
	   (AUTHORS PRCKLN DEISS)
	   (INPUT   "CHARACTER is a character-"
		    "DOC-STREAM is a stream open for output")
	   (EFFECT  "Nothing is done with lines beginning and ending"
		    "with multiple occurrences of CHARACTER.")
	   (VALUE   "Undefined."))
  (declare (ignore CHARACTER DOC-STREAM)))

(DEFUN DOCL=PRINT-DOCUMENTATION-LATEX (DEFINITION DOC-STREAM DOC-KEYS DOC-TYPE)
  (DECLARE (EDITED  "28-JUN-1990 13:24")
	   (AUTHORS DEISS)
	   (INPUT   "DEFINITION is the definiton of a defun-documentation, etc."
		    "DOC-STREAM is a stream open for output."
		    "DOC-KEYS are additional documentation keys."
		    "DOC-TYPE is a keyword (:simple-tex, :interface-tex or :complete-tex),")
	   (EFFECT  "The name, arguments, declarations etc. of DEFINITION are computed and printed on DOC-STREAM"
		    "using DOCL=PRINT-DOC-FUN-LATEX.")
	   (VALUE   "undefined."))
  (LET* ((DEFTYPE nil)      
	 (DEFNAME (CADR DEFINITION))
	 (DEFBODY (CDDR DEFINITION))
	 ARGUMENTS
	 (DECLARATIONS (DOCL=SYS-DECLARATIONS DEFBODY))
	 (DOC-STRINGS (DOCL=DOCUMENTATION-STRINGS DEFBODY)))
    (SETQ ARGUMENTS (CDR (ASSOC 'ARGS DECLARATIONS))
	  DECLARATIONS (DOCL=REMASSOC 'ARGS DECLARATIONS))
    (DOCL=PRINT-DOC-FUN-LATEX DEFTYPE DEFNAME ARGUMENTS DECLARATIONS DOC-STREAM DOC-KEYS DOC-STRINGS DOC-TYPE))
  )
 
(DEFUN DOCL=PRINT-DOC-COM-LATEX (COMLIST DOC-STREAM &OPTIONAL (PREFIX ""))
  (DECLARE (INPUT "COMLIST is a list of strings."
		  "DOC-STREAM is a stream open for output."
		  "PREFIX is printed at the beginning of each line.")
	   (VALUE "undefined")
	   (EFFECT "COMLIST is printed on DOC-STREAM in a format accepted by LATEX."
		   "Nothing is printed if the first string is the first line of a file ( -*- ... -*-) or if"
		   "it is a :lisp comment. If COMLIST contains a string begin{lisp} all comments up to a"
		   "end{lisp} or a defun etc. are ignored.")
	   (AUTHORS "deiss")
	   (EDITED "8.jun.90"))
  (UNLESS (EQL (SEARCH " -*- " (CAR COMLIST)) 0)
    (MAPCAR #'(LAMBDA (COMMENT)
		(WHEN (OR (NOT (EQ DOCL*COMMENT-FORMAT :VERBATIM))
                          (STRING= (STRING-TRIM '(#\sPACE) COMMENT) "end{verbatim}"))
		  (SETQ COMMENT (STRING-TRIM '(#\sPACE) COMMENT)))
		(IF (EQ DOCL*COMMENT-FORMAT :LISP)
		    (WHEN (STRING= COMMENT "end{lisp}")
		      (SETQ DOCL*COMMENT-FORMAT :BOTH))
		    (COND ((STRING= COMMENT "begin{lisp}")
			   (SETQ DOCL*COMMENT-FORMAT :LISP))
			  ((STRING= COMMENT "begin{latex}")
			   (SETQ DOCL*COMMENT-FORMAT :LATEX))
			  ((STRING= COMMENT "end{latex}")
			   (SETQ DOCL*COMMENT-FORMAT :BOTH))
			  ((STRING= COMMENT "begin{verbatim}")
			   (SETQ DOCL*COMMENT-FORMAT :VERBATIM))
			  ((STRING= COMMENT "end{verbatim}")
			   (SETQ DOCL*COMMENT-FORMAT :BOTH))
			  ((EQ DOCL*COMMENT-FORMAT :VERBATIM)	; :verbatim comments are printed with \verb^ ... ^
			   (FORMAT DOC-STREAM "\\verb^~A ^~2%" COMMENT))
			  ((EQ DOCL*COMMENT-FORMAT :LATEX)	; :latex comments are printed direct
			   (FORMAT DOC-STREAM "~A~A~%" PREFIX COMMENT))
			  ((EVERY #'(LAMBDA (CHAR)	; usual comments are prepared to be accepted by latex
				      (CHAR= #\;
					     CHAR))	; in a simple manner.
				  COMMENT)
			   (FORMAT DOC-STREAM "~2%"))	; end of paragraph
			  ((STRING= (STRING-TRIM '(#\sPACE) COMMENT) "")
			   (FORMAT DOC-STREAM "~2%"))	; end of paragraph
			  ((DOCL=ONE-CHAR-P COMMENT)
			   (DOCL=PRINT-ONE-CHAR-STRING-LATEX (DOCL=ONE-CHAR-P COMMENT) DOC-STREAM))
			  ((DOCL=ENDING-POINT-P COMMENT)
			   (FORMAT DOC-STREAM "~A~A~2%" PREFIX (DOCL=STRING-LATEX COMMENT)))
						; print comment and then end of paragraph
			  (T (FORMAT DOC-STREAM "~a~A~%" PREFIX (IF (SYMBOLP COMMENT)
								    (DOCL=SYMBOL-LATEX COMMENT)
								    (DOCL=STRING-LATEX COMMENT))))
						; print comment without forced linebreak
			  )))
	    COMLIST))
  )

;;; Comments can be used in Latex for structuring in the following way:
;;; ; are completely ignored.
;;; ;; in functions are put in the description.
;;; ;;; are handled as text at the top level of files.
;;; ;;;; are sections in LaTeX.
;;; ;;;;; are subsections.
;;; ;;;;;; are subsubsections.

#+old(defun DOCL=PRINT-DEFINITION-LATEX (definition DOC-STREAM DOC-KEYS DOC-TYPE)
  (declare (edited  "24-SEP-1992 17:58")
	   (authors PRCKLN)
	   (input   "DEFINITION is a toplevel expression of a file."
		    "DOC-STREAM is an open output stream."
		    "DOC-KEYS is a list of additional documenation keys."
		    "DOC-TYPE is a keyword (:interface-tex, :simple-tex, or :complete-tex).")
	   (effect  "DOC-TYPE controls whether DEFINITION will be documented.")
	   (value   "Undefined."))
  (COND ((CONSP DEFINITION)
	 (CASE (CAr DEFINITION)
	   (eval-when (DOCL=PRINT-DEFINITIONS-LATEX (rest (rest definition)) DOC-STREAM DOC-KEYS DOC-TYPE))
	   ((DEFUN DEFMACRO)
						      ; Object to be printed is functional.
	    (SETQ DOCL*COMMENT-FORMAT :BOTH)
	    (LET ((DEFTYPE (CAR DEFINITION))
		  (DEFNAME (CADR DEFINITION))
		  (DEFBODY (CDDDR DEFINITION))
		  (DEFLL (CADDR DEFINITION)))
	      (WHEN (OR (EQ :COMPLETE-TEX DOC-TYPE)
			(EQ 'INTERFACE (DOCL=GET-USAGE DEFNAME)))     ; Documentation
		(DOCL=PRINT-DOC-FUN-LATEX DEFTYPE DEFNAME DEFLL (DOCL=SYS-DECLARATIONS DEFBODY)
					  DOC-STREAM DOC-KEYS (DOCL=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE))))
	   ((user::defclass DEFCLASS)
	    (DOCL=PRINT-CLASS-LATEX DEFINITION DOC-STREAM))
	   (DEFSTRUCT
	     (SETQ DOCL*COMMENT-FORMAT :BOTH)
	     (WHEN (AND DOC-STREAM
			(OR (EQ :COMPLETE-TEX DOC-TYPE)
			    (EQ 'INTERFACE (DOCL=GET-USAGE
					     (IF (CONSP (SECOND DEFINITION))  ; name of structure
						 (CAR  (SECOND DEFINITION))
						 (SECOND DEFINITION))))))
	       (DOCL=PRINT-STRUCTURE-LATEX DEFINITION DOC-STREAM)))
	   (DEFSETF (SETQ DOCL*COMMENT-FORMAT :BOTH)
		    (LET ((DEFOUTPUT (CADR DEFINITION))
			  (DEFNAME (IF (SYMBOLP (CADR DEFINITION))
				       (CADR DEFINITION)
				       (IF (AND (CONSP (CADR DEFINITION))
						(CONSP (CDADR DEFINITION))
						(SYMBOLP (CADADR DEFINITION)))
					   (CADADR DEFINITION)
					   (CADR DEFINITION))))
			  (DEFBODY (CDDR DEFINITION))
			  ARGS)
		      (IF (OR (SYMBOLP DEFOUTPUT)
			      (AND (CONSP DEFOUTPUT)
				   (CONSP (CDADR DEFINITION))
				   (SYMBOLP (CADADR DEFINITION))))
			  (PROGN
			    (UNLESS (AND (CONSP (CAR DEFBODY))
					 (EQ 'DECLARE (CAAR DEFBODY)))
			      (SETQ ARGS (CONS (CAR DEFBODY) (SECOND DEFBODY)))
			      (SETQ DEFBODY (CDDR DEFBODY)))
			    (WHEN (AND DOC-STREAM
				       (OR (EQ :COMPLETE-TEX DOC-TYPE)
					   (EQ 'INTERFACE
					       (DOCL=GET-USAGE DEFNAME))))
			      (DOCL=PRINT-DOC-FUN-LATEX 'DEFSETF (LIST (INTERN "SETF") DEFOUTPUT) ARGS (DOCL=SYS-DECLARATIONS DEFBODY)
							DOC-STREAM DOC-KEYS
							(DOCL=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE))))))
	   (DOC=CCC
	     (DOCL=PRINT-DOC-COM-LATEX (CDR DEFINITION) DOC-STREAM))
	   (DOC=CCCC
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
	       (FORMAT DOC-STREAM
		       "~2%\\goodbreak~%\\section{~A}~2%"
		       (DOCL=STRING-LATEX (CADR DEFINITION)))))
	   (DOC=CCCCC
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
	       (FORMAT DOC-STREAM
		       "~2%\\goodbreak~%\\subsection{~A}~2%"
		       (DOCL=STRING-LATEX (CADR DEFINITION)))))
	   (DOC=CCCCCC
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
	       (FORMAT DOC-STREAM
		       "~2%\\goodbreak~%\\subsubsection{~A}~2%"
		       (DOCL=STRING-LATEX (CADR DEFINITION)))))
	   (keim::doc~latex
	    (error "this is an error")
	    (let ((doc-stream *terminal-io*))
	    (terpri doc-stream)
	    (princ (cadr definition) doc-stream)
	    (terpri doc-stream)
	    #+old(format doc-stream "~%~A~%" 
		    (docl=string-latex (cadr definition)))))
	   (OTHERWISE
	     (COND (nil				      ; To Do
		    (DOCL=PRINT-DEFINITIONS-LATEX (MACROEXPAND-1 DEFINITION) DOC-STREAM DOC-KEYS DOC-TYPE))
		   ((AND (EQL 0 (SEARCH "DEF" (symbol-name (CAR DEFINITION))
					:END2 (MIN (LENGTH (symbol-name (CAR DEFINITION))) 3)))
			 (OR (NOT (EQ :SIMPLE-TEX DOC-TYPE))
			     (NOT (STRING-EQUAL (symbol-name (CAR DEFINITION))
						"defmethod"))))
		    (SETQ DOCL*COMMENT-FORMAT :BOTH)
		    (LET ((DEFTYPE (CAR DEFINITION))
			  (DEFOUTPUT (CADR DEFINITION))
			  (DEFNAME (IF (SYMBOLP (CADR DEFINITION))
				       (CADR DEFINITION)
				       (IF (AND (CONSP (CADR DEFINITION))
						(CONSP (CDADR DEFINITION))
						(SYMBOLP (CADADR DEFINITION)))
					   (CADADR DEFINITION)
					   (CADR DEFINITION))))
			  (DEFBODY (CDDR DEFINITION))
			  ARGS)
		      (IF (OR (SYMBOLP DEFOUTPUT)
			      (AND (CONSP DEFOUTPUT)
				   (CONSP (CDADR DEFINITION))
				   (SYMBOLP (CADADR DEFINITION))))
			  (PROGN
			    (UNLESS (AND (CONSP (CAR DEFBODY))
					 (EQ 'DECLARE (CAAR DEFBODY)))
			      (SETQ ARGS (CAR DEFBODY))
			      (SETQ DEFBODY (CDR DEFBODY)))
			    (WHEN (AND DOC-STREAM
				       (OR (EQ :COMPLETE-TEX DOC-TYPE)
					   (EQ 'INTERFACE
					       (DOCL=GET-USAGE DEFNAME))))
			      (DOCL=PRINT-DOC-FUN-LATEX DEFTYPE DEFOUTPUT ARGS (DOCL=SYS-DECLARATIONS DEFBODY)
							DOC-STREAM DOC-KEYS
							(DOCl=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE)))))))))	 
)))

; replace case with a cond using string= dan
(defun DOCL=PRINT-DEFINITION-LATEX (definition DOC-STREAM DOC-KEYS DOC-TYPE)
  (declare (edited  "24-SEP-1992 17:58")
	   (input   "DEFINITION is a toplevel expression of a file."
		    "DOC-STREAM is an open output stream."
		    "DOC-KEYS is a list of additional documenation keys."
		    "DOC-TYPE is a keyword (:interface-tex, :simple-tex, or :complete-tex).")
	   (effect  "DOC-TYPE controls whether DEFINITION will be documented.")
	   (value   "Undefined."))
  (if (and (CONSP DEFINITION) (symbolp (car definition)))
      (cond ((string= (CAr DEFINITION) "EVAL-WHEN")
	     (DOCL=PRINT-DEFINITIONS-LATEX 
	      (rest (rest definition)) DOC-STREAM DOC-KEYS DOC-TYPE))
	    ((or (string= (CAr DEFINITION) "DEFUN")
		 (string= (CAr DEFINITION) "DEFMACRO"))
					;; Object to be printed is functional.
		(SETQ DOCL*COMMENT-FORMAT :BOTH)
		(LET ((DEFTYPE (CAR DEFINITION))
			 (DEFNAME (CADR DEFINITION))
			 (DEFBODY (CDDDR DEFINITION))
		       (DEFLL (CADDR DEFINITION)))
		     (WHEN (OR (EQ :COMPLETE-TEX DOC-TYPE)
			       (EQ 'INTERFACE (DOCL=GET-USAGE DEFNAME))) ; Documentation
			   (DOCL=PRINT-DOC-FUN-LATEX DEFTYPE DEFNAME DEFLL (DOCL=SYS-DECLARATIONS DEFBODY)
						     DOC-STREAM DOC-KEYS (DOCL=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE))))
	    ((string= (CAr DEFINITION) "DEFCLASS")
	     (DOCL=PRINT-CLASS-LATEX DEFINITION DOC-STREAM))
	    ((string= (CAR DEFINITION) "DEFSTRUCT")
		(SETQ DOCL*COMMENT-FORMAT :BOTH)
		(WHEN (AND DOC-STREAM
			   (OR (EQ :COMPLETE-TEX DOC-TYPE)
			       (EQ 'INTERFACE (DOCL=GET-USAGE
					       (IF (CONSP (SECOND DEFINITION)) ; name of structure
						   (CAR  (SECOND DEFINITION))
						   (SECOND DEFINITION))))))
		      (DOCL=PRINT-STRUCTURE-LATEX DEFINITION DOC-STREAM)))
	    ((string= (CAr DEFINITION) "DEFSETF")
	     (SETQ DOCL*COMMENT-FORMAT :BOTH)
	     (LET ((DEFOUTPUT (CADR DEFINITION))
		      (DEFNAME (IF (SYMBOLP (CADR DEFINITION))
				   (CADR DEFINITION)
				   (IF (AND (CONSP (CADR DEFINITION))
					    (CONSP (CDADR DEFINITION))
					    (SYMBOLP (CADADR DEFINITION)))
				       (CADADR DEFINITION)
				       (CADR DEFINITION))))
		      (DEFBODY (CDDR DEFINITION))
		    ARGS)
		  (IF (OR (SYMBOLP DEFOUTPUT)
			  (AND (CONSP DEFOUTPUT)
			       (CONSP (CDADR DEFINITION))
			       (SYMBOLP (CADADR DEFINITION))))
		      (PROGN
		       (UNLESS (AND (CONSP (CAR DEFBODY))
				    (EQ 'DECLARE (CAAR DEFBODY)))
			       (SETQ ARGS (CONS (CAR DEFBODY) (SECOND DEFBODY)))
			       (SETQ DEFBODY (CDDR DEFBODY)))
		       (WHEN (AND DOC-STREAM
				  (OR (EQ :COMPLETE-TEX DOC-TYPE)
				      (EQ 'INTERFACE
					  (DOCL=GET-USAGE DEFNAME))))
			     (DOCL=PRINT-DOC-FUN-LATEX 'DEFSETF (LIST (INTERN "SETF") DEFOUTPUT) ARGS (DOCL=SYS-DECLARATIONS DEFBODY)
						       DOC-STREAM DOC-KEYS
						       (DOCL=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE))))))
	    ((string= (CAr DEFINITION) "DOC=CCC")
	     (DOCL=PRINT-DOC-COM-LATEX (CDR DEFINITION) DOC-STREAM))
	    ((string= (CAr DEFINITION) "DOC=CCCC")
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
		     (FORMAT DOC-STREAM
			     "~2%\\goodbreak~%\\section{~A}~2%"
			     (DOCL=STRING-LATEX (CADR DEFINITION)))))
	    ((string= (CAr DEFINITION) "DOC=CCCCC")
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
		     (FORMAT DOC-STREAM
			     "~2%\\goodbreak~%\\subsection{~A}~2%"
			     (DOCL=STRING-LATEX (CADR DEFINITION)))))
	    ((string= (CAr DEFINITION) "DOC=CCCCCC")
	     (UNLESS (EQ DOCL*COMMENT-FORMAT :LISP)
		     (FORMAT DOC-STREAM
			     "~2%\\goodbreak~%\\subsubsection{~A}~2%"
			     (DOCL=STRING-LATEX (CADR DEFINITION)))))
	    ((string= (CAr DEFINITION) "DOC~LATEX")
	       (terpri doc-stream)
	       (princ (cadr definition) doc-stream)
	       (terpri doc-stream)
	       #+old(format doc-stream "~%~A~%" 
			    (docl=string-latex (cadr definition))))
	    (t
	     (COND (nil			; To Do
		    (DOCL=PRINT-DEFINITIONS-LATEX (MACROEXPAND-1 DEFINITION) DOC-STREAM DOC-KEYS DOC-TYPE))
		   ((AND (or (string-equal "TERM~DEFGENERIC"
					   (symbol-name (CAR DEFINITION)))
			     (EQL 0 (SEARCH "DEF" (symbol-name (CAR DEFINITION))
					:END2 (MIN (LENGTH (symbol-name (CAR DEFINITION))) 3))))
			 (OR (NOT (EQ :SIMPLE-TEX DOC-TYPE))
			     (NOT (STRING-EQUAL (symbol-name (CAR DEFINITION))
						"defmethod"))))
		    (SETQ DOCL*COMMENT-FORMAT :BOTH)
		    (LET ((DEFTYPE (CAR DEFINITION))
			     (DEFOUTPUT (CADR DEFINITION))
			     (DEFNAME (IF (SYMBOLP (CADR DEFINITION))
					  (CADR DEFINITION)
					  (IF (AND (CONSP (CADR DEFINITION))
						   (CONSP (CDADR DEFINITION))
						   (SYMBOLP (CADADR DEFINITION)))
					      (CADADR DEFINITION)
					      (CADR DEFINITION))))
			   (DEFBODY (CDDR DEFINITION))
			   ARGS)
			 (IF (OR (SYMBOLP DEFOUTPUT)
				 (AND (CONSP DEFOUTPUT)
				      (CONSP (CDADR DEFINITION))
				      (SYMBOLP (CADADR DEFINITION))))
			     (PROGN
			      (UNLESS (AND (CONSP (CAR DEFBODY))
					   (EQ 'DECLARE (CAAR DEFBODY)))
				      (SETQ ARGS (CAR DEFBODY))
				      (SETQ DEFBODY (CDR DEFBODY)))
			      (WHEN (AND DOC-STREAM
					 (OR (EQ :COMPLETE-TEX DOC-TYPE)
					     (EQ 'INTERFACE
						 (DOCL=GET-USAGE DEFNAME))))
				    (DOCL=PRINT-DOC-FUN-LATEX DEFTYPE DEFOUTPUT ARGS (DOCL=SYS-DECLARATIONS DEFBODY)
							      DOC-STREAM DOC-KEYS
							      (DOCl=DOCUMENTATION-STRINGS DEFBODY) DOC-TYPE)))))))))))

(DEFUN DOCL=PRINT-DEFINITIONS-LATEX (print-fileinfo DOC-STREAM DOC-KEYS DOC-TYPE)
  (DECLARE (EDITED "12.jun.90")
	   (AUTHORS "deiss")
	   (INPUT "DOC-TYPE is a keyword (:interface-tex, :simple-tex, or :complete-tex).")
	   (EFFECT "DOC-TYPE controls which functions etc. will be documented."))
  (MAPC #'(LAMBDA (DEFINITION) (DOCL=PRINT-DEFINITION-LATEX definition DOC-STREAM DOC-KEYS DOC-TYPE))
	PRINT-FILEINFO))

(DEFUN DOCL=PRINT-LAMBDALIST-LATEX (LAMBDALIST DOC-STREAM)
  (DECLARE (INPUT "LAMBDALIST is the lambdalist of definition-"
		  "DOC-STREAM is a stream open for output.")
	   (VALUE "undefined")
	   (EFFECT "LAMBDALIST is printed on DOC-STREAM as a list of single words, separated by blanks or"
		   "by LATEX commands like newline")
	   (AUTHORS "deiss")
	   (EDITED "12.jun.90"))
  (COND ((NULL LAMBDALIST)
	 (FORMAT DOC-STREAM "---"))
	((EQL 1 (LENGTH LAMBDALIST))		; no &optional etc. is possible
	 (FORMAT DOC-STREAM "~(~:A~)" (DOCL=SYMBOL-LATEX (CAR LAMBDALIST))))
	(T (MAPCAR #'(LAMBDA (ARGUMENT)
		       (COND ((AND (SYMBOLP ARGUMENT)
				   (CHAR= (CHAR (SYMBOL-NAME ARGUMENT) 0)
					  #\&))
			      (FORMAT DOC-STREAM
				      "~(~:A~)~~"
				      (DOCL=SYMBOL-LATEX ARGUMENT)))
			     ((SYMBOLP ARGUMENT)
			      (FORMAT DOC-STREAM
				      "~(~:A~)~%"
				      (DOCL=SYMBOL-LATEX ARGUMENT)))
			     (T (FORMAT DOC-STREAM
					"~(~:A~)~%"
					(DOCL=STRING-LATEX (FORMAT NIL "~:S" ARGUMENT))))
			     ))
		   LAMBDALIST))
	))
	  
;;; The possible DECLARE keys are:
;;; EDITED, AUTHORS, INPUT, EFFECT, 
;;;	VALUE 
;;;	VALUES
;;;	REMARK
;;;	EXAMPLE
;;;       EXAMPLES,
;;;	COMMENTS,
;;;	LAMBDALIST.
;;;

(DEFUN DOCL=PRINT-DOC-FUN-LATEX (TYPE NAME LAMBDALIST DECLARATIONS DOC-STREAM DOC-KEYS DOC-STRINGS DOC-TYPE)
  (DECLARE (INPUT "TYPE is the type (defun, defmacro etc.) of the definition NAME to be documented."
		  "LAMBDALIST is the lambda list of the definition NAME to be documented"
		  "on the open output stream DOC-STREAM."
		  "DECLARATIONS is the list of its declarations."
		  "DOC-STRINGS is the list of documentation strings."
		  "DOC-TYPE is a keyword (:simple-tex, :interface-tex or :complete-tex),")
	   (EFFECT "Prints documentation for a definition on the stream DOC-STREAM.")
	   (AUTHORS "deiss")
	   (EDITED "12.jun.90"))
  (LET ((INPUT (DOCL=PRINT-CASSOC 'INPUT DECLARATIONS))
	(EFFECT (DOCL=PRINT-CASSOC 'EFFECT DECLARATIONS))
	(VALUE (DOCL=PRINT-CASSOC 'VALUE DECLARATIONS))
	(VALUES (DOCL=PRINT-CASSOC 'VALUES DECLARATIONS))
	(REMARK (DOCL=PRINT-CASSOC 'REMARK DECLARATIONS))
	(EXAMPLE (DOCL=PRINT-CASSOC 'EXAMPLE DECLARATIONS))
	(EXAMPLES (DOCL=PRINT-CASSOC 'COMMENTS DECLARATIONS))
	(COMMENTS (MAPCAR #'(LAMBDA (SYMBOL-STRING)
			      (IF (SYMBOLP SYMBOL-STRING)
				  (DOCL=SYMBOL-LATEX SYMBOL-STRING)
				  (DOCL=STRING-LATEX SYMBOL-STRING)))
			  (DOCL=PRINT-CASSOC 'COMMENTS DECLARATIONS)))
	(LAMBDALIST (LET ((ARGLIST (DOCL=PRINT-CASSOC 'ARGS DECLARATIONS)))
		      (COND (ARGLIST (LIST ARGLIST))
			    ((LISTP LAMBDALIST) LAMBDALIST)
			    (T NIL)))))
    (WHEN (OR INPUT EFFECT VALUE VALUES REMARK EXAMPLE EXAMPLES COMMENTS
	      (DOCL=PRINT-CASSOC 'AUTHORS DECLARATIONS) (DOCL=PRINT-CASSOC 'EDITED DECLARATIONS))
      (FORMAT DOC-STREAM
	      "~2&\\begin{lispdefinition}{~A}{~A}{"
	      (CASE TYPE
		(DEFUN "Function")
		(DEFMACRO "Macro")
		(DEFCLASS "Class")
		(DEFGENERIC "Generic Function")
		(OTHERWISE (if (string= (symbol-name type) "TERM~DEFGENERIC")
			       "Term Generic Function"
			       (FORMAT NIL "~:(~A~)" TYPE))))
	      (DOCL=SYMBOL-LATEX NAME NIL))
      (DOCL=PRINT-LAMBDALIST-LATEX LAMBDALIST DOC-STREAM)
      (FORMAT DOC-STREAM "}~%")
      (WHEN COMMENTS
	(FORMAT DOC-STREAM "~&\\item~%")
	(MAPC #'(LAMBDA (COMMENT)
		  (IF (DOCL=ENDING-POINT-P COMMENT)
		      (FORMAT DOC-STREAM "~A~2%" COMMENT)
		      (FORMAT DOC-STREAM "~A~%" COMMENT)))
	      COMMENTS))
      (WHEN (AND (NOT (EQ :SIMPLE-TEX DOC-TYPE))
		 (MEMBER 'AUTHORS DOC-KEYS))
	(UNLESS (SOME #'(LAMBDA (STRING) (SEARCH "Author" STRING)) COMMENTS)
	  (FORMAT DOC-STREAM
		  "~&\\item [Authors\\hfill]~:[~:* Nobody edited this.~;~:*~{ ~A~}~]~%"
		  (DOCL=PRINT-CASSOC 'AUTHORS DECLARATIONS))))
      (WHEN (AND (NOT (EQ :SIMPLE-TEX DOC-TYPE))
		 (MEMBER 'AUTHORS DOC-KEYS))
	(UNLESS (SOME #'(LAMBDA (STRING) (SEARCH "Edited" STRING)) COMMENTS)
	  (FORMAT DOC-STREAM
		  "~&\\item [Edited\\hfill]~:[~:* This function doesn't exist.~;~:*~{ ~A~}~]~%"
		  (DOCL=PRINT-CASSOC 'EDITED DECLARATIONS))))
      (UNLESS (SOME #'(LAMBDA (STRING) (SEARCH "Input" STRING)) COMMENTS)
	(COND ((AND LAMBDALIST (NOT INPUT))
	       (FORMAT DOC-STREAM "~&\\item [Input\\hfill] These bad guys didn't comment the input: ~A~%"
		       (DOCL=PRINT-CASSOC 'AUTHORS DECLARATIONS)))
	      (INPUT (FORMAT DOC-STREAM "~&\\item [Input\\hfill] ")
		     (DOCL=PRINT-DOC-COM-LATEX INPUT DOC-STREAM))
	      (T (FORMAT DOC-STREAM "~&\\item [Input\\hfill] ---")))
	)
      (WHEN (OR (NOT (EQ :SIMPLE-TEX DOC-TYPE))
		(AND EFFECT
		     (NOT (AND (= 1 (LENGTH EFFECT))
			       (MEMBER (STRING-TRIM '(#\sPACE) (FIRST EFFECT)) '("-" "None" "None.") :TEST #'STRING-EQUAL)))))
	(UNLESS (SOME #'(LAMBDA (STRING) (SEARCH "Effect" STRING)) COMMENTS)
	  (FORMAT DOC-STREAM "~&\\item [Effect\\hfill]~%")
	  (COND ((NOT EFFECT)
		 (FORMAT DOC-STREAM "None."))
		(EFFECT (DOCL=PRINT-DOC-COM-LATEX EFFECT DOC-STREAM))
		(T NIL))
	  ))
      (UNLESS (SOME #'(LAMBDA (STRING) (SEARCH "Value" STRING)) COMMENTS)
	(FORMAT DOC-STREAM "~&\\item [~A\\hfill]~%" (IF VALUES "Values" "Value"))
	(COND ((NOT (OR VALUE VALUES))
	       (FORMAT DOC-STREAM "Undefined."))
	      ((OR VALUE VALUES) (DOCL=PRINT-DOC-COM-LATEX (OR VALUE VALUES) DOC-STREAM))
	      (T NIL))
	)
      (WHEN REMARK
	(FORMAT DOC-STREAM "~&\\item [Remark\\hfill]~%")
	(COND ((NOT REMARK) NIL)
	      (REMARK (DOCL=PRINT-DOC-COM-LATEX REMARK DOC-STREAM))
	      (T NIL)))
      (WHEN (OR EXAMPLES EXAMPLE)
	(FORMAT DOC-STREAM "~&\\item [~A\\hfill]~%" (IF EXAMPLES "Examples" "Example"))
	(DOCL=PRINT-DOC-COM-LATEX (OR EXAMPLES EXAMPLE) DOC-STREAM)
	)
      (MAPC #'(LAMBDA (KEY)
		(LET ((KEYVALUE (CDR (ASSOC KEY DECLARATIONS :KEY #'SYMBOL-NAME :TEST #'STRING=))))
		  (WHEN KEYVALUE
		    (FORMAT DOC-STREAM
			    "~&\\item [~A\\hfill]~%"
			    (DOCL=STRING-LATEX (STRING-CAPITALIZE (symbol-name KEY))))
		    (DOCL=PRINT-DOC-COM-LATEX KEYVALUE DOC-STREAM))))
	    DOC-KEYS)
      (WHEN DOC-STRINGS
	(FORMAT DOC-STREAM "~&\\item [Documentation\\hfill]~%")
	(DOCL=PRINT-DOC-COM-LATEX DOC-STRINGS DOC-STREAM))
      (FORMAT DOC-STREAM "~&\\end{lispdefinition}~2%")
      )))

(DEFUN DOCL=PRINT-STRUCTURE-LATEX (DEFINITION DOC-STREAM)
  (DECLARE (EDITED  "03-JUL-1990 08:56")
	   (AUTHORS DEISS)
	   (INPUT   "DEFINITION is the definition of a defstruct."
		    "DOC-STREAM is a stream open for output.")
	   (EFFECT  "information about DEFINITION is written on DOC-STREAM.")
	   (VALUE   "undefined")
	   )
  (LET* ((STRUCTURE-NAME (IF (CONSP (SECOND DEFINITION))
			     (FIRST (SECOND DEFINITION))
			     (SECOND DEFINITION)))
	 (STRUCTURE-OPTIONS (IF (CONSP (SECOND DEFINITION))
				(CDR (SECOND DEFINITION))
				NIL))
	 DOC-STRINGS
	 SLOTNAMES
	 STRUCTURE-SLOTS	
	 )
    (IF (MEMBER :SIMPLE STRUCTURE-OPTIONS :TEST #'EQ)
	(SETQ DOC-STRINGS NIL
	      STRUCTURE-SLOTS (delete nil (maplist #'(LAMBDA (EXPRS)
						       (LET ((EXPR (FIRST EXPRS)))
							 (COND ((SYMBOLP EXPR)
								(LET ((COMS NIL))
								  (do ()
								      ((not (AND (CONSP (FIRST (REST EXPRS)))
										 (EQL 'DOCL=C (FIRST (FIRST (REST EXPRS)))))))
								    (PUSH (POP (REST EXPRS)) COMS))
								  (CONS EXPR (NREVERSE COMS))))
							       (T NIL))))
						   (CDDR DEFINITION))))
	(PROGN
	  (SETQ DOC-STRINGS NIL
		SLOTNAMES nil  ; To Do
		STRUCTURE-SLOTS (delete nil (MAPLIST #'(LAMBDA (EXPRS)
							 (LET ((EXPR (FIRST EXPRS)))
							   (COND ((OR (AND (SYMBOLP EXPR)
									   (MEMBER (SYMBOL-NAME EXPR) SLOTNAMES :TEST #'STRING=))
								      (AND (CONSP EXPR)
									   (MEMBER (SYMBOL-NAME (CAR EXPR)) SLOTNAMES :TEST #'STRING=)))
								  (LET ((COMS NIL))
								    (do () ((not (EQL 'DOCL=C (FIRST (REST EXPRS)))))
								      (PUSH (POP (REST EXPRS)) COMS))
								    (CONS EXPR (NREVERSE COMS))))
								 (T NIL))))
						     (CDDR DEFINITION))))
	  (WHEN (STRINGP DOC-STRINGS) (SETQ DOC-STRINGS (LIST DOC-STRINGS)))
	  ))
    (FORMAT DOC-STREAM "~2%\\begin{structure}{~A}~%" (DOCL=SYMBOL-LATEX STRUCTURE-NAME))
    (WHEN STRUCTURE-OPTIONS
      (FORMAT DOC-STREAM "\\begin{structureoptions}~%")
      (MAPC #'(LAMBDA (OPTION)
		(IF (CONSP OPTION)
		    (CASE (CAR OPTION)
		      (:TYPE
			(FORMAT DOC-STREAM
				"~&\\item [:type\\hfill] ~(~S~) ~:[~;~:*~(~A~)~]~%"
				(SECOND OPTION)
				(IF (CDDR OPTION) (DOCL=SYMBOL-LATEX (THIRD OPTION)) NIL)))
		      (:WRITE-DEMONS
			(FORMAT DOC-STREAM
				"~&\\item [:write-demons\\hfill]~{~( ~S~)~}~%"
				(CDR OPTION)))
		      ((:CONC-NAME :ALTERANT :DELETE :COPIER :WITH :PREDICATE :INCLUDE :EXTENSION)
		       (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill] ~(~A~)~%"
			       (CAR OPTION)
			       (DOCL=SYMBOL-LATEX (SECOND OPTION))))
		      ((:OPTIMIZED :EXTERNAL)
		       (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill]~(~{ ~S~}~)~%"
			       (CAR OPTION)
			       (CDR OPTION)))
		      (:PPRINT
			(FORMAT DOC-STREAM "~&\\item [:pprint\\hfill]~[dummy~; non-standard~; ~(~A~) non-standard~]~%"
              (LENGTH (CDR OPTION))
              (cadr option)
				))
		      (:CONSTRUCTOR
			(FORMAT DOC-STREAM "~&\\item [:constructor\\hfill] ~(~A~)~%" (DOCL=SYMBOL-LATEX (SECOND OPTION)))
			(WHEN (CDDR OPTION)
			  (FORMAT DOC-STREAM "~%\\parbox[t]{\\optionwidth}{")
			  (DOCL=PRINT-LAMBDALIST-LATEX (THIRD OPTION) DOC-STREAM)
			  (FORMAT DOC-STREAM "}~%"))
			(WHEN (CDDDR OPTION)
			  (FORMAT DOC-STREAM "~%~:[preforms defined~%~;pre- and postforms defined~%~]"
				  (CDDDDR OPTION))))
		      ((:ADDITIONAL-FUNCTIONS :ADDITIONAL-MACROS :ADDITIONAL-DEFSUBSTS)
		       (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill]~{ ~(~A~)~}~%"
			       (CAR OPTION)
			       (MAPCAR #'(LAMBDA (ADDITIONAL)
					   (DOCL=SYMBOL-LATEX (CAR ADDITIONAL)))
				       (CDR OPTION)))
		       ))
		    (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill] ~~~%" OPTION))	; options without arguments
		)
	    STRUCTURE-OPTIONS)
      (FORMAT DOC-STREAM "\\end{structureoptions}~%"))
    (WHEN STRUCTURE-SLOTS
      (FORMAT DOC-STREAM "\\begin{structureslots}~%")
      (MAPC #'(LAMBDA (SLOT-COMS)
		(LET ((SLOT (FIRST SLOT-COMS)))
		  (IF (CONSP SLOT)
		      (PROGN
			(FORMAT DOC-STREAM "~&\\item [~(~A~)\\hfill]~%" (DOCL=SYMBOL-LATEX (CAR SLOT)))
			(LET ((FIRST T))
			  (MAPC #'(LAMBDA (SLOT-COM)
				    (COND (FIRST
					   (FORMAT DOC-STREAM "(")
					   (FORMAT DOC-STREAM "~A" (SECOND SLOT-COM))
					   (SETQ FIRST NIL))
					  (T (FORMAT DOC-STREAM "~%~A" (SECOND SLOT-COM)))))
				(REST SLOT-COMS))
			  (WHEN (REST SLOT-COMS) (FORMAT DOC-STREAM ")~%")))
			(FORMAT DOC-STREAM "\\begin{slotoptions}~%")
			(MAPC #'(LAMBDA (OPTION)
				  (IF (CONSP OPTION)
				      (CASE (CAR OPTION)
					(:DEFAULT
					  (FORMAT DOC-STREAM
						  "~&\\item [:default\\hfill] ~(~:A~)~%"
						  (DOCL=STRING-LATEX (FORMAT NIL "~S" (SECOND OPTION)))))
					(:FUNCTIONAL
					  (FORMAT DOC-STREAM "~&\\item [:functional\\hfill] ~(~S~)~%" (SECOND OPTION)))
					(:PPRINT
					  (FORMAT DOC-STREAM "~&\\item [:pprint\\hfill] defined by user~%"))
					((:COPIER :LINEARIZER)
					 (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill] ~:[~;~(~A~) ~]defined by user~%"
						 (CAR OPTION)
						 (SYMBOLP (SECOND OPTION))
						 (DOCL=SYMBOL-LATEX (SECOND OPTION))))
					(:DOC-STRING
					  (FORMAT DOC-STREAM
						  "~&\\item [:doc-string\\hfill] ~A~%"
						  (DOCL=STRING-LATEX (SECOND OPTION))))
					(:CHECK
					  (FORMAT DOC-STREAM "~&\\item [:check\\hfill] ~:[defined by user~;~(~A~)~]~%"
						  (SYMBOLP (SECOND OPTION))
						  (DOCL=SYMBOL-LATEX (SECOND OPTION))))
					(:TYPE
					  (FORMAT DOC-STREAM "~&\\item [:type\\hfill] ~(~A~)~%" (SECOND OPTION)))
					(:SLOT-TYPE
					  (FORMAT DOC-STREAM
						  "~&\\item [:slot-type\\hfill] ~(~A~)~%"
						  (DOCL=SYMBOL-LATEX (SECOND OPTION))))
					)
				      (FORMAT DOC-STREAM "~&\\item [~(~S~)\\hfill] ~~~%" OPTION)))
			      (CDR SLOT))
			(FORMAT DOC-STREAM "\\end{slotoptions}~%")
			)
		      (PROGN 
			(FORMAT DOC-STREAM "~&\\item [~(~A~)\\hfill]~%" (DOCL=SYMBOL-LATEX SLOT))
			(LET ((FIRST T))
			  (MAPC #'(LAMBDA (SLOT-COM)
				    (COND (FIRST
					   (FORMAT DOC-STREAM "(")
					   (FORMAT DOC-STREAM "~A" (SECOND SLOT-COM))
					   (SETQ FIRST NIL))
					  (T (FORMAT DOC-STREAM "~%~A" (SECOND SLOT-COM)))))
				(REST SLOT-COMS))
			  (WHEN (REST SLOT-COMS) (FORMAT DOC-STREAM ")~%"))
			  (FORMAT DOC-STREAM "~~ "))))))
	    STRUCTURE-SLOTS)
      (FORMAT DOC-STREAM "\\end{structureslots}~%"))
    (WHEN DOC-STRINGS
      (FORMAT DOC-STREAM "\\begin{structuredocs}~%")
      (MAPC #'(LAMBDA (DOC-STRING)
		(IF (DOCL=ENDING-POINT-P DOC-STRING)
		    (FORMAT DOC-STREAM "~A~2%" (DOCL=STRING-LATEX DOC-STRING))
		    (FORMAT DOC-STREAM "~A~%" (DOCL=STRING-LATEX DOC-STRING))))
	    DOC-STRINGS)
      (FORMAT DOC-STREAM "\\end{structuredocs}~%"))
    (FORMAT DOC-STREAM "\\end{structure}~%")
    )
  )

(DEFUN DOCl=PRINT-CLASS-LATEX (DEFINITION DOC-STREAM)
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
    (FORMAT DOC-STREAM "~2&\\begin{lispdefinition}{Class}{~A}{~:A}~%\\item "
	    (DOCL=SYMBOL-LATEX STRUCTURE-NAME NIL) (THIRD DEFINITION))
    (PRINC (IF DOC-STRING (DOCL=STRING-LATEX DOC-STRING) "No documentation string available!!!!!!!") DOC-STREAM)
    (FORMAT DOC-STREAM "~%\\end{lispdefinition}~%")
    )
  )

