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

(mod~defmod man :uses (mod doc)
	    :documentation "Things useful for extracting documentation from
systems and files."
	    :exports (
		      man~write-tex-special-hyphenations
		      man~write-tex-title
		      man~write-tex-declarations
		      man~write-input-omega-macros
		      man~exdocu-file-operation
		      man~exdocu-system-operation
		      man~exdocu-module
		      )
	    )

#{\section{The Manual Module}
In this module, we define some auxiliary functions for making manuals
automatically.  We also define the interface to the defsystem package.

#}

(defun man~write-tex-special-hyphenations (stream)
  (declare (edited  " 1-APR-1993 10:21" )
	 (authors KOHLHASE )
	 (input   "An output stream.")
	 (effect  "A TeX Hyphenation statement is written to STREAM.")
	 (value   "Undefined."))
  (format stream "\\hyphenation\{IN-NER--SUBS-TI-TU-TION OUT-TER--SUBS-TI-TU-TION SUBS-TI-TU-TION
             SHARE-SYM RE-PLACE--FREE--VARIABLES--AND--RE-NAME\}~%"))

(defun man~write-tex-title (stream title)
  (declare (edited  " 1-APR-1993 10:31" )
	 (authors KOHLHASE )
	 (input   "An output stream and a string with the title.")
	 (effect  "A TeX title declaration is written to STREAM.")
	 (value   "Undefined."))
  (format stream "\\author{The \\keim-Group\\\\
Fachbereich Informatik, Universit\\\"at des Saarlandes\\\\
Postfach 1150\\\\
D-66041 Saarbruecken\\\\
Germany \\\\    
\{\\tt keim@cs.uni-sb.de\}\\\\\[2cm\]}~%
\\title\{\\mbox\{~A\}\}
\\maketitle~%" title))

(defun man~write-tex-declarations (stream)
  (declare (edited  " 1-APR-1993 10:01" )
	 (authors KOHLHASE )
	 (input   "An output-stream" )
	 (effect  "A set of TeX-declarations for the manual is written onto STREAM.")
	 (value    "Undefined."))
  (format stream "\\documentstyle\[psfig,code,makeidx,fancyheadings\]\{report\}~%
\\pagestyle\{headings\}~%\\makeindex~%~%\\hoffset-1.55cm~%\\voffset-0.55cm~%~%
\\textheight23cm\\textwidth17cm
\\oddsidemargin2cm\\evensidemargin2cm
\\topmargin2cm\\headheight12pt\\headsep1cm
\\parskip1ex plus 2pt\\parindent0em
\\tabcolsep5mm\\raggedbottom~%
\\pagestyle\{fancyplain\}
\\headrulewidth0pt
\\rhead\{\\fancyplain\{\\thepage\}\{\\thepage\}\}
\\cfoot\{\\fancyplain\{\\today\}\{\\today\}\}~%~%~%~%"))
  
(defun man~write-input-omega-macros (stream)
  (declare (edited  " 1-APR-1993 10:06" )
	   (authors KOHLHASE )
	   (input   "An output-stream")
	   (effect  "An input statement for the relevant TeX-macro files 
is written onto STREAM." )
	   (value   "Undefined."))
  (dolist (macro-file (list "omega-abbrevs" "tex-macros" "omega-macros"))
    (format stream "\\input\{~A~A\}~%" *omega-tex-macro-dir* macro-file)))


(defun man~exdocu-file-operation (component force)
  ;; Returns T if the file's documentation had to be extracted.
  (let ((must-extract
	 (or (find force '(:all :new-source-all t) :test #'eq) 
	     (and (find force '(:new-source :new-source-and-dependents)
			:test #'eq)
		  (man=needs-exdocu component)))))
    (cond ((and must-extract
		(probe-file (mk::component-full-pathname component :source)))
	   (mk::with-tell-user ("Extracting documentation from source" 
			    component :source)
	     (or mk::*oos-test*
		 (doc~extract 
		  (mk::component-full-pathname component :source)
		  :simple-tex
		  (mk::component-full-pathname component :tex))
		 ))
	   must-extract)
	  (must-extract
	   (mk::tell-user "Source file not found. Not extracting documentation"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

(defun man=system-depends-on* (x y)
  (let ((y-depends (mapcar #'(lambda (y)
			       (if (symbolp y) (mk::find-system y :load nil) y))
			   (mk::component-depends-on y)))
	(x (if (symbolp x) (mk::find-system x :load nil) x)))
    (or (member x y-depends)
      (some #'(lambda (z) (man=system-depends-on* x z))
	    y-depends))))


(defun man~exdocu-system-operation (component force)
  (let ((has-depends-on nil))
  (labels ((all-files (x)
	      (if (typep x 'mk::file)
		  (list x)
		  (apply #'append (mapcar #'all-files
					  (mk::component-components x)))))
	   (all-depends-on (x)
	    (delete-duplicates 
	      (append 
	       (apply #'append
		      (mapcar #'all-depends-on
			      (mapcar #'(lambda (y)
					  (mk::find-system y :load nil))
				      (mk::component-depends-on x))))
	       (list x)))))
    (mk::with-tell-user 
     ("Making system tex file" component :tex-manual)
     (or mk::*oos-test*
	 (with-open-file (out (mk::component-full-pathname component :tex-manual)
			      :direction :output :if-exists :supersede
			      :if-does-not-exist :create)
	   ;;write preamble
	   (man~write-tex-declarations out)
	   (man~write-input-omega-macros out)
	   (format out "\\begin{document}~% %\\makepostsyntax~%")
	   (man~write-tex-special-hyphenations out)
	   (man~write-tex-title 
	    out 
	    (format nil "~A Manual"
		    (string-capitalize (mk::component-name component))))
	   (dolist (depends-on (stable-sort (remove component (all-depends-on component)) 
					    #'man=system-depends-on*))
	     (setq has-depends-on t)
	     (mk::operate-on-component depends-on :exdocu force)
	     (format out "~%\\input{~A}" 
		     (mk::component-full-pathname depends-on :tex-chapter)))
	   (with-open-file (chap (mk::component-full-pathname component
							 :tex-chapter)
				:direction :output :if-exists :supersede
				:if-does-not-exist :create)
	     (when has-depends-on
	     (format chap "~%\\chapter{~A}" (string-capitalize
					     (mk::component-name component))))
	     (when (mk::component-full-pathname component :tex-intro)
	       (format chap "~%\\input{~A}"
		       (mk::component-full-pathname component :tex-intro)))
	     (dolist (file (all-files component))
	       (format chap "~%\\input{~A}" 
		       (mk::component-full-pathname file :tex))))
	   (format out "~%\\input{~A}"
		   (mk::component-full-pathname component :tex-chapter))
	   (format out 
		   "~%\\begin{appendix}~%
\\input{../doc/prog-conv}
%\\makepostsyntaxsection
\\end{appendix}
{\\tiny\\printindex}
\\end{document}~%")
	   ;;write postamble
	   t)
	 ))
   t)))

(defgeneric man=needs-exdocu (component)
  ;; If there is no tex file, or it is older than the source
  ;; file, then the component needs to be compiled.
  ;; Otherwise we only need to recompile if it depends on a file that changed.
  (:method ((component mk::tex-file))
    (and 
     (probe-file (mk::component-full-pathname component :source)) 
     (or
      (null (probe-file (mk::component-full-pathname component :tex))) 
      (< (file-write-date (mk::component-full-pathname component :tex)) 
	 (file-write-date (mk::component-full-pathname component :source))))))
  (:method ((component mk::tex-system))
    (null (probe-file (mk::component-full-pathname component :tex)))))


(defun man~exdocu-module (module)
  (when (symbolp module)
      (setq module (mod~find-module module)))
  (if (typep module 'ags::mod+module)
      (man~exdocu-file-operation (ags::mod=component module) :all)
      (if (or (stringp module) (pathnamep module))
	  (ags::doc~extract module :simple-tex)
	  (error "Module ~S is not a module, string or pathname" module))))
