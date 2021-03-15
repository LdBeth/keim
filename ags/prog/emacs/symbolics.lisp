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

(in-package "AGS")

#+(or symbolics explorer)
(defun emacs-time (&optional symbolp)
  (multiple-value-bind (second minute hour day month year day.of.week daylight.saving.time.p time.zone)
      (get-decoded-time)
    (declare (ignore second day.of.week daylight.saving.time.p time.zone))
    (let ((res (format nil "~@2,1,0,'0a-~a-~a ~@2,1,0,'0a:~@2,1,0,'0a"
		       day
		       (case month
			 (1 "JAN")
			 (2 "FEB")
			 (3 "MAR")
			 (4 "APR")
			 (5 "MAY")
			 (6 "JUN")
			 (7 "JUL")
			 (8 "AUG")
			 (9 "SEP")
			 (10 "OCT")
			 (11 "NOV")
			 (12 "DEC"))
		       year
		       hour
		       minute)))
      (if symbolp (intern res) res))))

#+(or symbolics explorer)
(defvar emacs*author8)

#+(or symbolics explorer)
(defun emacs-authors ()
    (intern (string-upcase (format nil "~a" (let ((user (global:send si:*user* :name)))
					      (if (stringp user)
						  user
						  (global:send user :string)))))))

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 '(#\h-r zwei:com-evaluate-and-replace-into-buffer))


#+(or symbolics explorer)
(zwei:DEFCOM COM-update-authors "" ()
  (let ((position (zwei:search (zwei:point) "declare")))
    (cond (position (zwei:move-point position)
		    (let ((ed.position (zwei:search (zwei:point) "(authors")))
		      (cond (ed.position (zwei:move-point ed.position)
					 (unless (search (symbol-name (emacs-authors)) (car ed.position))
					   (zwei:insert (list (car ed.position) (1+ (cadr ed.position)))
							(format nil "~a " (emacs-authors))))))))))
      zwei:DIS-TEXT)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 `(#\h-s com-update-authors))

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-declare (nil)
  #\return "(ags::emacs-time)" #\c-space #\c-m-b #\h-r #\c-a #\tab "(declare (edited  " #\c-e ")"
  #\return "(ags::emacs-authors)" #\c-space #\c-m-b #\h-r #\c-a #\tab "(authors " #\c-e ")"
  #\return #\tab "(input   )"
  #\return #\tab "(effect  )"
  #\return #\tab "(value   ))")

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-declare)
		    #\h-d zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-update-current-declare (nil)
  #\c-m-a #\h-s
  #\c-m-a #\c-s "(declare (edited" #\end "  " #\c-space #\c-e #\c-w
  "(ags::emacs-time)" #\c-space #\c-m-b #\h-r #\c-e ")")

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-update-current-declare)
		    #\h-shift-s zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-kkl-evaluate (nil)
  ;; Evaluation of definitions with update of KKL declarations.
  #\h-shift-s #\c-shift-e)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-kkl-compile (nil)
  ;; Evaluation of definitions with update of KKL declarations.
  #\h-shift-s #\c-shift-c)

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-kkl-evaluate)
		    #\h-shift-e zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-kkl-compile)
		    #\h-shift-c zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:DEFCOM COM-extract-docUmentation "" ()
  (MULTIPLE-VALUE-BIND (PATH MAJOR-MODE)
      (ZWEI::READ-MAJOR-MODE-DEFAULTED-PATHNAME "Extract documentation for file:" (ZWEI::DEFAULT-PATHNAME)); ':NEWEST)
    (LET* ((OUT-PATH (AND ZWEI::*NUMERIC-ARG-P*
			  (kkb-default-pathname
			    (FORMAT NIL "Extract documentation ~A into file:" PATH)
			    PATH (GLOBAL:SEND MAJOR-MODE ':DEFAULT-COMPILER-OBJECT-FILE-TYPE)
			    ':NEWEST ':WRITE)))
	   (BUFFER (ZWEI::FIND-BUFFER-NAMED PATH)))
      (PROG ()
	    (WHEN (AND BUFFER
		       (GLOBAL:SEND BUFFER ':MODIFIED-P)
		       (ZWEI::FQUERY '(:SELECT T) "Save buffer ~A first? " (GLOBAL:SEND BUFFER ':NAME)))
	      (OR (ZWEI::SAVE-BUFFER BUFFER)
		  (RETURN (VALUES))))			;Saving failed, so quit early
	    (ZWEI::TYPEIN-LINE "Extract documentation ~A~@[ into ~A~] ... " PATH OUT-PATH)
	    (DOC-EXTRACT PATH)
	    (ZWEI::TYPEIN-LINE-MORE "Done."))))
  ZWEI::DIS-NONE)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil `(("Extract Documentation" . com-EXTRACT-DOCUMENTATION)))

;;; extract tex documentation
;;; -------------------------

#+(or symbolics explorer)
(zwei:DEFCOM COM-extract-tex-docUmentation "" ()
  (MULTIPLE-VALUE-BIND (PATH MAJOR-MODE)
      (ZWEI::READ-MAJOR-MODE-DEFAULTED-PATHNAME "Extract tex documentation for file:" (ZWEI::DEFAULT-PATHNAME)); ':NEWEST)
    (LET* ((OUT-PATH (AND ZWEI::*NUMERIC-ARG-P*
			  (kkb-default-pathname
			    (FORMAT NIL "Extract tex documentation ~A into file:" PATH)
			    PATH (GLOBAL:SEND MAJOR-MODE ':DEFAULT-COMPILER-OBJECT-FILE-TYPE)
			    ':NEWEST ':WRITE)))
	   (BUFFER (ZWEI::FIND-BUFFER-NAMED PATH)))
      (PROG ()
	    (WHEN (AND BUFFER
		       (GLOBAL:SEND BUFFER ':MODIFIED-P)
		       (ZWEI::FQUERY '(:SELECT T) "Save buffer ~A first? " (GLOBAL:SEND BUFFER ':NAME)))
	      (OR (ZWEI::SAVE-BUFFER BUFFER)
		  (RETURN (VALUES))))			;Saving failed, so quit early
	    (ZWEI::TYPEIN-LINE "Extract tex documentation ~A~@[ into ~A~] ... " PATH OUT-PATH)
	    (DOC-EXTRACT PATH :interface-tex)
	    (ZWEI::TYPEIN-LINE-MORE "Done."))))
  ZWEI::DIS-NONE)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil `(("Extract tex Documentation" . com-EXTRACT-tex-DOCUMENTATION)))

;;; extract manual documentation
;;; -------------------------

#+(or symbolics explorer)
(zwei:DEFCOM COM-extract-manual-docUmentation "" ()
  (MULTIPLE-VALUE-BIND (PATH MAJOR-MODE)
      (ZWEI::READ-MAJOR-MODE-DEFAULTED-PATHNAME "Extract manual documentation for file:" (ZWEI::DEFAULT-PATHNAME)); ':NEWEST)
    (LET* ((OUT-PATH (AND ZWEI::*NUMERIC-ARG-P*
			  (kkb-default-pathname
			    (FORMAT NIL "Extract manual documentation ~A into file:" PATH)
			    PATH (GLOBAL:SEND MAJOR-MODE ':DEFAULT-COMPILER-OBJECT-FILE-TYPE)
			    ':NEWEST ':WRITE)))
	   (BUFFER (ZWEI::FIND-BUFFER-NAMED PATH)))
      (PROG ()
	    (WHEN (AND BUFFER
		       (GLOBAL:SEND BUFFER ':MODIFIED-P)
		       (ZWEI::FQUERY '(:SELECT T) "Save buffer ~A first? " (GLOBAL:SEND BUFFER ':NAME)))
	      (OR (ZWEI::SAVE-BUFFER BUFFER)
		  (RETURN (VALUES))))			;Saving failed, so quit early
	    (ZWEI::TYPEIN-LINE "Extract manual documentation ~A~@[ into ~A~] ... " PATH OUT-PATH)
	    (DOC-EXTRACT PATH :simple-tex)
	    (ZWEI::TYPEIN-LINE-MORE "Done."))))
  ZWEI::DIS-NONE)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil `(("Extract manual Documentation" . com-EXTRACT-manual-DOCUMENTATION)))

;;; extract complete documentation
;;; ------------------------------

#+(or symbolics explorer)
(zwei:DEFCOM COM-extract-complete-docUmentation "" ()
  (MULTIPLE-VALUE-BIND (PATH MAJOR-MODE)
      (ZWEI::READ-MAJOR-MODE-DEFAULTED-PATHNAME "Extract complete documentation for file:" (ZWEI::DEFAULT-PATHNAME)); ':NEWEST)
    (LET* ((OUT-PATH (AND ZWEI::*NUMERIC-ARG-P*
			  (kkb-default-pathname
			    (FORMAT NIL "Extract complete documentation ~A into file:" PATH)
			    PATH (GLOBAL:SEND MAJOR-MODE ':DEFAULT-COMPILER-OBJECT-FILE-TYPE)
			    ':NEWEST ':WRITE)))
	   (BUFFER (ZWEI::FIND-BUFFER-NAMED PATH)))
      (PROG ()
	    (WHEN (AND BUFFER
		       (GLOBAL:SEND BUFFER ':MODIFIED-P)
		       (ZWEI::FQUERY '(:SELECT T) "Save buffer ~A first? " (GLOBAL:SEND BUFFER ':NAME)))
	      (OR (ZWEI::SAVE-BUFFER BUFFER)
		  (RETURN (VALUES))))			;Saving failed, so quit early
	    (ZWEI::TYPEIN-LINE "Extract complete documentation ~A~@[ into ~A~] ... " PATH OUT-PATH)
	    (DOC-EXTRACT PATH :complete)
	    (ZWEI::TYPEIN-LINE-MORE "Done."))))
  ZWEI::DIS-NONE)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil `(("Extract complete Documentation" . com-EXTRACT-COMPLETE-DOCUMENTATION)))

;;; extract complete tex documentation
;;; ------------------------------

#+(or symbolics explorer)
(zwei:DEFCOM COM-extract-complete-tex-docUmentation "" ()
  (MULTIPLE-VALUE-BIND (PATH MAJOR-MODE)
      (ZWEI::READ-MAJOR-MODE-DEFAULTED-PATHNAME "Extract complete tex-documentation for file:"
						(ZWEI::DEFAULT-PATHNAME)); ':NEWEST)
    (LET* ((OUT-PATH (AND ZWEI::*NUMERIC-ARG-P*
			  (kkb-default-pathname
			    (FORMAT NIL "Extract complete tex documentation ~A into file:" PATH)
			    PATH (GLOBAL:SEND MAJOR-MODE ':DEFAULT-COMPILER-OBJECT-FILE-TYPE)
			    ':NEWEST ':WRITE)))
	   (BUFFER (ZWEI::FIND-BUFFER-NAMED PATH)))
      (PROG ()
	    (WHEN (AND BUFFER
		       (GLOBAL:SEND BUFFER ':MODIFIED-P)
		       (ZWEI::FQUERY '(:SELECT T) "Save buffer ~A first? " (GLOBAL:SEND BUFFER ':NAME)))
	      (OR (ZWEI::SAVE-BUFFER BUFFER)
		  (RETURN (VALUES))))			;Saving failed, so quit early
	    (ZWEI::TYPEIN-LINE "Extract complete tex documentation ~A~@[ into ~A~] ... " PATH OUT-PATH)
	    (DOC-EXTRACT PATH :complete-tex)
	    (ZWEI::TYPEIN-LINE-MORE "Done."))))
  ZWEI::DIS-NONE)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 nil `(("Extract complete tex Documentation" . com-EXTRACT-COMPLETE-tex-DOCUMENTATION)))

;;; Same with semicolon comments
;;; ----------------------------

#+(or symbolics explorer)
(zwei:DEFCOM COM-update-authors-my "" ()
  (let ((position (zwei:search (zwei:point) "")))
    (cond (position (zwei:move-point position)
		    (let ((ed.position (zwei:search (zwei:point) "; Authors:")))
		      (cond (ed.position (zwei:move-point ed.position)
					 (unless (search (symbol-name (emacs-authors)) (car ed.position))
					   (zwei:insert (list (car ed.position) (1+ (cadr ed.position)))
							(format nil "~a " (emacs-authors))))))))))
      zwei:DIS-TEXT)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab*
		 `(#\s-s com-update-authors-my))

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-declare-my (nil)
  #\return "(ags::emacs-time t)" #\c-space #\c-m-b #\h-r #\c-a #\tab
  "; Edited:  " #\c-space #\c-e #\c-% #\| #\return #\return #\c-e
  #\return "(ags::emacs-authors)" #\c-space #\c-m-b #\h-r #\c-a #\tab "; Authors: " #\c-e 
  #\return #\tab "; Input:   "
  #\return #\tab "; Effect:  "
  #\return #\tab "; Value:   " #\m-b #\m-b #\m-b #\m-f #\c-f #\c-f #\c-f #\c-f)

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-declare-my)
		    #\s-d zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-update-current-declare-my (nil)
  #\c-m-a #\s-s
  #\c-m-a #\c-s "; Edited:" #\end "  " #\c-space #\c-e #\c-w
  "(ags::emacs-time t)" #\c-space #\c-m-b #\h-r #\c-a #\c-s "|" #\c-b #\c-d #\c-s "|" #\c-b #\c-d)


#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-update-current-declare-my)
		    #\s-shift-s zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-kkl-evaluate-my (nil)
  ;; Evaluation of definitions with update of KKL declarations.
  #\s-shift-s #\c-shift-e)

#+(or symbolics explorer)
(zwei:define-keyboard-macro emacs-kkl-compile-my (nil)
  ;; Evaluation of definitions with update of KKL declarations.
  #\s-shift-s #\c-shift-c)

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-kkl-evaluate-my)
		    #\s-shift-e zwei:*zmacs-comtab*)

#+(or symbolics explorer)
(zwei:command-store (zwei:make-macro-command :emacs-kkl-compile-my)
		    #\s-shift-c zwei:*zmacs-comtab*)
