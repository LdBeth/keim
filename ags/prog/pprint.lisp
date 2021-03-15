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

(mod~defmod pp :uses (mod)
	    :documentation "Pretty-printing"
	    :exports (pp~pprint
		      pp~pprint-to-string
		      pp+style 
		      pp~find-style 
		      pp~defstyle
		      pp~modify-style
		      pp~pprint-obj-with-desc
		      pp~set-current-style
		      pp~with-style
		      pp~style-p
		      pp~pprint-table))

#{\section{Pretty printing}
This module describes a way to set up and use pretty printing functions.
The method used here is based on the new {\vb PPRINT} functions to be
standardized in the upcoming ANSI \commonlisp. 

The pretty printing functions can be organized into styles. 
#}

(eval-when (load compile eval)
(defclass pp+style ()
  ((name :initarg :name :accessor pp=style-name
	 :documentation "The name of the style (a lisp symbol)")
   (help :initarg :help :accessor pp=style-help
	 :documentation "A help string for the style")
   (parent :initarg :parent :accessor pp=style-parent
	   :documentation "The parent style of this style.")
   (pprint-dispatch :initarg :pprint-dispatch
		    :initform (copy-pprint-dispatch nil)
		    :accessor pp=style-pprint-dispatch
		    :documentation "A pretty-printing dispatch table"))
  (:documentation "A pp+style is basically a way of storing a pprint dispatch
table.  The parent of the style is used to inherit pprint methods. 
The parent's dispatch table is copied when the child is created; any later
additions or deletions to the parent will not show up in the child.  Changes
will show up if they use the same dispatch function name.
The name of a style is considered as a case-insensitive string")
  ))

(defmethod print-object ((style pp+style) stream)
  (format stream "#<~A ~A>" (class-name (class-of style)) (pp=style-name style)))

(eval-when (load compile eval)
(defvar pp*style-hash-table (make-hash-table :test #'equal)
  "A hash table to store the styles, keyed on their names (case-insensitive
strings).")
(defvar pp*current-style nil "The current pprinting style.  Used in
some functions as optional argument.")

(defun pp~set-current-style (key)
  (declare 
   (authors nesmith)
   (input "A style, or a symbol or string naming a style.")
   (effect "If the input is an existing style, or the KEY refers to
an existing style, the value of the global variable pp*current-style
will be set to this style.")
   (value "The style if found."))
  (let ((style (pp~find-style key)))
    (if (pp~style-p style)
	(setq pp*current-style style)
	(error "~S does not refer to a valid style." key))))

(defmacro pp~with-style (key &body forms)
  (declare 
   (authors nesmith)
   (input "A style, or a symbol or string naming a style, and a body
of forms to be executed.")
   (effect "If the KEY is an existing style, or the KEY refers to
an existing style, the value of the global variable pp*current-style
will be bound to this style and the FORMS will be executed. Note:
KEY will be evaluated, so you must quote it if you directly give the
name of a style.")
   (value "The style if found."))
  `(let ((style (pp~find-style ,key)))
     (if (pp~style-p style)
	 (let ((pp*current-style style))
	   ,@forms)
	 (error "~S does not refer to a valid style." ,key))))





(defmacro pp~modify-style (stylename &rest pprint-methods)
  (declare
   (authors nesmith)
   (input "A symbol or string naming an existing style, and a list of
pprint methods.  ")
   (value "T if it succeeds")
   (effect "Modifies the pprint-dispatch table of the given style
as specified by the pprint methods."))
  (let ((new-defuns nil)
	(new-pprint-methods nil))
    (dolist (pprint-method pprint-methods)
      (let ((type (first pprint-method))
	    (fun (second pprint-method))
	    (pri (third pprint-method)))
	(cond ((and (listp fun) (eq (first fun) 'lambda))
	       (let ((newname (make-symbol 
                               (format nil "PP-~A-~A" stylename type))))
		 (push `(defun ,newname ,@(cdr fun)) new-defuns)
		 (push `(,type ,newname ,(eval pri)) new-pprint-methods)))
	      ((symbolp fun)
	       (push pprint-method new-pprint-methods))
	      (t (error 
                  "Function ~S must be either a symbol or a lambda expression."
		  fun)))))
  `(let* ((style (pp~find-style ',stylename))
	  (default-depth
	    (let ((n 1)
		  (newstyle style))
	      (loop (if (pp=style-parent newstyle)
			(setq newstyle (pp~find-style
					(pp=style-parent newstyle))
			      n (1+ n))
			(return n))))))
     (if style
	 (progn ,@new-defuns
		(let ((dispatch-table (pp=style-pprint-dispatch style)))
		  (dolist (pprint-method ',new-pprint-methods t)
		    (set-pprint-dispatch 
		     (first pprint-method) 
		     (if (second pprint-method)
			 (symbol-function (second pprint-method)))
		     (or (third pprint-method)
			 (when (and (symbolp (first pprint-method))
				    (find-class (first pprint-method) nil)
				    (sys~class-precedence-list 
				     (find-class (first pprint-method))))
			   (length (sys~class-precedence-list 
				    (find-class (first pprint-method)))))
			 default-depth)
		     dispatch-table))))
	 (error "~S not an existing style." ',stylename)))))

(defmacro pp~defstyle (name &key (parent 'pp+top)
				 (help "") (pprint-methods nil))
  (declare
   (authors nesmith)
   (input "A symbol NAME, and keyword arguments PARENT (a symbol),
HELP (a string), and a list of PPRINT-METHODS.")
   (effect "A new style with the NAME will be created, initialized 
with initial 
pprint-methods of its PARENTS, then {\\vb PP~MODIFY-STYLE} will be
called on the new style and the given PPRINT-METHODS.  If there were
already a style with name NAME, it will be redefined.")
   (value "Same as {\\vb PP~MODIFY-STYLE}"))
  `(let* ((parent-style (pp~find-style ',parent))
	  (old-style (pp~find-style ',name))
	  (new-style-dispatch-table 
	   (copy-pprint-dispatch
	    (if parent-style
		(pp=style-pprint-dispatch parent-style)
	    nil)))
	  (new-style
	   (make-instance 'pp+style :name ',name :help ',help 
			  :parent ',parent
			  :pprint-dispatch
			  new-style-dispatch-table)))
     (when old-style
       (warn (format nil "Redefining style ~A" ',name)))
     (setf (gethash (string-upcase (string ',name))
		    pp*style-hash-table) new-style)
    (pp~modify-style ,name ,@pprint-methods)))


(defun pp~find-style (name)
  (declare
   (authors nesmith)
   (input "A NAME, which is either a {\\vb pp+style} or a symbol or string.
If the input is a style it will simply be returned, otherwise the style
associated with NAME will be returned.  If there is none, nil will be returned."))
  (etypecase name
   (pp+style name)
   ((or string symbol)
    (gethash (string-upcase (string name)) pp*style-hash-table))))

(defun pp~style-p (thing)
  (typep thing 'pp+style))


(pp~defstyle pp+top 
	     :parent nil 
	     :pprint-methods
	     ((string (lambda (s o) (if *print-readably*
					(write o :stream s :pretty nil
					       :escape t)
					(write-string o s))
			(values))))
	     :help "The topmost style, using the default pretty-printing
functions, except that strings are princ'd (so no quote marks show up).")

)

#+old(defun pp~pprint (obj &optional (style (or pp*current-style
					   (pp~find-style 'pp+top))
				       style-p)
		      (stream nil stream-p))
  (declare
   (authors nesmith)
   (effect  "Print OBJ to STREAM (optional) using STYLE (optional).  
STYLE defaults
to either pp*current-style (if not null) or the style PP+TOP.  
If STYLE is specified and null, 
the ordinary pprint table is
used (just like calling pprint).  
If STREAM is null or not provided, it defaults
to *standard-output*; if STREAM is T, *terminal-io* will be used.
While printing, the variable *print-pprint-dispatch* is bound to the
print dispatch of the given style, so that pprint methods may use
functions like `write'. ")
   (input "A lisp object and optional STYLE.")
   (value "undefined"))
  (let ((outstream
	 (if stream-p
	     (case stream
	       ((nil) *standard-output*)
	       ((t) *terminal-io*)
	       (otherwise stream))
	     *standard-output*))
	(realstyle
	 (cond ((null style-p))
	       ((pp~style-p style) style)
	       ((and (symbolp style) (pp~find-style style)))
	       (t (error "~S is not a style." style)))))
    (if (null style-p)
	(pprint obj outstream)
	(let ((dispatch-fn (pp=dispatch-fn obj realstyle))
	      #+old(*print-pprint-dispatch*
		    (pp=style-pprint-dispatch realstyle))
	      (pp*current-style realstyle))
	  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
		)
	  (funcall dispatch-fn outstream obj)))))

(defun pp~pprint (obj &optional (style (or pp*current-style
					   (pp~find-style 'pp+top)))
		      (stream *standard-output*))
  (declare
   (authors nesmith)
   (effect  "PPRINT OBJ using STYLE (optional) to STREAM (optional).  
STYLE defaults to either pp*current-style (if not null) or the style PP+TOP to
STREAM (defaults to *standard-output*).  

If STYLE is specified and null, the ordinary pprint table is
used (just like calling pprint).  During the printing, *print-pprint-dispatch*
is bound to the dispatch table of the given style, and *print-pretty* is
bound to T, so the pprint functions can confidently use functions like `write'.
Also binds pp*current-style to the given style.

If STREAM is specified and null, *standard-output* is used; if specified and
T, then *terminal-io* is used.  Otherwise STREAM should be a STREAM acceptable
for use with PPRINT.")
   (input "A lisp object and optional STYLE.")
   (value "undefined"))
  (let* ((realstyle
	  (cond ((pp~style-p style) style)
		((null style) nil)
		((and (symbolp style) (pp~find-style style)))
		(t (error "~S is not a style." style))))
	 (outstream
	  (cond ((null stream) *standard-output*)
		((eq t stream) *terminal-io*)
		(t stream)))
	 (pp*current-style realstyle)
	 (*print-pprint-dispatch*
	  (if (null realstyle) 
	      (copy-pprint-dispatch nil)
	      (pp=style-pprint-dispatch realstyle))))
    (pprint obj outstream)))


(defun pp~pprint-to-string (obj &optional (style pp*current-style)
				(string ""))
  (declare
   (authors nesmith)
   (input "An object, an optional STYLE and optional STRING.")
   (effect "The object is pp~pprint'd using the STYLE (defaults to
           pp*current-style) and the output is appended to the input string
           (defaults to empty string).")
   (value "The string with the appended output."))
  (concatenate 'string string
	       (with-output-to-string (out)
		 (pp~pprint obj style out))
	       ))

(defun pp=dispatch-fn (obj style)
  (declare
   (authors nesmith)
   (input "An object and a style")
   (value "The object is looked up in the style's pprint-dispatch table,
and if a matching method is found, it is returned.  If only a default
method is returned, then it is returned in a closure that switches its
two arguments around."))
  (multiple-value-bind (fn foundp)
      (pprint-dispatch obj (pp=style-pprint-dispatch style))
    (if foundp 
	fn
      #'(lambda (s o) 
	  (funcall fn o s)))))
	  

(defun pp~pprint-table (&rest args)
  (declare
   (authors nesmith)
   (input "A list like {\\vb (obj1 desc1 \\ldots objn descn)} where
obji are the objects to be printed, and desci describes how it should
appear. The syntax of the descriptions is as follows:
\\begin{code}
({\\it width} {\\it orientation} {\\it prefix} {\\it suffix} {\\it function})
\\end{code}
{\\it width} must be an integer specifying how wide the field is in which
the object should be printed. {\\it orientation} is one of :l, :c, :r or
nil, specifying whether the object should be left-, center- or right-justified
in its field (nil just prints the object). {\\it prefix} is a prefix that
should be printed before the object on each line, and {\\it suffix} a
suffix that should be printed after the object.  {\\it function} is a
function that should be used to print the object; when nil,  then
{\\vb PPRINT} will be used.  The
function specified should use take two arguments, a stream and the object
to be printed.  All parts of the description except for the width can
be omitted, and they will default to nil.  The prefixes and suffixes are
not counted toward the width of each field.  

PP~PPRINT-OBJ-WITH-DESC is called to print each object.")
    (effect "Prints the objects with the descriptions to standard output.")
    (value "not defined.")
    (example "{\\vb (pp~pprint-table 'ABC '(7 :l \"x\" \"y\") 'DEF '(5 :c nil \"z\"))} results in
\\begin{code}
xABC    y DEF z
\\end{code}"))
  (when args
    (unless (evenp (length args))
      (error "~S is not an even number of args."
	     args))
    (let ((descs nil)
	  (objs nil)
	  (streams-and-numlines nil)
	  (widths nil)
	  (streams nil)
	  (numlines nil)
	  (prefixes nil)
	  (suffixes nil))
      (dotimes (i (length args))
	(if (evenp i) (push (nth i args) objs) 
	  (push (nth i args) descs) ))
      (setq descs (nreverse descs) 
	    objs (nreverse objs)
	    widths (mapcar #'first descs)
	    prefixes (mapcar #'third descs)
	    suffixes (mapcar #'fourth descs))
      (setq streams-and-numlines
	(mapcar #'pp~pprint-obj-with-desc objs descs))
      (setq streams (mapcar #'(lambda (x) (make-string-input-stream 
					   (car x)))
			    streams-and-numlines))
      (setq numlines (apply #'max (mapcar #'cdr streams-and-numlines)))
      (do* ((numlines numlines (1- numlines))
	    (lines
	     (unless (zerop numlines)
	       (mapcar #'(lambda (x w) 
			   (read-line x nil (make-string w :initial-element
							 #\space)))
		       streams widths))
	     (unless (zerop numlines)
	       (mapcar #'(lambda (x w) (read-line x nil 
						  (make-string w 
							       :initial-element #\space)))
		       streams widths))))
	  ((zerop numlines)(values))
        (mapc #'(lambda (pre lin suf)
		  (format t "~A~A~A" (or pre "") lin (or suf "")))
		  prefixes lines suffixes)
	(terpri)))))

(defun pp~pprint-obj-with-desc (obj desc)
  (declare
   (authors nesmith)
   (input "An object and a description as described in documentation for
{\\vb pp~pprint-table}. ")
   (value "Returns a cons, with the car being a string containing the
output, and cdr being the number of lines it required to print.  In this
function, prefix and suffix are ignored."))
  (let* ((*print-right-margin* (car desc))
	 (orientation (cadr desc))
	 (write-function (fifth desc))
	 (str1 (with-output-to-string (out)
		(if write-function
		    (funcall write-function out obj)
		    (pprint obj out))))
	 (numlines 0))
    (cons 
	(with-output-to-string (out)
	  (with-input-from-string (in str1)
	    (let ((line nil))
	      (loop (setq line (read-line in nil :eof))
		    (when (eq line :eof) (return))
		    (incf numlines)
		    (case orientation
		      (:l (format out 
				  (format nil "~~~DA~%" (car desc))
				  line))
		      (:r (format out
				  (format nil "~~~D@A~%" (car desc))
				  line))
		      (:c (format out (format nil "~~~DA~%" (car desc))
				  (format nil
					  (format nil "~~~D@A"
						  (max (length line)
						       (round (+ (length line)
								 (/ (- (car desc)
								       (length line))
								    2)))))
					  line)))
		      (otherwise (format out "~A~%" line)))))))
	numlines)))

#+test
(progn
  (defun test-print-list (s l)
    (let ((*standard-output* s))
      (princ "(LIST")
      (dolist (item l (progn (princ ")") (terpri) t))
	(princ " ")
	(pp~pprint item))))
  (pp~defstyle test :parent pp+top 
	       :pprint-methods  
	       ((list test-print-list))
	       :help "Test")
  (setq test-style (pp~find-style 'test))
  (pp~pprint 7 test-style)
  (pp~pprint '(1 2 3) test-style)
  (defun test1-print-int (s i) (format s "~R" i))
  (pp~defstyle test1 :parent test 
	       :pprint-methods 
	       (((integer 2 5) test1-print-int))
	       :help "Test1"
	       )
  (setq test1-style (pp~find-style 'test1))
  (pp~pprint 7 test1-style)
  (pp~pprint 3 test1-style)
  (pp~pprint '(1 2 3) test1-style)
  (pp~defstyle test2 :parent test 
	       :pprint-methods 
	       (((integer 2 5) nil))
	       :help "Test2"
	       )
  (setq test2-style (pp~find-style 'test2))
  (pp~pprint 7 test2-style)
  (pp~pprint 3 test2-style)
  (pp~pprint '(1 2 3) test2-style)

)



