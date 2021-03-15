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
 
(mod~defmod com :uses (mod sys keim help arg)
	    :exports (
		      com~command-hash-table
		      com+category
		      com~cat-supercategories
		      com~cat-subcategories
		      com+command
		      com~argnames
		      com~argtypes
		      com~arghelps
		      com~function
		      com~help
		      com~categories
		      com~default-fn
		      com~find-category
		      com~find-command
		      com~command-p
		      com~category-p
		      com~defcategory
		      com~defcommand
		      com+abort-command
		      com~apply-defaults
		      com~unspecified
		      com~specified-arg-p
		      )
	    )

#{
\section{Commands}
 A command is, conceptually, a function to be applied by a user,
 but provides an interface over the actual function. This interface
 allows one to specify the types the arguments should have and to
 describe the arguments to the user. 

 We also allow commands to be grouped in categories, so that they
 can be kept track of and/or appropriately displayed to the user.
#}

;; We'll use this hash table to keep track of all the commands that
;; exist at any given time 
(eval-when (load compile eval)
(defvar com*command-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by command name, that holds all existing commands.")

(defvar com*category-hash-table (make-hash-table :test #'equal)
  "Hash table, indexed by category name, that holds all existing command
categories.")
)

(defun com~command-hash-table ()
  (declare (authors nesmith)
	   (input "None")
	   (value "The hash table where all commands are stored, keyed by 
command name (stored as strings, so packages makes no difference)."))
  com*command-hash-table)


(eval-when (load compile eval)
#{
\subsection{Categories}
 A command category is a way to group commands in a logical
 fashion, so that a hierarchy of commands can be defined.  One may
 want to define {\vb system} commands or commands pertaining to a specific
 prover, etc.  One may then break down this category into commands
 relating to natural deduction proving, or to commands that do
 printing, etc.  This information does not really affect how the
 command works, but is merely available if a program wishes to take
 advantage of it.
#}
(defclass com+category (help+help keim+name)
  ((direct-supercategories 
    :initarg :direct-supercategories
    :accessor com=cat-direct-supercategories
    :documentation "Direct supercategories of this one.")
   (direct-subcategories :initarg :direct-subcategories :initform nil
			 :accessor com=cat-direct-subcategories
			 :documentation "Direct subcategories of this one."))
  (:documentation "General class of command categories."))

(defgeneric com~cat-supercategories (cat)
  (declare
   (authors nesmith)
   (input "A category.")
   (value "The category's direct supercategories."))
  (:method ((cat com+category))
      (com=cat-direct-supercategories cat)))

(defgeneric com~cat-subcategories (cat)
  (declare
   (authors nesmith)
   (input "A category.")
   (value "The category's direct subcategories."))
  (:method ((cat com+category))
      (com=cat-direct-subcategories cat)))

#{
 \subsection{Commands}
 Each {\vb com+command} object describes how a command is defined.
 The slot ARGNAMES is a list of names of the arguments that the
 command requires; these are used when interacting with the user.
 ARGTYPES are argument types (defined by {\vb arg~deftype}) corresponding
 to the ARGNAMES.  These are used in prompting the user for the
 arguments.  ARGHELPS is a list of strings which describe what each
 argument is.  FUNCTION is a function which will carry out the
 command when given the number and type of arguments specified.
 CATEGORIES is a list of command categories that this command to
 which this command belongs.
 DEFAULT-FN is a symbol naming a function, which when called with a list of arguments (same number
 as the command takes), and some of which may be unspecified, computes defaults 
 for (some of) the unspecified arguments.  
If DEFAULT-FN is NIL, then no defaulting  will be done.
#}

(defclass com+command (help+help keim+name)
  ((argnames :initarg :argnames :initform nil :reader com=argnames
	     :documentation "Names of the arguments for this function.
A list of symbols.")
   (argtypes :initarg :argtypes :initform nil :reader com=argtypes
	     :documentation "Types of the arguments. A list of symbols.")
   (arghelps :initarg :arghelps :initform nil :reader com=arghelps
	     :documentation "Help strings for the arguments.")
   (function :initarg :function :reader com=function
	     :documentation "Function that carries out the command.
Anything suitable as an argument for funcall or apply.")
   (categories :initarg :categories :initform nil :reader com=categories
	       :documentation "The categories under which this command
falls.  A list of category names (symbols).")
   (default-fn :initarg :default-fn :initform nil :reader com=default-fn
	       :documentation
	       "A function for computing argument defaults."))
  (:documentation "General class of commands."))
)

(defgeneric com~argnames (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument names for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=argnames com)))

(defgeneric com~argtypes (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument types for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=argtypes com)))

(defgeneric com~arghelps (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "Argument help strings for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=arghelps com)))

(defgeneric com~function (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The function for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (com=function com)))

(defgeneric com~help (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The help string for the command.")
   (effect "None"))
  (:method ((com com+command))
	   (help~help-string com)))

(defgeneric com~categories (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "The categories this command belongs to.")
   (effect "None"))
  (:method ((com com+command))
	   (com=categories com)))


(defgeneric com~default-fn (command)
  (declare
   (authors nesmith)
   (input "A command.")
   (value "This command's default function.")
   (effect "None"))
  (:method ((command com+command))
    (com=default-fn command)))


(eval-when (load compile eval)
(defun com~find-category (string-or-symbol)
  (declare
   (authors nesmith)
   (input "A string or symbol.")
   (value "The category with the given name or NIL if none exists.")
   (effect "None"))
  (gethash (etypecase string-or-symbol 
	     (string string-or-symbol)
	     (symbol (symbol-name string-or-symbol)))
	   com*category-hash-table))
)

(defmethod print-object ((argtype com+command) stream)
  (format stream "#<com+command ~A>" (keim~name argtype)))

(eval-when (load compile eval)

(defun com~find-command (string-or-symbol)
  (declare
   (authors nesmith)
   (input "A string or symbol.")
   (value "The command with the given name or NIL if none exists.")
   (effect "None"))
  (gethash (etypecase string-or-symbol 
	     (string string-or-symbol)
	     (symbol (symbol-name string-or-symbol)))
	   com*command-hash-table))

(defun com~command-p (thing)
  (declare
   (authors nesmith)
   (input "Anything.")
   (value "T if the argument is a command, otherwise NIL.")
   (effect "None"))
  (typep thing 'com+command))
)

(defmethod print-object ((argtype com+category) stream)
  (format stream "#<com+category ~A>" (keim~name argtype)))

(defun com~category-p (thing)
  (declare
   (authors nesmith)
   (input "Anything.")
   (value "T if the argument is a command category, otherwise NIL.")
   (effect "None"))
  (typep thing 'com+category))

(eval-when (load compile eval)
(defmacro com~defcategory (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an com+category.
Here is an example of the syntax:
\\begin{code}
(com~defcategory my-arithmetic
  (direct-supercategories real-arithmetic)
  (help \"My arithmetic commands.\"))
\\end{code}")
           (effect  "Read the category, construct a real com+category.")
           (value   "The new category."))
  (let ((help "")
	(direct-supercategories nil)
	(direct-supercategories-names nil)
	(classname (intern (format nil "COM+CATEGORY__~A" name))))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
	  (let ((carattrib (if (symbolp (car attrib)) 
                               (symbol-name (car attrib)) 
                  (car attrib))))
            (cond ((string-equal carattrib :help) (setq help (cadr attrib)))
                  ((string-equal :direct-supercategories carattrib)
                   (setq direct-supercategories (cdr attrib)))
                  (t
 	           (error ";;;com~~defcategory: Not expecting ~S" attrib))))))
    (let ((badcat (find-if-not #'com~find-category direct-supercategories)))
      (when badcat
	(error ";;;com~~defcategory ~A: Category ~S does not refer to an
existing category." name badcat)))
    (setq direct-supercategories-names
	  (mapcar #'(lambda (x) (class-name (class-of (com~find-category x))))
		  direct-supercategories))
    (unless direct-supercategories-names
      (push 'com+category direct-supercategories-names))
  `(block nil
     (when (com~find-category ',name)
       )
    (defclass ,classname (,@direct-supercategories-names)
      ()
      (:documentation ,help))
    (let* ((new-category
	     (make-instance ',classname
	       :name ',name 
	       :help ',help
	       :direct-supercategories
	       ',direct-supercategories)))
       (setf (gethash ',(symbol-name name) com*category-hash-table)
	 new-category)
       (dolist (supname ',direct-supercategories)
	 (let ((sup (com~find-category supname)))
	   (push ',name (com=cat-direct-subcategories sup))))
       new-category))))

(defmacro com~defcommand (name &rest attribs)
  (declare (edited  "28-JUL-92 10:00")
           (authors NESMITH)
           (input   "A written representation of an com+command.
Here is an example of the syntax:
\\begin{code}
(com~defcommand divide
  (argnames dividend divisor)
  (argtypes number posinteger)
  (arghelps \"Number to divide\" \"Number by which to divide\")
  (function /)
  (defaults (0 1))
  (help \"Divide one number by a positive integer.\")
  (categories my-arithmetic))
\\end{code}
Any categories and argtypes used must already be defined.

The defaults slot should have the following syntax:
\\begin{code}
(defaults {\\it fn-name}| {(lambda (arg1 \\ldots argn) . {\\it body})} 
         |  ({\\it default1 \\ldots defaultn}) )
\\end{code}
A single symbol will be interpreted as the name of a function to call.  A lambda expression will be
expanded into a function definition.  A list of defaults will be interpreted as the defaults for the
arguments, which will be evaluated at run-time, not at definition time, if they are not already specified.  
In such a list, you can specify an ``unspecified'' argument
by a call to the function com~unspecified (with no arguments), e.g.,  (defaults ((com~unspecified) 1)).")
           (effect  "Read the command, construct a real com+command.")
           (value   "The new command."))
  (let ((argnames nil)
	(argtypes nil)
	(arghelps nil)
	(function nil)
	(help "")
	(categories nil)
        (default-fn nil))
    (do ((attribs (cdr attribs) (cdr attribs))
	 (attrib (car attribs) (car attribs)))
	((and (null attrib) (null attribs)))
      (if (consp attrib)
          (let ((carattrib (if (symbolp (car attrib))
                               (symbol-name (car attrib))
                           (car attrib))))
	  (cond 
            ((string-equal carattrib :argnames) (setq argnames (cdr attrib)))
	    ((string-equal carattrib :argtypes) (setq argtypes (cdr attrib)))
	    ((string-equal carattrib :arghelps) (setq arghelps (cdr attrib)))
	    ((string-equal carattrib :function) 
	     (setq function (cadr attrib)))
	    ((string-equal carattrib :help) (setq help (cadr attrib)))
	    ((string-equal carattrib :categories) 
             (setq categories (cdr attrib)))
            ((string-equal carattrib :defaults) (setq default-fn (com=read-default-spec (cadr attrib) name)))
	    (t
	     (error ";;;com~~defcommand: Not expecting ~S" attrib))))))
    (unless (symbolp function)
      (error ";;;com~~defcommand: Function ~S must be a symbol." function))
    (unless (= (length argnames) (length argtypes))
      (error ";;;com~~defcommand ~A: Length of Argnames and Argtypes do not match." name))
    (let ((badname (find-if-not #'symbolp argnames)))
      (when badname
	(error ";;;com~~defcommand ~A: Argname ~S must be a symbol." name 
	       badname)))
    (let ((badtype (find-if-not #'arg~find-argtype argtypes)))
      (when badtype
	(error ";;;com~~defcommand ~A: Argtype ~S does not refer to an
argument type." name badtype)))
    (let ((badhelp (find-if-not #'stringp arghelps)))
      (when badhelp
	(error ";;;com~~defcommand ~A: Help ~S must be a string." 
	       name badhelp)))
    (let ((badcat (find-if-not #'com~find-category categories)))
      (when badcat
	(warn ";;;com~~defcommand ~A: Category ~S is not a known category."
		 name 
	       badcat)))
    (unless (equal argnames 
		   (remove-duplicates argnames))
      (error ";;;com~~defcommand ~A: Argnames ~S must be unique." name argnames))
  `(block nil
     (when (com~find-command ',name)
       )
     ,(if (symbolp default-fn) nil default-fn)
     (let* ((new-command
	     (make-instance 'com+command
	       :name ',name 
	       :argtypes ',argtypes
	       :argnames ',argnames
	       :arghelps ',arghelps	       
	       :help ',help
	       :function ',function
	       :categories ',categories
	       :default-fn ',(if (symbolp default-fn) default-fn (second default-fn)))))
       (setf (gethash ',(symbol-name name) com*command-hash-table)
	 new-command))
     )))
)

(eval-when (compile load eval)
#{
\subsection{ Some useful categories and commands}
 The category {\vb top} is defined to act as an uppermost category. Every
 command belongs to category {\vb top}.
#}

(com~defcategory top
  (help "An uppermost level for commands."))

#{The category {\vb system} is used for commands that deal with the Lisp
 process itself. 
#}
(com~defcategory system
  (direct-supercategories top)
  (help "Commands which are related to the Lisp system itself."))

#{ The {\vb exit-lisp} command will halt the
 Lisp completely.
#}

(com~defcommand exit-lisp
  (function sys~exit-from-lisp)
  (help "Exit from the Lisp completely.")
  (categories system))
)

#{ {\vb com+abort-command} is a new condition type that is used when
 aborting a command.
#}

(sys~define-condition com+abort-command (sys+abort)
  (command)
  (lambda (condition stream)
	     (format stream ";;;Aborting command ~A~%" 
		     (keim~name (com+abort-command-command condition)))))

#{ We define the {\vb command} argument type. #}


(arg~deftype command
 (read-function com~read-command)
 (predicate com~command-p)
 (help "a command"))


(defmethod com~read-command ((thing com+command) &rest others)
  (declare (ignore others))
  thing)

(defmethod com~read-command ((thing symbol) &rest others)
  (declare (ignore others))
  (let ((com (com~find-command thing)))
    (if com 
	com
	(arg~signal-wrong-type 'command thing))))

(defmethod com~read-command ((thing string) &rest others)
  (declare (ignore others))
  (let ((com (com~find-command thing)))
    (if com 
	com
	(arg~signal-wrong-type 'command thing))))

#{
\subsection{Using defaults for command arguments}
#}

(defun com~apply-defaults (command arglist)
  (declare
   (authors nesmith)
   (input "A command and a list of arguments to the command.")
   (value "The result of calling the command's default-fn on the given arguments.  If the command does
not have a default function, the list is returned unchanged."))
  (let ((default-fn (com~default-fn command)))
    (if default-fn
	(apply default-fn arglist)
      arglist)))

(eval-when (compile load eval)
  (defvar com*unspecified (cons nil nil)))

(defun com~unspecified () 
  (declare
   (authors nesmith)
   (value "The designated undefined argument (guaranteed to be the only object that is not 
com~specified-arg-p)."))
  com*unspecified)

(defun com~specified-arg-p (arg)
  (declare
   (authors nesmith)
   (input "Any lisp object.")
   (value "True if this object is not the same as (com~unspecified). Used to tell specified arguments
to functions from unspecified."))
  (not (eq arg (com~unspecified))))

(defun com=read-default-spec (default-spec name)
  (declare
   (authors nesmith)
   (input "a specification of a default function, and symbol NAME")
   (effect "when DEFAULT-SPEC is a symbol, returns it.  If DEFAULT-SPEC is
a lambda-form, returns a {\\vb defun} using the NAME and the body of the
DEFAULT-SPEC.  Otherwise, it is assumed that DEFAULT-SPEC is a list of
forms, one for each argument, with the defaults for them.")
   (example "\\begin{code}
(com=read-default-spec 'foo 'read-fun) -> FOO
(com=read-default-spec '(lambda (x y) (body y x)) 'read-fun) ->
                 (DEFUN READ-FUN (X Y) (BODY Y X))
(com=read-default-spec '(1 2 3) 'read-fun) ->
                    (DEFUN READ-FUN (&REST GIVEN-ARGS)
                      (MAPCAR #'(LAMBDA (GIVEN DEFAULT)
           		        (IF (COM~SPECIFIED-ARG-P GIVEN) GIVEN DEFAULT))
                         GIVEN-ARGS (LIST 1 2 3)))
\\end{code}
"))
  (cond ((symbolp default-spec) default-spec)
	((listp default-spec)
	 (let ((new-name (gensym (format nil "~A-DEFAULT-FN" name))))
	   (cond ((eq (car default-spec) 'lambda)
		  `(defun ,new-name ,@(cdr default-spec)))
		 (t
		  (com=build-default-fn default-spec new-name)))))
	(t (error "Can't read default-fn specifier ~S" default-spec))))

(defun com=build-default-fn (default-list name)
  `(defun ,name (&rest given-args)
     (mapcar #'(lambda (given default)
		 (if (com~specified-arg-p given) given default))
	     given-args (list ,@default-list))))
