;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
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

(mod~defmod env :uses (mod keim help)
	    :documentation
	    "Functions which define the environment interface."
	    :exports
	    (env+environment
	     env~p
	     env~lookup-object
	     env~classes-keys
	     env~class-keys
	     env~enter
	     env~remove
	     env~remove-plural
	     env~empty-p
	     env~query
	     env~create
	     env~post-print
	     env~copy
	     )
	    )

#{\section{Environments}

In any \keim\ application it is important to keep tract of the actual context of defined objects and to have a
mapping from internal objects to external names, the {\em keys}. This service is brought to you by \keim\
environments.  Since environments are used by most other modules in \keim\ they must be very basic objects and
cannot have any semantic concern for what it is storing. 

It turns out that many higher-level \keim\ objects like proofs, problems, inference rules and the like carry 
their own environment with them, so that in a typical \keim\ application there will be a multitude of
environments that differ only in small parts. In order conserve space and reduce copying \keim\ supports a
simple inheritance mechanism for environments. An environment may have a single parent environment from which
it inherits all objects and keys. Thus an application can have a tree-like inheritance structure of environments.#}

#{\subsection{General}#}

(eval-when (load compile eval)
  (defclass env+environment (help+help keim+object)
    ((storage :initarg :storage :reader env=storage 
	      :writer env=write-storage!
	      :initform (env=make-storage) :documentation
	      "Some kind of storage place.  Just what kind is determined by
the implementation.")
     (parent :initarg :parent :reader env=parent :writer env=write-parent!
		:initform nil 
		:documentation 
		"A superenvironment which is part of this one."))
    (:documentation "An environment is a way of providing a mapping
between a key and a KEIM object.  The environment just does lookups and
stores, without any semantic concern for what it is storing."))
)


(defun env~create (&optional (parent nil) (name nil) (help ""))
  (declare 
   (authors NESMITH)
   (input   "Optionally: an environment PARENT (default NIL), a symbol NAME
(default NIL)  and help-string HELP (default is empty string).")
   (effect  "Creates a new environment.")
   (value   "The new environment which has PARENT as parent, NAME as name and HELP as help string."
	    "This means that all objects in PARENT are also in the new environment."))
   (etypecase parent
    ((or env+environment null)
     (etypecase name
      (symbol
       (etypecase help
        (string 
         (make-instance 'env+environment :parent parent :name name :help help))))))))



(defun env~copy (env)
  (declare 
   (authors NESMITH)
   (input "An environment to be copied.")
   (value "A new environment with the same parent as the given one, and
which contains every key of the old environment associated to the
same object.  Does not copy the old environment's name or help property."))
  (let ((newenv (env~create (env=parent env))))
    (env=write-storage! (copy-alist (env=storage env))
			newenv)
    newenv))

(defun env~p (env)
  (declare 
   (authors NESMITH)
   (input   "An object.")
   (effect  "None.")
   (value   "T if this object is an environment, NIL otherwise."))
  (typep env 'env+environment))

#{\subsection{Retrieving objects from environments}#}


(defgeneric env~query (key env &key test)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an environment ENV. The keyword TEST, if given, should 
be a predicate which determines when two keys are equal.")
   (effect  "None.")
   (value   "The CLOS class of the object associated with KEY in ENV if any, NIL otherwise."))
  (:method ((key t) (env env+environment) &key (test #'eql))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj))))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj))))
  (:method ((key string) (env env+environment) &key (test #'string=))
	   (let ((obj (env~lookup-object key env :test test)))
	     (if (null obj)
		 nil
	       (class-of obj)))))
  

(defgeneric env~lookup-object (key env &key test)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an environment ENV. The keyword TEST, if given, should 
be a predicate which determines when two keys are equal.")
   (effect  "None.")
   (value   "The object associated with KEY in ENV if any, NIL otherwise."))
  (:method ((key t) (env env+environment) &key (test #'eql))
	   (env=simple-lookup key env :test test))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
	   (env=simple-lookup key env :test test))
  (:method ((key string) (env env+environment) &key (test #'string=))
	   (env=simple-lookup key env :test test)))

(defgeneric env~classes-keys (env classlist)
  (declare 
   (authors NESMITH)
   (input   "An environment ENV and a list of CLOS classes CLASSLIST.  
Each class is either an actual CLOS class object or a symbol which names the class.")
   (effect  "None.")
   (value   "The keys of all objects in the ENV whose class is among those those
in CLASSLIST."))
  (:method  ((env env+environment) (classlist list))
   (mapcan #'(lambda (class) (env~class-keys env class))
	   classlist)))
     
(defgeneric env~class-keys (env class)
  (declare 
   (authors RICHTS NESMITH)
   (input   "An environment ENV and a CLOS class CLASS, which is either an
actual class object or a symbol which names it.")
   (effect  "None.")
   (value   "The keys of all objects in the ENV whose class is CLASS."))
  (:method ((env env+environment) (class class))
      (let ((res nil))
	(dolist (pair (env=storage env) 
		      (remove-duplicates (append (nreverse res) 
						 (if (env=parent env)
						     (env~class-keys (env=parent env) class)
						     nil))
					 :from-end t))
	  (when (typep (cdr pair) class)
	    (push (car pair) res)))))
  (:method ((env env+environment) (class symbol))
      (env~class-keys env (find-class class)))
  )



(defgeneric env~empty-p (environment)
  (declare (edited  "05-AUG-1993 13:49")
	   (authors ACSEHN)
	   (input "An environment."  )
	   (effect "none." )
	   (value "T, iff the environment is empty."  ))
  (:method ((environment env+environment))
	   (let ((parent-env (env=parent environment)))
	     (and (if parent-env
		      (env~empty-p parent-env)
		      T)
		  (null (env=storage environment))))))

#{\subsection{Manipulating Environments}

Once created empty, environments grow when objects are entered into them by the {\vb ENV~ENTER} function.
This takes as arguments a key (a \lisp\ symbol or a string) 
and a \keim\ object. Since environments do not
care about the semantic content of the objects the association of a key for the object is entirely left to the
user. Note that in deduction system applications it is not customary and sensible to enter variables
into the environment, since members of an environment are not open to \lisp\ garbage collection. Indeed \keim\
variables are stored in their binding occurrences (quantors or abstractions).#}

(defgeneric env~enter (key object env)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an object OBJECT, an environment ENV, and possibly some keywords.")
   (effect   "The object is stored in ENV, associated with KEY.")
   (value  "The changed ENV."))
    (:method ((key symbol) (object keim+object) (env env+environment))
	   (env=simple-enter key object env))
    (:method ((key string) (object keim+object) (env env+environment))
	   (env=simple-enter key object env)))
  

(defgeneric env~remove (key env &key test)
  (declare 
    (authors NESMITH)
    (input   "A KEY and an environment ENV. The keyword TEST, if given, should be"
	     "a predicate which determines when two keys are equal.")
    (effect  "The first object found which is stored under KEY in ENV is removed if it is not in a parent of ENV."
	     "A warning is singnaled if the object is in a parent."
	     "No error is signaled if the KEY has no association in ENV.")
    (value   "The changed ENV."))
  (:method ((key symbol) (env env+environment) &key (test #'string=))
   (env=simple-remove key env :test test))
  (:method ((key string) (env env+environment) &key (test #'string=))
   (env=simple-remove key env :test test)))

(defgeneric env~remove-plural (list-of-keys env &key test)
  (declare 
    (authors NESMITH)
    (input   "A list of KEYS and an environment ENV. The keyword TEST, if given, should be"
	     "a predicate which determines when two keys are equal.")
    (effect  "The objects stored under the KEYS are removed sequentially from ENV."
	     "That is, two occurrences of the same KEY can result in two objects"
	     "being removed.  No error is signaled if the KEY has no association in ENV.")
    (value   "The changed ENV."))
  (:method ((list-of-keys list) (env env+environment) &key (test #'string=))
   (dolist (key list-of-keys env)
     (env~remove key env :test test))))
 
#| Now we decide how to actually implement this thing.  We need to
be able to put things on and pop them off like a stack, so our storage
has to be something like an alist.  Let's make it an alist. 
|#

; Here's where we actually define the lookup/enter functions
(defun env=make-storage () 
  (declare 
   (authors NESMITH)
   (input   "None.")
   (value "A new storage space for an environment.")
   (effect "None"))
  ()) 

(defun env=simple-lookup (key env &key (test #'eql))
  (declare 
   (authors NESMITH)
   (input   "A key, an environment and keyword TEST predicate (defaults to EQL).")
   (value "Looks up the KEY in the ENVIRONMENT using the given predicate, returns the object associated or NIL if none.  A second value is a boolean which
indicates if the key was found or not.")
   (effect "None"))
  (let ((pair (assoc key (env=storage env) :test test)))
    (cond (pair (values (cdr pair) t))
	  ((env=parent env) (env=simple-lookup key (env=parent env) :test test))
	  (t (values nil nil)))))

(defun env=simple-enter (key object env)
  (declare 
   (authors NESMITH)
   (input   "A key, an object and an environment.")
   (value "The changed environment.")
   (effect "The environment is changed to add the association between the KEY
and the OBJECT."))
  (env=write-storage! (acons key object (env=storage env))
		      env)
  env)

(defun env=simple-remove (key env &key (test #'eql))
  (declare 
   (authors NESMITH)
   (input   "A key, an environment and a keyword predicate TEST (defaults 
to EQL.")
   (value "The changed environment.")
   (effect "The environment is changed to remove the most recent association 
which this key has in the environment if this association is not in a parent of ENV."))
  (when (and (not (assoc key (env=storage env) :test test))
	     (env=parent env)
	     (env=simple-lookup key (env=parent env) :test test))
    (warn "The key ~A which shall be removed is only in a parent of the environment. It is not removed." key))
  (env=write-storage! (remove key (env=storage env) :key #'car :test test
			      :count 1)
		      env)
  env)

#{\subsection{Miscellaneous}#}

(defmethod print-object ((env env+environment) stream)
  (format stream "#<ENV ~{~S ~} ~A>" 
	  (mapcar #'car (env=storage env))
	  (if (env=parent env) (env=parent env) "")))

#{The next function is simply a result of the module conventions of \keim, it carries no significance for the
user. The {\vb post} module must use environments, but environments must have a \post\ representation, So here
it is.#}

(defgeneric env~post-print (key object stream)
  (declare 
   (authors NESMITH)
   (input   "A KEY, an OBJECT with which this key is associated and a STREAM.")
   (effect  "Outputs, to STREAM, a suitable environment declaration for this
key and object type.")
   (value   "Undefined.")))





#{\subsection{Example session}
We give an example of a \keim\ session that uses some of the interface functions in this module.

\begin{code}
KEIM(1): (setq env1 (env~create))
#<ENV  >
KEIM(2): (env~empty-p env1)
T
{\em we create an environment and see that it is created empty, then we create and enter some named \keim\ objects. 
Note that the choice of keys for the objects is fully at the whim of the user.}
KEIM(3):  (setq test (make-instance 'keim+name :name 'test))
#<KEIM+NAME @ #xbc492e>
KEIM(4): (env~enter 'test test env1)
#<ENV TEST  >
KEIM(5): (env~enter 'tast test env1)
#<ENV TAST TEST  >
{\em now we create another environment that inherits everything from the first one, this time we choose to
give it a name and a help string, then we make and enter a \keim\ object into the new environment.}  
KEIM(7): (setq env2 (env~create env1 `son "Help String"))
#<ENV  #<ENV TAST TEST  >>
KEIM(8): (env~query 'tast env2)
#<KEIM+NAME TEST>
KEIM(9): (setq help1 (make-instance 'help+help :name 'tust :help "Help object for testing"))
#<HELP+HELP TUST>
KEIM(10): (env~enter 'tust help1 env2)
#<ENV TUST  #<ENV TAST TEST  >>
KEIM(11): (env~lookup-object 'tust env2)
#<HELP+HELP TUST>
T
KEIM(111): (env~query 'tust env1)
NIL
{\em {\vb TUST} is a member of {\vb ENV2} but not of {\vb ENV1}}
KEIM(113): (env~remove 'tast env2)
Warning: The key TAST which shall be removed is only in a parent of 
the environment. It is not removed.
#<ENV TUST  #<ENV TAST TEST  >>
KEIM(114): (env~remove 'tast env1)
#<ENV TEST  >
KEIM(115): env2
#<ENV TUST  #<ENV TEST  >>
KEIM(117): (env~class-keys env2 'help+help)
(TUST)
\end{code}
#}


