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

(mod~defmod poly :uses (keim mod sym term type )
	    :documentation "Definitions of polymorphic symbol classes."
	    :exports (
		      poly+poly
		      poly+polyvar
		      poly+polyconst
		      poly~instances
		      poly~set-instances!
		      poly~initial-type
		      poly~p
		      poly~copy
		      poly~instantiate
		      poly~env-lookup
		      poly~make-poly-sym
		      )
	    )

#{\section{ Polymorphic symbols}\label{mod:poly}

Polymorphic symbols are those that have variable types.  For example,
the equality operator $=$ can be instantiated at every type.  It would
be impossible to declare every instantiation of this operator, so we
use variable types. $=$ is declared with type ``(0 AA AA)'', with the
intention that the type ``AA'' will be instantiated whenever a formula
containing $=$ is parsed.  

Polymorphic symbols are just like normal symbols, except that they are 
also a subclass of the {\vb POLY+POLY} class. They contain, therefore, 
a slot {\vb INSTANCE-OBJ}, which is a structure that records all the
instances of the polymorphic term used in the current environment. 
That is, in each instance of a polymorphic symbol you can find all its
instances.  This is useful in minimizing redundant copies.  

The polymorphic symbols are created in the following way.  When a symbol
is read in (whether constant or variable), it is created as defined in
the {\vb TERM} module.  In this module we define an :after method on
{\vb INITIALIZE-INSTANCE}.  This :after method checks each newly created
symbol and, if it does not have a ground type, it changes its class to
the appropriate polymorphic class and initializes its {\vb INSTANCES}.

When later a ground version of this symbol is required, its instances will
first be searched to see if any are already the appropriate type.  If yes,
then that already-existing instance will be used to reduce redundancy.
If no, then a new instance, differing only in type, will be created and
pushed into the list of instances.  {\vb POLY~MAKE-POLY-SYM} is the
function that does this.
#}

(eval-when (load compile eval)
  ;; this structure makes it easier to maintain the property that 
  ;; the instances of a poly+poly have an eq instance-obj slot.
  (defstruct poly=instance-obj-struct
    (instances nil)
    (initial-type nil))
  (defclass poly+poly (keim+object)
    ((instance-obj :initarg :instance-obj 
		   :initform (make-poly=instance-obj-struct :instances nil
							    :initial-type nil)
		   :accessor poly=instance-obj
		   :documentation "A list of all instantiated versions of this polymorphic symbol."))
    (:documentation "A mixin for polymorphic symbols."))
  (defclass poly+polyvar (poly+poly sym+var)
    ()
    (:documentation "A mixin for polymorphic symbols."))
  (defclass poly+polyconst (poly+poly sym+const)
    ()
    (:documentation "A mixin for polymorphic symbols.")))

(defmethod print-object ((instance poly=instance-obj-struct) stream)
  (format stream "#<poly=instance-obj-struct >"))
     
(defgeneric poly~instances (poly)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "A polymorphic symbol." )
 	   (effect  "None." )
	   (value   "The instances of this polymorphic symbol in the"
		    "environment where it was defined.")
	   (example "consider for instance the polymorphic symbol {\bv FORALL} in an environment that"
		    "occurs in the forms (forall (lam (P (O I)) ...)) and (forall (lam (x I) ...))"
		    "forall --> (forall forall), where the type of the"
		    "first forall is (((I) -> O) -> O) -> O and of the"
		    "second forall is ((I) -> O) -> O"))
  (:method ((poly poly+poly))
	   (poly=instance-obj-struct-instances (poly=instance-obj poly))))


(defgeneric poly~set-instances! (poly instances)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "A polymorphic symbol and a list of instances of it." )
 	   (effect  "The instances slot of the symbol is set to the given value." )
	   (value   "Undefined.")
	   (example "Let {\\vb FORALL} be a polymorphic object and {\\vb FORALL1} and {\\vb FORALL2}"
		    "two instances"
		    "FORALL    (list FORALL1 FORALL2)  --> the instances of {\\vb FORALL} are set to the"
  		    "two instances {\\vb FORALL1} and {\\vb FORALL2}"))
  (:method ((poly poly+poly) (instances list))
      (setf (poly=instance-obj-struct-instances 
	     (poly=instance-obj poly))
	instances)))

(defgeneric poly~initial-type (poly)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "A polymorphic symbol." )
 	   (effect  "None." )
	   (value   "The initial type of this symbol, when it was originally defined.")
	   (example "Let forall be polymorphical defined with type ((AA) -> o) -> o"
		    "and forall1 be an instantiation with type (((I) -> O) -> O) -> O"
		    "forall1 --> ((AA) -> o) -> o"))
  (:method ((poly poly+poly))
	   (poly=instance-obj-struct-initial-type (poly=instance-obj poly))))



(defun poly~p (thing)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "Any lisp object." )
 	   (effect  "None" )
	   (value   "T if the object is a polymorphic, nil otherwise.")
	   (example "Let forall be polymorphical defined with type ((AA) -> o) -> o"
		    "forall --> T"))
  (typep thing 'poly+poly))


(defun poly~copy (poly)
  (declare (authors nesmith)
	   (input "A polymorphic symbol POLY.")
	   (effect "none")
	   (value "A new instance of this class with slots initialized to the same as those of POLY."))
  (make-instance (class-name (class-of poly))
    :type (term~type poly)
    :symbol (sym~shared-symbol poly)
    :instance-obj (poly=instance-obj poly)))

(defmethod term~copy ((poly poly+poly))
  (poly~copy poly))

(defun poly~instantiate (poly type)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "A polymorphic symbol and a formula type." )
 	   (effect  "None" )
	   (value   "Checks to see if there is an instance of this polymorphic symbol with the given type."
		    "If so, returns it, otherwise creates a new instance with the new type and returns it.")
	   (example "Let forall be polymorphical defined with type ((AA) -> O) -> O"
		    "forall     ((II) -> O) ->O -->  forall with type  ((II) -> O) ->O"
		    "forall I --> Error: The given type i doesn't match ...."))
  (or (find-if #'(lambda (x) (keim~equal type (term~type x)))
	       (poly~instances poly))
      (if (type~unify type (poly~initial-type poly))
	(let ((new-term
	       (poly~copy poly)))
	  (term~set-type! new-term type)
	  (poly~set-instances! poly (adjoin new-term (poly~instances poly)))
	  new-term)
	(error "The given type ~S doesn't match the type of the polymorphic symbol ~A to be instantiated (~S)" type poly (poly~initial-type poly)))))

(defun poly~env-lookup (key env type)
  (declare (edited  "01-MAR-1993 12:59")
	   (authors RICHTS)
	   (input   "A key, an environment and a type.")
	   (effect  "")
	   (value   "If KEY is associated with a polymorphic symbol in ENV, then its instance for"
		    "TYPE is returned."
		    "Else (if it is associated with a normal symbol) this symbol is returned.")
	   (example "Let forall be polymorphical defined with type ((AA) -> O) -> O in environment ENV"
		    "'forall      ENV      (((i) -> i) -> o) -> o --> forall with type"
		    "(((i) -> i) -> o) -> o"))
  (let ((symbol (term~env-lookup key env)))
    (if (poly~p symbol)
	(poly~instantiate symbol type)
	symbol)))

;; don't need special read function, just automatically make a new poly
;; symbol whenever not ground

(defmethod initialize-instance :after ((term sym+sym) &rest initargs)
  (declare (ignore initargs))
  (if (type~ground-p (term~type term)) 
      term
      (let ((polyterm (poly~make-poly-sym term)))
	(setf (poly=instance-obj-struct-initial-type 
	       (poly=instance-obj polyterm))
	      (term~type term))
	polyterm)))

(defgeneric poly~make-poly-sym (term)
  (declare (edited  "9-Feb-1993")
	   (authors nesmith )
	   (input   "A term.")
 	   (effect  "Makes the term a polymorphic term if appropriate.")
	   (value   "The new term.")
	   (example "Let X be a (non-poly) variable of type I,e.g.,"
		    "X --> X, but now X is a poly term, i.e. (poly~p X) --> T,"
		    "of type I."))
  (:method ((term sym+sym))
	   term)
  (:method ((term poly+poly))
	   (unless (find (term~type term)
			 (poly~instances term)
			 :key #'term~type
			 :test #'keim~equal)
	     (poly~set-instances! term (adjoin term
					       (poly~instances term))))
	   term)
  (:method ((term sym+const))
      (change-class term 'poly+polyconst))
  (:method ((term sym+var))
      (change-class term 'poly+polyvar)))

(defmethod type~subst-apply ((subst type+substitution) (term poly+polyconst))
  (let ((newtype (type~subst-apply subst (term~type term))))
    (poly~instantiate term newtype)))

(defmethod type~subst-apply ((subst type+substitution) (term poly+polyvar))
  (let ((newtype (type~subst-apply subst (term~type term))))
    (poly~instantiate term newtype)))


