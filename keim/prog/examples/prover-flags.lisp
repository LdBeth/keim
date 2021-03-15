(in-package :resprover)

;;; We set up a list of flags, which can then be interactively changed
;;; using the SET-FLAG command.
;;; This is pretty barebones.  You could think about defining a flag
;;; to have a certain range of possible values or certain types.

(defvar *flags*
  (list  '*report-generated* 
	 '*report-kept* 
	 '*use-unit-resolution*
	 '*use-unit-preference*
	 '*use-input-resolution* 
	 '*use-forward-subsumption* 
	 '*use-backward-subsumption* 
	 '*use-factoring*)
  "A list of symbols, all of which are flags to be set for the prover.")

(eval-when (load compile eval)
(arg~deftype flag
 (predicate flag-p)
 (read-function read-flag)
 (help "a flag"))
)

(defmethod read-flag ((flag symbol) &rest others)
  (declare (ignore others))
  (read-flag (symbol-name flag)))

(defmethod read-flag ((flag string) &rest others)
  (declare (ignore others))
  (or (car (member flag *flags* :test #'string=))
      (arg~signal-wrong-type 'flag flag)))

(defun do-set-flag (sym val)
  (setf (symbol-value sym) val))

(com~defcommand set-flag
  (argnames flag value)
  (argtypes flag anything)
  (arghelps "Flag" "Value")
  (function do-set-flag)
  (defaults set-flag-defaults)
  (categories resprover)
  (help "Set given flag with given value."))

(defun set-flag-defaults (flag val)
  (cond ((not (com~specified-arg-p flag))
	 (list (com~unspecified) val))
	((not (com~specified-arg-p val))
	 (list flag (symbol-value flag)))
	(t (list flag val))))


