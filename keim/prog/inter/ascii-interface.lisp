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

(mod~defmod asi :uses (mod sys inter arg) 
	    :documentation "Defines a simple ASCII interface."
	    :exports (
		      asi+inter
		      asi~create
		      asi~input
		      asi~output
		      asi~error
		      asi+no-input-error
		      asi~linereadp
		      ))


#{
\section{A simple ASCII interface}
 Here we define a simple interface using streams.  The {\vb asi+inter}
 interface class has three slots.  The first is used for input and
 must be an input stream.  The second two, output and error, must
 be output streams.  Output is for normal output and error is for
 error output.  This kind of interface is conceptually similar to
 the {\vb *standard-input*}, {\vb *standard-output*} and {\vb *error-output*} 
of \commonlisp.
#}

(eval-when (load compile eval) 
(defclass asi+inter (inter+face)
  ((input :initarg :input :accessor asi=input
          :documentation "An input stream.")
   (output :initarg :output :accessor asi=output
          :documentation "An output stream.")
   (error :initarg :error :accessor asi=error
          :documentation "An output stream for error messages."))
  (:documentation "An interface with a single input stream, a output stream 
for normal output and a output stream for error output.")))


(defun asi~create (&key (name (gensym "ASI-INTERFACE"))
			(input *standard-input*) (output *standard-output*)
			(error *error-output*)
			(help ""))
  (declare 
   (authors nesmith)
   (input "Four keyword parameters. NAME names the interface.  INPUT
is the interface's input stream (defaults to *standard-input*); OUTPUT
is the interface's output stream and defaults to *standard-output*,
while ERROR is the interface's ERROR stream and defaults to *error-output*")
   (value "A new interface initialized with given slots."))
  (assert (symbolp name))
  (assert (input-stream-p input))
  (assert (output-stream-p output))
  (assert (output-stream-p error))
  (assert (stringp help))
  (make-instance 'asi+inter :name name :input input 
		 :output output :error error :help help))

(defgeneric asi~input (asi)
  (declare 
   (authors nesmith)
   (input "An interface of type {\\vb asi+inter}.")
   (value "The interface's input stream."))
  (:method ((asi asi+inter))
   (asi=input asi)))

(defgeneric asi~output (asi)
  (declare 
   (authors nesmith)
   (input "An interface of type {\\vb asi+output}.")
   (value "The interface's output stream."))
  (:method ((asi asi+inter))
   (asi=output asi)))

(defgeneric asi~error (asi)
  (declare 
   (authors nesmith)
   (input "An interface of type ASI+ERROR.")
   (value "The interface's error stream."))
  (:method ((asi asi+inter))
   (asi=error asi)))

#{ {\vb inter~terpri} merely performs a {\vb terpri} on the output stream.#}
(defmethod inter~terpri ((interface asi+inter))
  (terpri (asi=output interface)))

#{{\vb inter~fresh-line} performs a {\vb fresh-line} on the output stream.
#}
(defmethod inter~fresh-line ((interface asi+inter))
  (fresh-line (asi=output interface)))

#{
{\vb inter~print-error} prints the error object to the error stream,
 with a short preamble.
#}
(defmethod inter~print-error ((interface asi+inter) error)
  (format (asi=error interface) "~%;;;ERROR: ~A~&" error))

#{ {\vb inter~print-warning} prints the warning object to the error stream,
 with a short preamble.
#}
(defmethod inter~print-warning ((interface asi+inter) warning)
  (format (asi=error interface) "~%;;;WARNING: ~A~&" warning))

#{ {\vb inter~output-object} formats the object to the output stream, using
 the ~A directive.
#}
(defmethod inter~output-object ((interface asi+inter) object)
  (format (asi=output interface) "~A" object))

#{ {\vb inter~input-object} reads an object from the input stream.  If eof
 is seen, an {\vb inter+error} condition is signaled.  Otherwise the 
 read-function of the given argtype is called on the object that is read.
#}
(defmethod inter~input-object ((interface asi+inter) (type arg+type))
  (let* ((read-function (arg~read-function type))
	 (input (asi=input interface))
	 (eof-sym (gensym))
	 (obj (read input nil eof-sym)))
    (if (eq obj eof-sym)
	(sys~signal
	 (sys~make-condition 'inter+error
			     :format-string "Hit eof on input"
			     :args nil))
	 (funcall read-function obj))))

(sys~define-condition asi+no-input-error (sys+error)
  ()
  (lambda (cond stream) (declare (ignore cond))
    (format stream "Received no input."))
  (:documentation "Indicates that the user simply entered a return and no input."))


(defmethod inter~prompt-for-input-with-default ((interface asi+inter) prompt (argtype arg+type) default)
  (sys~handler-case
   (progn
     (inter~output-object interface prompt)
     (inter~output-object interface "[")
     (inter~output-object interface default)
     (inter~output-object interface "]")
     (let* ((read-function (arg~read-function argtype))
	    (input (asi=input interface))
	    (obj (car (asi~linereadp input))))
       (funcall read-function obj)))
   (asi+no-input-error () (return-from inter~prompt-for-input-with-default default))
   (inter+error (condition)
		(inter~print-error interface condition))
   (inter+warning (condition)
		  (inter~print-warning interface condition))))
  
#{Here we define a function to get forms from the input stream.
{\vb asi~linereadp} is based on the token-reading rules given in CLTL.
#}
(defun asi~linereadp (&optional (input-stream *standard-input*))
  (declare
   (authors nesmith)
   (input "an optional stream (defaults to *standard-input*)")
   (value "a list of input objects contained on the line(s)")
   (effect "an input line is read from the stream. If an object is not finished,
another line is read (ad infinitum)"))
  (labels ((lisp-ize (tokens)
	     (mapcar #'(lambda (x) (with-input-from-string (in x) (read in)))
		     tokens))
	   (tokenize-line (str)
	     (do ((next-start 0)
		  (last-start 0)
		  (len (length str))
		  (new-token t)
		  (tokens nil (if (and new-token (not (zerop (length new-token))))
				  (cons new-token tokens) 
				tokens)))
		 ((or (null new-token) 
		      (= len next-start))
		  (values (nreverse tokens) 
			  (if (null new-token) (subseq str last-start))
			  (if new-token t nil)))
	       (declare (special len))
	       (setq last-start next-start)
	       (multiple-value-setq (new-token next-start)
		 (get-next-token str next-start))))
	   (get-next-token (str begin)
	     ;;"Input string STR and index BEGIN. 
	     ;;Output: If we can get a full token from STR beginning at BEGIN, returns the 
	     ;;token (a string) and the index of STR at which the token ends.  Otherwise
	     ;;returns NIL and BEGIN."
	     (declare (special len))
	     ;; assume str is not empty string and first elt is not whitespace
	     (let ((char (char str begin)))
	       (cond ((eql char #\")	
		      (read-a-string-token str begin))
		     ((or (eql char #\')	
			  (eql char #\`)
			  (eql char #\,))
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token (values (concatenate 'string (string char)
						       token)
					  end-index))))
		     ((eql char #\()	
		      (read-a-list-token str begin))
		     ((eql char #\))	
		      (values "" (next-non-whitespace-char str (1+ begin))))
		     ((eql char #\#)
		      (read-a-dispatch-macro-token str begin))
		     ((eql char #\\)	
		      (read-a-single-escape-token str begin))
		     ((eql char #\|)	
		      (accumulate-token-even-multiple str begin ""))
		     ((eql char #\;) 
		      (values "" (length str)))
		     (t
		      ;; hope it's a constituent character! 
		      (accumulate-token-even-multiple str begin "")))))
	   (read-a-dispatch-macro-token (str begin)
	     "know that begin'th char of str is a #"
	     (declare (special len))
	     (let ((ch (if (> len (1+ begin)) (char str (1+ begin)))))
	       (cond ((null ch) nil)
		     ((whitespace-char-p ch)
		      (values (subseq str begin (+ 2 begin)) 
			      (next-non-whitespace-char str (+ 2 begin))))
		     ((eql ch #\\)
		      ;; this should be a char object
		      (multiple-value-bind (token end-index)
			  (read-a-single-escape-token str (1+ begin))
			(if token (values (concatenate 'string "#"
						       token)
					  end-index))))
		     ((digit-char-p ch)
		      (let* ((end-of-num
			      (position-if-not  #'digit-char-p
						str :start (+ 1 begin)))
			     (next-ch (if (> len end-of-num) (char str end-of-num))))
			(cond ((or (null next-ch)
				   (whitespace-char-p next-ch))
			       ;; just send back the # and the number
			       (values (subseq str begin end-of-num)
				       (next-non-whitespace-char str
								 end-of-num)))
			      ((member next-ch '(#\( #\*))
			       ;; vectors
			       (multiple-value-bind (token end-index)
				   (get-next-token str end-of-num)
				 (if token (values (concatenate 'string 
						     (subseq str begin end-of-num)
						     token)
						   end-index))))
			      ((eql next-ch #\#)
			       (values (subseq str begin (1+ end-of-num))
				       (next-non-whitespace-char str
								 (1+ end-of-num))))
			      (t
			       ;; includes rational, array or ref
			       (multiple-value-bind (token end-index)
				   (get-next-token str (next-non-whitespace-char
							str
							(1+ end-of-num)))
				 (if token
				     (values (concatenate 'string
					       (subseq str begin (1+ end-of-num))
					       token)
					     end-index)))))))
		     ((eql  ch #\()
		      ;; vectors without numeric arg
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token (values (concatenate 'string "#"
						       token)
					  end-index))))
		     ((eql ch #\|)
		      (let ((next-start
			     (read-macro-comment str (+ 2 begin))))
			(if next-start (values "" next-start))))
		     ((member ch '(#\* #\' #\, #\: #\. #\B #\b #\O #\o #\X #\x
				   #\S #\s #\+ #\- ))
		      ;; things which have a char then a form
		      ;; e.g., #*, #.,  #'
		      (let ((next-spot
			     (next-non-whitespace-char str (+ 2 begin)))
			    (token nil))
			(when (< next-spot len)
			  (multiple-value-setq (token next-spot)
			    (get-next-token str next-spot))
			  (when token (values (concatenate 'string 
						(subseq str begin (+ 2 begin))
						token)
					      next-spot)))))
		     (t
		      (multiple-value-bind (token end-index)
			  (get-next-token str (1+ begin))
			(if token 
			    (values (concatenate 'string "#" token)
				    end-index)
			  (values (subseq str begin (+ 2 begin))
				  (next-non-whitespace-char str (+ 2 begin)))))))))
	   (read-a-string-token (str begin)
	     (multiple-value-bind (token next-start)
		 (read-until-token str (1+ begin) #\")
	       (when token
		 (values (concatenate 'string "\"" token) next-start))))
	   (read-a-list-token (str begin)
	     "Read a list in as a single token."
	     (declare (special len))
	     (do ((token t)
		  (index (1+ begin)))
		 ((or (null token)
		      (>= index len)
		      (eql (char str index) #\)))
		  (if (or (null token)
			  (>= index len))
		      nil
		    (values (subseq str begin (1+ index))
			    (next-non-whitespace-char str (1+ index)))))
	       (multiple-value-setq (token index)
		 (get-next-token str index))))
	   (read-until-token (str begin closech)
	     "Return the substring of STR from BEGIN to the next unescaped 
              occurrence of CLOSECH.  Also return position of next non-whitespace char after
              that.  If no CLOSECH occurs, return NIL."
	     (declare (special len))
	     (do  ((index begin))
		 ((or (>= index len)
		      (eql (char str index) closech))
		  (if (= index len) 
		      nil
		    (values (subseq str begin (1+ index))
			    (next-non-whitespace-char str (1+ index)))))
	       (if (eql (char str index) #\\)
		   (incf index 2)
		 (incf index 1))))
	   (read-macro-comment (str begin) 
	     "This is to read comments which are delimited by \#\| and \|\#.
              Just skips over them, returning no tokens."
	     (declare (special len))
	     (do  ((index begin)
		   (num-found 1))
		 ((or (>= (1+ index) len)
		      (zerop num-found))
		  (if (zerop num-found)
		      (next-non-whitespace-char str index)
		    nil))
	       (cond ((eql (char str index) #\\)
		      (incf index 2))
		     ((and (eql (char str index) #\|)
			   (eql (char str (1+ index)) #\#))
		      (decf num-found)
		      (incf index 2))
		     ((and (eql (char str index) #\#)
			   (eql (char str (1+ index)) #\|))
		      (incf num-found)
		      (incf index 2))
		     (t (incf index 1)))))
	   (whitespace-char-p (ch)
	     (member ch '(#\tab #\space #\page #\return #\newline #\linefeed)))
	   (next-non-whitespace-char (str index)
	     (declare (special len))
	     (do ((index index (1+ index)))
		 ((or (= index len)
		      (not (whitespace-char-p (char str index))))
		  index)))
	   (read-a-single-escape-token (str begin)
	     "Rule 5 on page 335 of CLtL."
	     (when (not (= begin (1- (length str))))
	       (accumulate-token-even-multiple str (+ begin 2)
					       (subseq str begin (+ begin 2)))))
	   (accumulate-token-even-multiple (str begin token-so-far)
	     "Rule 8 on page 337 of CLtL."
	     (declare (special len))
	     (let ((ch (if (< begin len) (char str begin))))
	       (cond ((not ch)
		      (values token-so-far begin))
		     ((eql ch #\\)
		      (if (= len (1+ begin))
			  nil
			(accumulate-token-even-multiple 
			 str (+ begin 2)
			 (concatenate 'string 
			   token-so-far
			   (subseq str begin (+ begin 2))))))
		     ((eql ch #\|)
		      (accumulate-token-odd-multiple str (1+ begin)
						     (concatenate 'string
						       token-so-far (string
								     ch))))
		     ((member ch '(#\' #\( #\) #\" #\; #\`)
			      :test #'eql)
		      (values token-so-far begin))
		     ((whitespace-char-p ch)
		      (values token-so-far (next-non-whitespace-char str begin)))
		     (t
		      ;; should be constituent, or non-terminating macro character
		      (accumulate-token-even-multiple str (1+ begin)
						      (concatenate 'string
							token-so-far (string
								      ch)))))))
	   (accumulate-token-odd-multiple (str begin token-so-far)
	     "Rule 9 on page 337 of CLtL."
	     (declare (special len))
	     (let ((ch (if (< begin len) (char str begin))))
	       (cond ((not ch) nil)
		     ((eql ch #\|)
		      (accumulate-token-even-multiple str (1+ begin)
						      (concatenate 'string
							token-so-far (string
								      ch))))
		     ((eql ch #\\)
		      (if (= len (1+ begin))
			  nil
			(accumulate-token-odd-multiple 
			 str (+ begin 2)
			 (concatenate 'string 
			   token-so-far
			   (subseq str begin (+ begin 2))))))
		     (t 
		      (accumulate-token-odd-multiple str (1+ begin)
						     (concatenate 'string
						       token-so-far (string
								     ch)))))))

	   )
    (let ((eof-value (cons nil nil)))
      (do ((instring (read-line input-stream nil eof-value)
		     (unless finished-p
		       (concatenate 'string unfinished-part
				    (string #\newline)
				    (read-line input-stream nil 
					       eof-value))))
	   (new-tokens nil)
	   (tokens nil (nconc tokens new-tokens))
	   (unfinished-part "")
	   (finished-p nil))
	  (finished-p  
	   (if tokens 
	       (lisp-ize tokens)
	     (sys~signal 'asi+no-input-error)))
	(multiple-value-setq (new-tokens unfinished-part finished-p)
	  (tokenize-line instring))))))
