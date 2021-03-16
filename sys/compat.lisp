(in-package #:cl-user)

(defpackage #:ags
  (:use #:cl))

(in-package #:ags)

(declaim (declaration edited authors input effect value remark example))

(defmacro ags::doc~latex (&rest args)
  (declare (ignore args))
  (values))

(unless (fboundp 'read-latex-string)
  (eval-when (load compile eval)

	     (defvar read-buffer)
	     (defvar read-buffer-length)
	     (defvar ouch-ptr)
	     (defvar inch-ptr)

	     (defmacro reset-read-buffer ()
	       ;;turn read-buffer into an empty read-buffer.
	       ;;ouch-ptr always points to next char to write
	       `(progn
		  (unless (and (boundp 'read-buffer) read-buffer) (init-read-buffer))
		  (setq ouch-ptr 0)
		  ;;inch-ptr always points to next char to read
		  (setq inch-ptr 0)))
	     )

  (defun init-read-buffer ()
    (setq read-buffer (make-string 512)) ;initial bufsize
    (setq read-buffer-length 512)
    (reset-read-buffer))

  (eval-when (load compile eval)
	     (defmacro ouch-read-buffer (char)
	       `(progn
		  (if (>= (the fixnum ouch-ptr)
			  (the fixnum read-buffer-length))
		      ;;buffer overflow -- double the size
		      (grow-read-buffer))
		  (setf (elt (the simple-string read-buffer) ouch-ptr) ,char)
		  (setq ouch-ptr (1+ ouch-ptr))))

	     ;; macro to move ouch-ptr back one.

	     (defmacro ouch-unread-buffer ()
	       '(if (> (the fixnum ouch-ptr) (the fixnum inch-ptr))
		    (setq ouch-ptr (1- (the fixnum ouch-ptr)))))
	     )

  (defun grow-read-buffer ()
    (let ((rbl (length (the simple-string read-buffer))))
      (declare (fixnum rbl))
      (setq read-buffer
	    (concatenate 'simple-string
			 (the simple-string read-buffer)
			 (the simple-string (make-string rbl))))
      (setq read-buffer-length (* 2 rbl))))


  (eval-when (load compile eval)
	     (defmacro unread-buffer ()
	       `(decf (the fixnum inch-ptr)))
	     )

  (defun read-buffer-to-string ()
    (subseq (the simple-string read-buffer) 0 ouch-ptr))


  (defun read-latex-string (stream)
    (reset-read-buffer)
    (do ((char (read-char stream t nil t)
	       (read-char stream t nil t))
       	 (last-char-is-sharp nil
			     (char= char #\#)))
	((and last-char-is-sharp
	      (char= char #\}))
	 (ouch-unread-buffer)(read-buffer-to-string))
      (ouch-read-buffer char)))

  (defvar *ags-readtable* nil "A readtable defining #{ #} syntax.")

  (setq *ags-readtable* (copy-readtable nil))

  ;; here's the important stuff

  (defun |#{-reader| (stream subchar arg)
    (declare (ignore subchar arg))
    (list 'ags::doc~latex (read-latex-string stream)))

  (set-dispatch-macro-character #\# #\{ #'|#{-reader| *ags-readtable*)

  (setq *readtable* *ags-readtable*)

  (dolist (fun '(read-latex-string |#{-reader| init-read-buffer
				   read-buffer-to-string grow-read-buffer))
    (compile fun))
  )

(setq *readtable* *ags-readtable*)
