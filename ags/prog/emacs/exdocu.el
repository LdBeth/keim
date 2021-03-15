;;; -*- Mode: Lisp -*-
;;; Written 1992 in Emacs-Lisp

(defun time ()
  (substring(current-time-string) 11 16))

(defun day ()
  (if (string= (substring(current-time-string) 8 9) " ")
      (concat "0" (substring(current-time-string) 9 10))
    (substring(current-time-string) 8 10)))

(defun month ()
  (upcase(substring(current-time-string) 4 7)))

(defun year ()
  (substring(current-time-string) 20 24))

(defun emacs-time ()
  (concat (day) "-" (month) "-" (year) " " (time)))

(defun emacs-authors ()
  (upcase (getenv "USER")))

;;; 'declare'-comment
(defun emacs-declare  ()
  (interactive)
  (insert "\n  (declare (edited  \""(emacs-time)"\")\n\t   (authors "(emacs-authors)")\n\t   (input   )\n\t   (effect  )\n\t   (value   ))\n")
  (message "Declare inserted"))

(defun update-authors ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "declare (edited" nil t)
      (when (search-forward "(authors" nil t)
	(insert " " (emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(message "Authors updated"))
    (message "Error: Declare is missing or incorrect!!")))

(defun emacs-update-current-declare ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "(declare (edited  " nil t)
      (progn (kill-line)
	     (insert "\"" (emacs-time) "\")")
	     (message "Time has been updated"))      
    (message "Error: Declare is missing or incorrect!!")))

;;; semicolon-prefixed comment
(defun emacs-comment  ()
  (interactive)
  (insert "\n  ; Edited:  \""(emacs-time) "\"\n  ; Authors: "(emacs-authors)" \n  ; Input:   \n  ; Effect:  \n  ; Value:   \n")
  (message "Comment inserted"))

(defun update-authors-comment ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "; Edited:" nil t)
      (when (search-forward "; Authors:" nil t)
	(insert " "(emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(message "Authors updated"))
    (message "Error: Comment is missing or incorrect!!")))

(defun emacs-update-current-comment ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "; Edited:  " nil t)
      (progn (kill-line)
	     (insert "\"" (emacs-time) "\"")
	     (message "Time has been updated"))      
    (message "Error: Comment is missing or incorrect!!")))



;;; Extract Documentation
(defun extract-documentation (filename)
  (interactive "FExtract file: ")
  (setq source-filename filename)
  (setq source-filename (directory-file-name source-filename))
  (setq destination-filename (read-from-minibuffer "Destination file: " (concat source-filename ".doc")))
  (if (buffer-modified-p)
      (if (yes-or-no-p (format "Save Buffer first? "))
	  (save-buffer)))
  (process-send-string "*keim*" (concat "(doc-extract #P\"" source-filename "\" :interface #P\"" destination-filename "\")"))
  (sleep-for 3)
  (message "Extraction completed"))

(defun extract-tex-documentation (filename)
  (interactive "FExtract file: ")
  (setq source-filename filename)
  (setq source-filename (directory-file-name source-filename))
  (setq destination-filename (read-from-minibuffer "Destination file: " (concat source-filename ".tex")))
  (if (buffer-modified-p)
      (if (yes-or-no-p (format "Save Buffer first? "))
	  (save-buffer)))
  (process-send-string "*keim*" (concat "(doc-extract #P\"" source-filename "\" :interface-tex #P\"" destination-filename "\")"))
  (sleep-for 3)
  (message "Extraction completed"))

(defun extract-manual-documentation (filename)
  (interactive "FExtract file: ")
  (setq source-filename filename)
  (setq source-filename (directory-file-name source-filename)) 
  (setq destination-filename (read-from-minibuffer "Destination file: " (concat source-filename ".tex")))
  (if (buffer-modified-p)
      (if (yes-or-no-p (format "Save Buffer first? "))
	  (save-buffer))) 
  (process-send-string "*keim*" (concat "(doc-extract #P\"" source-filename "\" :simple-tex #P\"" destination-filename "\")"))
  (sleep-for 3)
  (message "Extraction completed"))

(defun extract-complete-documentation (filename)
  (interactive "FExtract file: ")
  (setq source-filename filename)
  (setq source-filename (directory-file-name source-filename))
  (setq destination-filename (read-from-minibuffer "Destination file: " (concat source-filename ".doc")))
  (if (buffer-modified-p)
      (if (yes-or-no-p (format "Save Buffer first? "))
	  (save-buffer)))
  (process-send-string "*keim*" (concat "(doc-extract #P\"" source-filename "\" :complete #P\"" destination-filename "\")"))
  (sleep-for 3)
  (message "Extraction completed"))

(defun extract-complete-tex-documentation (filename)
  (interactive "FExtract file: ")
  (setq source-filename filename)
  (setq source-filename (directory-file-name source-filename))
  (setq destination-filename (read-from-minibuffer "Destination file: " (concat source-filename ".tex")))
  (if (buffer-modified-p)
      (if (yes-or-no-p (format "Save Buffer first? "))
	  (save-buffer)))
  (process-send-string "*keim*" (concat "(doc-extract #P\"" source-filename "\" :complete-tex #P\"" destination-filename "\")"))
  (sleep-for 3)
  (message "Extraction completed"))

;;; eval&update
(defun emacs-evaluate ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "\n  (declare (edited" nil t)
      (when (search-forward "(authors" nil t)
	(insert " " (emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(if (eval-defun-lisp) ()
	  (sleep-for 3)
	  (message "Authors updated and function evaluated")))
    (message "Error: Declare is missing or incorrect!")))

(defun emacs-evaluate-comment ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "; Edited:" nil t)
      (when (search-forward "; Authors:" nil t)
	(insert " "(emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(if (eval-defun-lisp) ()
	  (sleep-for 3)
	  (message "Authors updated and function evaluated")))
    (message "Error: Comment is missing or incorrect!!")))

;;; compile&update
(defun emacs-compile ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "  (declare (edited" nil t)
      (when (search-forward "(authors" nil t)
	(insert " " (emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(if (compile-defun-lisp) ()
	  (sleep-for 3)
	  (message "Authors updated and function compiled")))
    (message "Error: Declare is missing or incorrect!")))

(defun emacs-compile-comment ()
  (interactive)
  (beginning-of-defun)
  (if (search-forward "\  ; Edited:" nil t)
      (when (search-forward "\  ; Authors:" nil t)
	(insert " "(emacs-authors))
	(when (search-forward (emacs-authors) nil t)
	  (backward-kill-word 1)
	  (backward-delete-char 1))
	(if (compile-defun-lisp) ()
	  (sleep-for 3)
	  (message "Authors updated and function compiled")))
    (message "Error: Comment is missing or incorrect!!")))

;;; Keybindings
(defvar sun-esc-bracket t  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")
(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")
(if sun-esc-bracket (progn (define-key esc-map "[" sun-raw-map)))
(define-key sun-raw-map "195z" 'undo)		                      ;   Undo L4
(define-key sun-raw-map "209z" 'print-buffer)               	      ;   PrSc R2
(define-key sun-raw-map "214z" 'beginning-of-buffer)	              ; 7 Home R7
(define-key sun-raw-map "216z" 'scroll-down)                          ; 9 PgUp R9
(define-key sun-raw-map "220z" 'end-of-buffer)	                      ; 1 End  R13
(define-key sun-raw-map "222z" 'scroll-up)                            ; 3 PgDn R15
;;; Edit-Functions
(define-key sun-raw-map "231zd" 'emacs-declare)                       ;  Hyper-d
(define-key sun-raw-map "231zs" 'update-authors)                      ;  Hyper-s
(define-key sun-raw-map "231zS" 'emacs-update-current-declare)        ;  Hyper-Shift-s
(define-key sun-raw-map "230zd" 'emacs-comment)                       ;  Super-d
(define-key sun-raw-map "230zs" 'update-authors-comment)              ;  Super-s
(define-key sun-raw-map "230zS" 'emacs-update-current-comment)        ;  Super-Shift-s
;;; eval&compile Functions
(define-key sun-raw-map "231ze" 'emacs-evaluate)                      ;  Hyper-e
(define-key sun-raw-map "230ze" 'emacs-evaluate-comment)              ;  Super-e
(define-key sun-raw-map "231zc" 'emacs-compile)                       ;  Hyper-c
(define-key sun-raw-map "230zc" 'emacs-compile-comment)               ;  Super-c
;;; my special keys
(define-key sun-raw-map "232z" 'mark-defun)                           ;  F9
(define-key sun-raw-map "-1z"  'eval-defun)                           ;  F10
(define-key sun-raw-map "224z" 'mark-whole-buffer)                    ;  F1
(define-key sun-raw-map "225z" 'indent-region)                        ;  F2
(define-key sun-raw-map "226z" 'eval-region)                          ;  F3





