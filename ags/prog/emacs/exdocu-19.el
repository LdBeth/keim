;;; -*- Mode: Lisp -*-
;;; Written 1993 in Emacs-Lisp

;;; Keybindings
(defvar sun-esc-bracket t  "*If non-nil, rebind ESC [ as prefix for Sun function keys.")
(defvar sun-raw-map (make-sparse-keymap) "*Keymap for ESC-[ encoded keyboard")
(if sun-esc-bracket (progn (define-key esc-map "[" sun-raw-map)))

(global-set-key [f14] 'undo)		                     ;   Undo L4
(global-set-key [f22] 'print-buffer)               	     ;   PrSc R2
(global-set-key [f27] 'beginning-of-buffer)	             ; 7 Home R7
(global-set-key [f29] 'scroll-down)                          ; 9 PgUp R9
(global-set-key [f33] 'end-of-buffer)	                     ; 1 End  R13
(global-set-key [f35] 'scroll-up)                            ; 3 PgDn R15
;;; Edit-Functions
(global-set-key [f8 ?d] 'emacs-declare)                       ;  Hyper-d
(global-set-key [f8 ?s] 'update-authors)                      ;  Hyper-s
(global-set-key [f8 ?S] 'emacs-update-current-declare)        ;  Hyper-Shift-s
(global-set-key [f7 ?d] 'emacs-comment)                       ;  Super-d
(global-set-key [f7 ?s] 'update-authors-comment)              ;  Super-s
(global-set-key [f7 ?S] 'emacs-update-current-comment)        ;  Super-Shift-s
;;; eval&compile Functions
(global-set-key [f8 ?e] 'emacs-evaluate)                      ;  Hyper-e
(global-set-key [f7 ?e] 'emacs-evaluate-comment)              ;  Super-e
(global-set-key [f8 ?c] 'emacs-compile)                       ;  Hyper-c
(global-set-key [f7 ?c] 'emacs-compile-comment)               ;  Super-c
;;; my special keys
;(global-set-key [f9] 'mark-defun)                            ;  F9
;(global-set-key [f10]  'eval-defun)                          ;  F10
;(global-set-key [f1] 'mark-whole-buffer)                     ;  F1
;(global-set-key [f2] 'indent-region)                         ;  F2
;(global-set-key [f3] 'eval-region)                           ;  F3





