;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;
;; FORTRAN ;;
;;;;;;;;;;;;;
(defun ntd/f90-return ()
  (interactive)
  (f90-indent-line)
  (newline-and-indent))

(add-hook 'f90-mode-hook
          '(lambda()
             (local-set-key [13] 'ntd/f90-return)    ; RET with automatic indent
             (imenu-add-to-menubar "Program-Units") ; Add index of func. to menu bar
             ))
