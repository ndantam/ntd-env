;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;
;; MAGIT ;;
;;;;;;;;;;;
(autoload 'magit-status "magit" "" t)

(eval-after-load 'magit
  '(progn
     ;;(require 'magit-key-mode)
     (require 'magit-svn)
     (add-hook 'magit-mode-hook 'turn-on-magit-svn)
     (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  ))
