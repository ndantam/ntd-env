;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;; CL  ;;
;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asdf-install$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))


(global-set-key "\C-cll" 'slime-load-system)
(global-set-key "\C-clr" 'slime-reload-system)
