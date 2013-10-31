;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;
;; AUCTeX  ;;
;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setenv "TEXINPUTS" ":/home/ntd/src/ntd-latex:")

;; make "C-c C-c" save buffer first
(add-hook 'LaTeX-mode-hook
          '(lambda()
             (local-set-key "\C-c\C-c"
                            (lambda ()
                              (interactive)
                              (command-execute 'save-buffer)
                              (command-execute 'TeX-command-master)))))
