;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;;;;
;; WHITESPACE ;;
;;;;;;;;;;;;;;;;
(require 'whitespace)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ntd-whitespace-cleanup-modes
      '(org-mode
        latex-mode
        html-mode xml-mode
        c-mode c++-mode
        conf-space-mode
        sh-mode
        markdown-mode
        lisp-mode emacs-lisp-mode))

(defun ntd-whitespace-cleanup ()
  (interactive)
  (when (find major-mode ntd-whitespace-cleanup-modes)
    (if whitespace-mode
        (whitespace-cleanup)
      (progn
        (whitespace-mode 1)
        (whitespace-cleanup)
        (whitespace-mode 0)))))

(add-hook 'before-save-hook
          'ntd-whitespace-cleanup)
