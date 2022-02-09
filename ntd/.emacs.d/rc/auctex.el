;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;
;; AUCTeX  ;;
;;;;;;;;;;;;;
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq-default TeX-master nil)
;(setenv "TEXINPUTS" ":/home/ntd/src/ntd-latex:")

;; make "C-c C-c" save buffer first
(defun ntd/LaTeX-mode-hook ()
  (local-set-key "\C-c\C-c"
                 (lambda ()
                   (interactive)
                   (command-execute 'save-buffer)
                   (command-execute 'TeX-command-master)))
             ;(visual-line-mode nil)
  (local-set-key "\C-c\C-g"
                 (lambda ()
                   (interactive)
                   (pdf-sync-forward-search )))
  (local-set-key "\C-cg"
                 (lambda ()
                   (interactive)
                   (pdf-sync-forward-search)))
  (auto-fill-mode 1)
  (TeX-PDF-mode)
  (TeX-source-correlate-mode))

(add-hook 'LaTeX-mode-hook
          #'ntd/LaTeX-mode-hook)


(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(setq LaTeX-item-indent 0)

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

(defun ntd/TeX-after-compilation-finished-functions (file)
  (TeX-revert-document-buffer file))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'ntd/TeX-after-compilation-finished-functions)


;; Display the PDF part
(setq pdf-sync-forward-display-action
      '(display-buffer-reuse-window
        (inhibit-same-window . t)
        (reusable-frames . visible)))
