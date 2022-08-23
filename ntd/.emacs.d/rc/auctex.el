;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;
;; AUCTeX  ;;
;;;;;;;;;;;;;
(setq TeX-auto-save t
      TeX-save-query nil
      TeX-parse-self t)

(setq-default TeX-master nil)

;(setenv "TEXINPUTS" ":/home/ntd/src/ntd-latex:")
;; (require 'auctex)


;; (setq LaTeX-item-indent 0)



;; LaTeXMk
;; Currently Broken
;; (require 'auctex-latexmk)
;; (auctex-latexmk-setup)
;; (setq auctex-latexmk-inherit-TeX-PDF-mode t)


;; PDF Tools
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)


;; Display the PDF part
(setq pdf-sync-forward-display-action
      '(display-buffer-reuse-window
        (inhibit-same-window . t)
        (reusable-frames . visible)))


;; Hooks
(defun ntd/LaTeX-mode-hook ()
  (visual-line-mode nil)
  (reftex-mode)
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (TeX-PDF-mode)
  (TeX-source-correlate-mode)
  )

(add-hook 'LaTeX-mode-hook #'ntd/LaTeX-mode-hook)

(defun ntd/TeX-after-compilation-finished-functions (file)
  (TeX-revert-document-buffer file))


;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'ntd/TeX-after-compilation-finished-functions)
