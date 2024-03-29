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

(defun ntd/delete-trailing-whitespace ()
  (delete-trailing-whitespace))

(add-hook 'before-save-hook 'ntd/delete-trailing-whitespace)

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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(define-key global-map "\C-\M-Q" 'unfill-paragraph)
