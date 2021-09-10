;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;;;;;;;;
;;  Compilation  ;;
;;;;;;;;;;;;;;;;;;;

(setq my-compile-command
      (concat "make -kj"
              (substring (shell-command-to-string "nproc")
                         0 -1)))

(defun get-closest-makefile-dir (d)
  (let ((d (expand-file-name d)))
    (cond
     ((file-exists-p (expand-file-name "Makefile" d))
      d)
     ((equal d "/")
      nil)
     (t
      (get-closest-makefile-dir (concat d "/.."))))))

(defun closest-makefile-hook ()
  (set (make-local-variable 'compile-command)
       (format "%s -C %s"
               my-compile-command
               (get-closest-makefile-dir default-directory))))

(add-hook 'c-mode-hook 'closest-makefile-hook)
(add-hook 'c++-mode-hook 'closest-makefile-hook)
(add-hook 'f90-mode-hook 'closest-makefile-hook)
(add-hook 'f77-mode-hook 'closest-makefile-hook)
(add-hook 'org-mode-hook 'closest-makefile-hook)
(add-hook 'markdown-mode-hook 'closest-makefile-hook)
