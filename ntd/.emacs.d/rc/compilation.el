;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;;;;;;;;
;;  Compilation  ;;
;;;;;;;;;;;;;;;;;;;

(setq my-compile-command "make -kj2")

(defun get-closest-makefile-dir ()
  (let ((root (expand-file-name "/")))
    (loop
     for d = ".." then (concat "../" d)
     if (file-exists-p (expand-file-name "Makefile" d))
     return d
     if (equal (expand-file-name d) root)
     return nil)))


(defun closest-makefile-hook ()
  (set (make-local-variable 'compile-command)
       (if (file-exists-p (expand-file-name "Makefile" default-directory))
           my-compile-command
         (format "cd %s && %s" (get-closest-makefile-dir)
                 my-compile-command))))

(add-hook 'c-mode-hook 'closest-makefile-hook)
(add-hook 'c++-mode-hook 'closest-makefile-hook)
(add-hook 'f90-mode-hook 'closest-makefile-hook)
(add-hook 'f77-mode-hook 'closest-makefile-hook)
(add-hook 'org-mode-hook 'closest-makefile-hook)
