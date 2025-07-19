;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;;;;;;;;
;;  Compilation  ;;
;;;;;;;;;;;;;;;;;;;

(defun ntd/get-closest-makefile-dir (d)
  (let ((d (expand-file-name d)))
    (cond
     ((file-exists-p (expand-file-name "Makefile" d))
      (let ((r (file-remote-p d)))
        (if r
          (subseq d (length r)) ; strip remote prefix
          d)))
     ((equal d "/")
      nil)
     (t
      (ntd/get-closest-makefile-dir (concat d "/.."))))))

(defvar ntd/base-compile-command
  (format "make -kj%d"
          (let ((n (string-to-number (shell-command-to-string "nproc"))))
            (cond
             ((<= n 4)
              (* n 2))
             ((<= n 16
                  (/ (* 3 n )
                     2)))
             (t n)))))

(defvar ntd/compile-command nil)
;;(setq ntd/compile-command nil)

(defun ntd/compile-command ()
  (let ((cmd
         (or
          ;; already defined, but let compile-command override
          (if ntd/compile-command
              (if compile-command
                  compile-command
                ntd/compile-command))
          ;; Project
          (let ((root (project-root (project-current))))
            (when (file-exists-p (expand-file-name "Makefile" root))
              ntd/base-compile-command))
          ;; TODO: look for other build files
          ;; Lookup
          (let ((dir (ntd/get-closest-makefile-dir default-directory)))
            (if dir
                (format "%s -C %s" ntd/base-compile-command dir)
              ntd/base-compile-command)))))
    cmd))

(defun ntd/compile (arg)
  (interactive "P")
  (when buffer-file-name (save-buffer))
  (let ((recompile (and ntd/compile-command (null arg)))
        (cmd (ntd/compile-command)))
    ;; Save
    (set (make-local-variable 'ntd/compile-command) cmd)
    (set (make-local-variable 'compile-command) cmd)
    ;; do it
    (cond
     (recompile (recompile))
     ((project-current)
      (project-compile))
     (t (compile cmd)))))
