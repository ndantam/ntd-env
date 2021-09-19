;; -*- mode: emacs-lisp -*-;;
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
            (if (<= n 4)
                (* n 2)
              (/ (* 3 n )
                 2)))))

(defvar ntd/compile-command nil)
(defun ntd/compile-command ()
  (or ntd/compile-command
      (let ((dir (ntd/get-closest-makefile-dir default-directory)))
        (if dir
            (format "%s -C %s" ntd/base-compile-command dir)
          ntd/base-compile-command))))

(defun ntd/closest-makefile-hook ()
  (let ((cmd (ntd/compile-command)))
    (set (make-local-variable 'compile-command)
         cmd)
    (set (make-local-variable 'ntd/compile-command)
         cmd)))

(defun ntd/compile (cmd)
  (interactive (list (read-from-minibuffer "Compile Command: "
                                           (ntd/compile-command))))
  (when buffer-file-name
    (save-buffer)
    (set (make-local-variable 'ntd/compile-command) cmd))
  (compile cmd))

(defun ntd/recompile ()
  (interactive)
  (when buffer-file-name
    (save-buffer))
  (command-execute 'recompile))

(add-hook 'c-mode-hook 'ntd/closest-makefile-hook)
(add-hook 'c++-mode-hook 'ntd/closest-makefile-hook)
(add-hook 'f90-mode-hook 'ntd/closest-makefile-hook)
(add-hook 'f77-mode-hook 'ntd/closest-makefile-hook)
(add-hook 'org-mode-hook 'ntd/closest-makefile-hook)
(add-hook 'markdown-mode-hook 'ntd/closest-makefile-hook)
