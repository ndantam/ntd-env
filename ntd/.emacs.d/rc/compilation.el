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



;; 1: filename
;; 2: command
;; 3: reset commands after executing
(defvar ntd/compile-alist
  `(("Makefile" ,(format "make -kj%d "
                          (let ((n (string-to-number (shell-command-to-string "nproc"))))
                            (cond
                             ((<= n 4)
                              (* n 2))
                             ((<= n 16
                                  (/ (* 3 n )
                                     2)))
                             (t n))))
     nil)
    ("configure" "./configure " t)
    ("autogen.sh" "./autogen.sh" t)
    ("configure.ac" "autoreconf -i" t)
    (".git" "" nil)))

(defun ntd/compile-probe (directory fun otherwise)
  ;; fun: (command directory reset) -> t
  ;; otherwise: () -> t
  (cl-labels ((f (alist)
                 (if alist
                     (cl-destructuring-bind (filename cmd reset) (car alist)
                       (if (file-exists-p (expand-file-name filename directory))
                           (funcall fun cmd directory reset)
                           (f (cdr alist))))
                   (funcall otherwise))))
    (f ntd/compile-alist)))

(defun ntd/compile-search (directory fun)
  (let ((directory (expand-file-name directory)))
    (ntd/compile-probe directory
                       fun
                       (lambda ()
                         (if (equal directory "/")
                             ;; Empty command in default directory
                             (funcall fun "" default-directory nil)
                           ;; look up
                           (ntd/compile-search (concat directory "/..")
                                               fun))))))

(defvar ntd/compile-command nil)
(defvar ntd/compile-directory nil)
(defvar ntd/compile-reset nil)

(defun ntd/compile (arg)
  "Context-aware compilation of the program including the current buffer.

If not called before on the current buffer, attempt to guess the
compilation command based on files in the current project or
parent directory.

If already called on the current buffer, recompile using the
previous command.  Some commands though (e.g., `./configure`),
will reset the compilation command and then we try to guess the
command again.

If given a prefix argument (C-u), allow editing of the command.  If
given two prefix arguemnsts (C-u C-u), reset the command and
guess again."
  (interactive "p")
  (cl-flet* ((reset ()
                    (set (make-local-variable 'ntd/compile-command) nil)
                    (set (make-local-variable 'ntd/compile-reset) nil)
                    (set (make-local-variable 'ntd/compile-directory) nil))
             (fun (cmd directory reset)
                  ;; Initialize compile-command
                  (set (make-local-variable 'compile-command) cmd)
                  ;; Compile
                  (cond
                   ((and ntd/compile-command
                         (= arg 1))
                    (recompile))
                   ((when-let ((project (project-current)))
                      (equal directory (project-root project)))
                    (project-compile))
                   (t
                    (let ((default-directory directory))
                      (call-interactively 'compile))))
                  ;; Save
                  (if reset
                      (reset)
                    (set (make-local-variable 'ntd/compile-command) t)
                    (set (make-local-variable 'ntd/compile-directory) directory)
                    (set (make-local-variable 'ntd/compile-reset) nil)))
             (search ()
                     (ntd/compile-search default-directory #'fun)))
    ;; Save buffer
    (when buffer-file-name (save-buffer))
    ;; Maybe reset compile command
    (when (> arg 4) (reset))
    ;; Find command and compile
    (if ntd/compile-command
        (fun compile-command ntd/compile-directory nil)
      ;; Guess command
      (if-let ((project (project-current)))
          ;; Probe project or search directories
          (ntd/compile-probe (project-root project) #'fun #'search)
        ;; Search directories
        (search)))))
