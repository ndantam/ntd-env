;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;;  C  ;;
;;;;;;;;;

(c-add-style "ros-cc-style"
             '("k&r"
               (c-basic-offset . 2)
               (c-offsets-alist . ((innamespace . [0])
                                   (member-init-intro . [0])))))

(c-add-style "user"
             '("linux"
               (c-basic-offset . 4)
               (c-offsets-alist . ((innamespace . [0])
                                   (inextern-lang . [0])))))

(setq c-default-style "user")

;;;;;;;;;;;;;;
;;  clangd  ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.clang\\(d\\|-tidy\\|-format\\)\\'" . yaml-mode))

(with-eval-after-load 'eglot
  (defun ntd/maybe-start-eglot ()
    (let ((root (project-root (project-current))))
      (when (or (file-exists-p (expand-file-name ".clangd" root))
                (file-exists-p (expand-file-name "compile_commands.json" root)))
      (eglot-ensure))))

  (add-hook 'c-mode-common-hook #'ntd/maybe-start-eglot))

;;;;;;;;;;;;;;;;;;
;;  Formatting  ;;
;;;;;;;;;;;;;;;;;;

;; Notes: clang-format takes ~.05s vs ~.025s for eglot-format.

(defun ntd/c-format (start end)
  (cond
   ;; Try Eglot
   ((bound-and-true-p eglot--managed-mode)
    (eglot-format start end))
   ;; Try clang-format
   ((fboundp 'clang-format-region)
    (clang-format-region start end))
   ;; Basic
   (t
    (c-indent-region start end))))

(defun ntd/c-format-region ()
  (interactive)
  (ntd/c-format (region-beginning) (region-end)))

(defun ntd/c-format-buffer ()
  (interactive)
  (cond
   ;; Try Eglot
   ((bound-and-true-p eglot--managed-mode)
    (eglot-format-buffer))
   ;; Try clang-format
   ((fboundp 'clang-format-buffer)
    (clang-format-buffer))
   ;; Basic
   (t
    (c-indent-region (point-min) (point-max)))))

(defun ntd/c-tab-or-format (fun)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (if (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties begin end))
        (c-indent-line-or-region)
      (funcall fun begin end))))

(defun ntd/c-tab ()
  (interactive)
  (if (c-region-is-active-p)
      (ntd/c-format (region-beginning) (region-end))
    (cond
     ;; Try Eglot
     ((bound-and-true-p eglot--managed-mode)
      (ntd/c-tab-or-format (function eglot-format)))
     ;; Try clang-format
     ((fboundp 'clang-format-region)
      (ntd/c-tab-or-format (function clang-format-region)))
     ;; Basic
     (t
      (c-indent-line-or-region)))))
