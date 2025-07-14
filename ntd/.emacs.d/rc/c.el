;; -*- mode: emacs-lisp -*-;;
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
