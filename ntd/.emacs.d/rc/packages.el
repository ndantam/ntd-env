;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;;
;;  Setup  ;;
;;;;;;;;;;;;;
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/"))
  ;(add-to-list 'package-archives '
               ;("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; (add-to-list 'package-archives
  ;;              '("melpa" . "https://melpa.org/packages/")
  ;;              )

  ;; update packages list if we are on a new install
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Get use-package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;; Require use-package
  (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;
;;  System Packages  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun ntd/sudo (args)
  "Run a sudo command using SUDO_ASKPASS and emacsclient prompt."
  (let ((default-directory "/") ;; sudo may need this
        (process-environment (cons (format "SUDO_ASKPASS=%s"
                                           (expand-file-name "~/bin/ecaskpass"))
                                   process-environment)))
    (make-process
     :name "sudo-async"
     :buffer "*sudo-output*"
     :command (cl-list* "sudo" "-A" args)
     :sentinel (lambda (proc event)
                 (when (memq (process-status proc) '(exit signal))
                   (message "Sudo command finished with %s" event)
                   (display-buffer (process-buffer proc)))))))

(defun ntd/install ()
  (let ((packages))
    (cl-labels ((cmd (command &optional system-package)
                     (unless (executable-find command)
                       (push (or system-package command)
                             packages)))
                (pkg (package &optional system-package)
                     (unless (package-installed-p package)
                       (push (if system-package
                                 system-package
                               (concat "elpa-" (symbol-name package)))
                             packages)))
                (rec (f args)
                     (dolist (a args)
                       (if (atom a)
                           (funcall f a)
                         (apply f a))))
                (cmds (args)
                      (rec #'cmd args))
                (pkgs (args)
                      (rec #'pkg args)))
      (cmds ntd/commands)
      (pkgs ntd/packages)
      (when packages
        (when (y-or-n-p (format "Install %s?" packages))
          (ntd/sudo (cl-list* "apt-get" "install" "-yq"
                              packages)))))))

;;;;;;;;;;;;;;;;
;;  Packages  ;;
;;;;;;;;;;;;;;;;

(setq ntd/commands `("clang-format"
                     "clangd-tidy"
                     "clangd"
                     )
      ntd/packages `(
                     editorconfig
                     eglot
                     magit
                     py-autopep8
                     (slime "slime")
                     tuareg
                     yaml-mode
                     ))

(use-package editorconfig
  :config (editorconfig-mode 1))
(use-package eglot)
(use-package py-autopep8)
(use-package yaml-mode)
