;; -*- mode: emacs-lisp -*-;;
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
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")
               )

  ;; update packages list if we are on a new install
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Get use-package
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;; Require use-package
  (require 'use-package))

;;;;;;;;;;;;;;;;
;;  Packages  ;;
;;;;;;;;;;;;;;;;
(with-eval-after-load 'use-package

  (use-package editorconfig
    :ensure t
    :config (editorconfig-mode 1))

  (use-package yaml-mode :ensure t)
  (use-package eglot :ensure t)
  (use-package py-autopep8 :ensure t)

  ;; End of packages
  )
