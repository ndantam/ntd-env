;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;;;;;
;;  PACKAGES  ;;
;;;;;;;;;;;;;;;;
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
  )

(with-eval-after-load 'package

  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))

  (use-package yaml-mode :ensure t)

  (use-package eglot)

;; End of packages
  )
