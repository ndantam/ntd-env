;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;
;; VIPER ;;
;;;;;;;;;;;

(setq viper-mode t)
(require 'viper)
(setq viper-always t)
(setq viper-vi-state-cursor-color "green")

(when (boundp 'viper-emacs-state-mode-list)
  (mapc (lambda (mode)
          (add-to-list 'viper-emacs-state-mode-list mode))
        '(magit-key-mode
          slime-connection-list-mode
          term-mode
          term-line-mode)))
