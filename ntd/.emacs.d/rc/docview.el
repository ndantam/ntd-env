;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
