;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;
;;  FONT  ;;
;;;;;;;;;;;;

(set-face-background 'region               "#555555")
(set-face-foreground 'mode-line             "white")
(set-face-background 'mode-line             "#333333")

(set-face-background 'default              "black")
(set-face-foreground 'default              "green")

(set-mouse-color "green")

(add-to-list 'default-frame-alist '(background-mode . dark))

(mouse-wheel-mode t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
