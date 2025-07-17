;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;
;; MAXIMA  ;;
;;;;;;;;;;;;;
(when-host ("daneel" "leela")
  (add-to-list 'load-path "/usr/share/maxima/5.22.1/emacs/")
  (add-to-list 'load-path "/usr/share/maxima/5.24.0/emacs/"))

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

(setq imaxima-fnt-size "Large")
(setq auto-mode-alist (cons '("\.mac$" . maxima-mode) auto-mode-alist))
