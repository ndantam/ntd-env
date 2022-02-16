;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'flyspell-mode)


(defun ntd/text-mode-hook ()
  (turn-on-auto-fill)
  (flyspell-mode)
  (flymake-mode)
  (flymake-proselint-setup))


(add-hook 'text-mode-hook
          #'ntd/text-mode-hook)
