;; -*- mode: emacs-lisp -*-;;
;; .emacs
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;;;;;;
;;  Wanderlust  ;;
;;;;;;;;;;;;;;;;;;

(defun ntd-email-addr (a b c)
  (concatenate 'string a "@" b "." c))

(setq user-mail-address (ntd-email-addr "ndantam" "mines" "edu"))
(setq wl-user-mail-address-list (list user-mail-address))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(defun my-wl-summary-sort-hook ()
  (wl-summary-rescan "date"))

(add-hook 'wl-summary-prepared-hook 'my-wl-summary-sort-hook)




;; SEE ALSO: ~/.wl
