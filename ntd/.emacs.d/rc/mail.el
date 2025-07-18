;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; .emacs
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;
;;  Text  ;;
;;;;;;;;;;;;

(defun ntd/fix-quote-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\(>+\\) +" end t)
      (replace-match "\\1")
      (beginning-of-line))
    (goto-char start)
    (while (re-search-forward "^\\(>+\\)" end t)
      (replace-match "\\1 "))))


;; (setq adaptive-fill-regexp
;;       ;; Default:
;;        (purecopy "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+[ \t]*\\)*"))
;;       ;; (rx (seq (regex "[ \t]*")
;;       ;;          (| (* (seq (regex "[-–!|#%;>*·•‣⁃◦]+")
;;       ;;                     (regex "[ \t]*")))
;;       ;;             (regex "[[:alnum:] ]+>[ \t]*")))))


;;;;;;;;;;;;;;;;;;
;;  Wanderlust  ;;
;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path
;;              "~/git/3rdparty/wanderlust/wl/")
;; (add-to-list 'load-path
;;              "~/git/3rdparty/wanderlust/elmo/")

(defun ntd/email-addr (a b c)
  (concat a "@" b "." c))

(setq user-mail-address (ntd/email-addr "ndantam" "mines" "edu"))
(setq wl-user-mail-address-list (list user-mail-address))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(defun my-wl-summary-sort-hook ()
  (wl-summary-rescan "date"))

(add-hook 'wl-summary-prepared-hook 'my-wl-summary-sort-hook)


(defun ntd/fill-mail ()
  (interactive)
  (save-excursion
    (mail-text)
    (fill-region (point)
                 (point-max))))


;; SEE ALSO: ~/.wl
