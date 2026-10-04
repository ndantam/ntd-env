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

(defun ntd/mail-fill-paragraph (&optional justify)
  (or (mail-mode-fill-paragraph justify)
      (markdown-fill-paragraph justify)))

(defun ntd/harden-newlines (start end &rest _)
  "Mark all newlines in the yanked region as hard."
  (interactive "r")
  ;; Fixup when calling post-yank
  (unless start
    (setq start end
          end (point)))
  (when (and start end (integer-or-marker-p start) (integer-or-marker-p end))
    (save-excursion
      (goto-char start)
      (let ((end-marker (copy-marker end)))
        (while (search-forward "\n" end-marker t)
          (let ((newline-pos (1- (point))))
            (when (or (looking-at paragraph-start)
                      (looking-at paragraph-separate))
              (put-text-property newline-pos (point) 'hard t))))
        (set-marker end-marker nil))))))


;; SEE ALSO: ~/.wl
