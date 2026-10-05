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
  (when (and start end (integer-or-marker-p start) (integer-or-marker-p end))
    (let ((end (copy-marker end t))
          (start (copy-marker start t)))
      (save-excursion
        (goto-char start)
        (while (re-search-forward "^\\(>+\\) +" end t)
          (replace-match "\\1")
          (beginning-of-line))
        (goto-char start)
        (while (re-search-forward "^\\(>+\\)" end t)
          (replace-match "\\1 "))))))


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

(let ((start-quote (rx (opt (or (* (regex "[ \t]*>"))
                                  (seq alpha ">")))
                         (regex "[ \t\f]*")))
      (quoted-mail-header (rx (+ (regex "[ \t]*") ">")
                              (regex "[ \t]*")
                              (or "From" "Sent" "To" "Subject" "Cc" "Date")
                              ":"))
        ; (mdheader "[=\\-]+[ \t]*$")
        (closing (rx (or (regex "[Tt]hanks")
                         (seq (regex "[Bb]est +") (opt (or (regex "[Rr]egards")
                                                           (regex "[Ww]ishes"))))
                         (regex "[Ww]arm +[Rr]egards")
                         (regex "[Cc]heers")
                         )
                     (regex ", *$")))
        (sig (rx (or "-" "/")
                 (+ alpha)))
        )
    (defvar ntd/mail-paragraph-start)
    (setq ntd/mail-paragraph-start
          ;; Should match start of lines that start or separate paragraphs
          (concat
           start-quote ; optional start quote
           "\\(?:"
           (mapconcat #'identity
                      (list
                       "\f" ; starts with a literal line-feed
                       "[ \t\f]*$" ; space-only line
                       ;; "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                       "[ \t]*[*+-][ \t]+" ; unordered list item
                       "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                       "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                       "[ \t]*:[ \t]+" ; definition
                       "|" ; table or Pandoc line block
                       "#+" ; header
                       quoted-mail-header
                       closing
                       sig
                       )
                      "\\|")
           "\\)"))
    (defvar ntd/mail-paragraph-separate)
    (setq ntd/mail-paragraph-separate
          ;; Should match lines that separate paragraphs without being
          ;; part of any paragraph:
          (concat
           start-quote ; optional start quote
           "\\(?:"
           (mapconcat #'identity
                      (list
                       "[ \t\f]*$" ; space-only line
                       ;; "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                       ;; The following is not ideal, but the Fill customization
                       ;; options really only handle paragraph-starting prefixes,
                       ;; not paragraph-ending suffixes:
                       ".*  $" ; line ending in two spaces
                       "\\(?:   \\)?[-=]+[ \t]*$" ;; setext
                       "[ \t]*\\[\\^\\S-*\\]:[ \t]*$"
                       ; mdheader
                       ) ; just the start of a footnote def
                      "\\|")
           "\\)")))


;;; RFC 3676 format=flowed support.
;;; -------------------------------
;;
;;; Convert markdown-ish emails to format=flowed.  Hard newlines mark
;;; no-flow lines.  Lines before paragraph-boundaries are also
;;; no-flow.  Blocks beginning with a short-line are no-flow until the
;;; next paragraph.

(defun ntd/harden-newlines (start end &rest _)
  "Mark newlines at paragraph boundaries region as hard."
  (interactive "r")
  ;; Fixup when calling post-yank
  (unless start
    (setq start (min end (point))
          end (max end (point))))
  ;; Based on USE-HARD-NEWLINES
  (when (and start end (integer-or-marker-p start) (integer-or-marker-p end))
    (let ((end (copy-marker end t))
          (start (copy-marker start t)))
      (save-excursion
        (goto-char start)
        (beginning-of-line)
        (cl-flet ((harden-prev (pos)
                    (when (< (point-min) pos)
                      (set-hard-newline-properties (1- pos) pos))))
          (while (< (point) end)
            (let ((pos (point)))
              (delete-trailing-whitespace (line-beginning-position) (line-end-position))
              (cond
               ;; paragraph-separate: newlines before and after are hard.
               ((looking-at paragraph-separate)
                (harden-prev pos)
                (end-of-line)
                (unless (eobp)
                  (set-hard-newline-properties (point) (1+ (point)))))
               ;; paragraph-start: newline before is hard.
               ((looking-at paragraph-start)
                (harden-prev pos)))
              ;; Advance line
              (forward-line)))
          ;; Check next line
          (when (and (not (eobp))
                     (or (looking-at-p paragraph-separate)
                         (looking-at-p paragraph-start)))
            (harden-prev (point))))))))

(defun ntd/flowable ()
  "Check if current line is flowable"
  (interactive)
  (save-excursion
    (let* ((end (line-end-position))
           (beg (line-beginning-position)))
      (beginning-of-line)
      (not (or
            (get-text-property end 'hard)                ; hard newline
            (< (- end beg) (/ fill-column 2))            ; short line
            (looking-at-p "[ \t>]*[[:alpha:]]+>[ \t>]*") ; named quote
            (looking-at-p paragraph-separate)            ; paragraph separator
            (progn (goto-char end)                       ; next line not new paragraph
                   (when (< (point) (point-max))
                     (forward-char)
                     (or (looking-at-p paragraph-start)
                         (looking-at-p paragraph-separate)))))))))

(defun ntd/soft-flow ()
  "Turn soft newlines \n\s for format=flowed"
  (interactive)
  ;; Note: mime-edit-translate-hooks get overwritten paragraph-start
  ;; and paragraph-end.
  (save-excursion
    (mail-text)
    (while (re-search-forward "[^ ]\n" nil t)
      (backward-char)
      (cond
       ;; Check if flowable
       ((ntd/flowable)
        (insert " ")
        (forward-line 1))
       ;; Skip short-line blocks up to next paragraph
       ((and (< (- (line-end-position) (line-beginning-position))
                (/ fill-column 2))
             (progn (beginning-of-line)
                    (not (looking-at-p paragraph-separate))))
        (forward-line 1)
        (re-search-forward (concat "^" paragraph-start)
                           nil t)
        (beginning-of-line))
       ;; Otherwise, advance line
       (t (forward-line 1))))))

(defun ntd/mail-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  ;; Based on markdown-adaptive-fill-function
  (cond
   ;; List item inside blockquote
   ((looking-at "^[ \t]*>[ \t]*\\(\\(?:[0-9]+\\|#\\)\\.\\|[*+:-]\\)[ \t]+")
    (replace-regexp-in-string
     "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;; Blockquote
   ;; "^[ \t]*\\(?1:[A-Z]?>\\)\\(?2:[ \t]*\\)\\(?3:.*\\)$"
   ((looking-at markdown-regex-blockquote)
    (buffer-substring-no-properties (match-beginning 0) (match-end 2)))
   ;; List items
   ((looking-at markdown-regex-list)
    (match-string-no-properties 0))
   ;; Footnote definition
   ((looking-at-p markdown-regex-footnote-definition)
    "    ") ; four spaces
   ;; No match
   (t nil)))

;; SEE ALSO: ~/.wl
