;; mode:-*-emacs-lisp-*-
;; Emacs initialization file for Wanderlust MUA
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(require 'w3m)
(require 'mime-w3m)

;; NOTES:
;; - MSGDB is stored under ~/.elmo

;; (require 'bbdb)
;; (bbdb-initialize 'wl)
;; (bbdb-mua-auto-update-init 'wl)



(defun ntd/from (name email-address)
  (concat  name " <" email-address ">"))

(setq elmo-passwd-storage-type 'auth-source

      ;; Mines IMAP Setup
      elmo-imap4-default-server "outlook.office365.com"
      elmo-imap4-default-user user-mail-address
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port 993
      elmo-imap4-default-stream-type 'ssl

      ;; Mines SMTP Setup
      wl-from (ntd/from "Neil T. Dantam" user-mail-address)
      wl-smtp-posting-server "smtp.office365.com"
      wl-local-domain "mines.edu"
      wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "login"
      wl-smtp-posting-user user-mail-address
      )

;; View
(setq mime-view-type-subtype-score-alist
  '(((text . plain) . 4)
    ((text . enriched) . 3)
    ((text . html) . 2)
    ((text . richtext) . 1)))

;; Misc
(setq elmo-passwd-storage-type 'auth-source
      wl-summary-width 160
      wl-folder-check-async t
      wl-use-scoring nil
      wl-stay-folder-window t
      wl-folder-window-width 35
      wl-ask-range nil
      ;; Prefetch 1 MB
      wl-message-buffer-prefetch-threshold (expt 2 20)
      ;; No confirm for 10 MB
      elmo-message-fetch-threshold (* 10 (expt 2 20))
      )

;; All the would needs to be UTF-8
(setq wl-mime-charset 'utf-8
      elmo-mime-charset 'utf-8
      default-mime-charset 'utf-8)

;; Try to find the icons
(let ((d (expand-file-name "icons/"
                           (file-name-directory (locate-library "wl")))))
  (when (and (file-exists-p d)
             (file-accessible-directory-p d))
    (setq wl-icon-directory d)))

;; Drafts
(setq wl-forward-subject-prefix "Fwd: "
      wl-draft-use-frame t
      wl-bcc user-mail-address
      wl-draft-always-delete-myself t
      ;; wl-insert-message-id nil
      ;; mime-edit-split-message nil
      ; wl-draft-cite-date-format-string ;; TODO
      )

;; Folder Setup
(setq elmo-maildir-folder-path "~/.maildir"
      wl-draft-folder "..drafts"       ; wandlerlust's default is MH format
      wl-trash-folder "..trash"
      wl-spam-folder  "..trash"
      wl-queue-folder "..queue"        ; we don't use this
      wl-fcc "..sent"                  ; sent msgs go to the "sent"-folder
      wl-fcc-force-as-read t           ; mark sent messages as read
      wl-default-spec "..")





(setq wl-message-ignored-field-list '("^.*:")
      ;; ..but these five
      wl-message-visible-field-list '("^From:"
                                      "^Newsgroups:"
                                      "^Subject:"
                                      "^Date:"
                                      "^To:"
                                      "^Cc:"
                                      "^User-Agent:")
      wl-message-sort-field-list '("^From:"
                                   "^Newsgroups:"
                                   "^Subject:"
                                   "^Date:"
                                   "^To:"
                                   "^Cc:"
                                   "^User-Agent:"))



;;;;;;;;;;;;;;;
;; COMPOSING ;;
;;;;;;;;;;;;;;;

;; Signature Setup
(setq signature-file-name "~/.signature"
      signature-insert-at-eof t
      signature-delete-blank-lines-at-eof t)

;; FORMAT = FLOWED

;; Desired behavior:
;;  - Add the format=flowed tag
;;  - Automatically insert flowed spaces when filling paragraphs


;; See:
;; - https://emacs.stackexchange.com/questions/19296/retooling-fill-paragraph-to-append-trailing-spaces
;; - https://www.reddit.com/r/emacs/comments/7v2b3q/emacs_email_and_format_flowed/
;; - https://www.emacswiki.org/emacs/WlFormatFlowed

(defun my-soft-flow ()
  "Turn soft newlines \n\s for format=flowed"
  (interactive)
  (save-excursion
    (mail-text)
    (while (re-search-forward "[^ ]\n" nil t)
      (backward-char)
      (unless (get-text-property (point) 'hard)
        (insert " ")
        (forward-char))
      ;; skip the newline
      (forward-char))))

(defun my-translate-hook ()
  ;; format=flowed
  (my-soft-flow) ; soft \n -> \n\s
  (save-excursion
    (mail-text)
    (mime-edit-insert-tag "text" "plain" "; format=flowed; charset=UTF-8"))
  ;; Insert Signature
  (mime-edit-insert-signature))

(add-hook 'mime-edit-translate-hook 'my-translate-hook)

(require 'messages-are-flowing)
(defun my-mime-edit-hook ()
  ;; * Maximimum line length from RFC 5322 is 78 chars, plus \r\n.
  ;; But we add the space for format=flowed.
  ;;
  ;; * However, the intarweb says that 66 is better for readability.
  ;;
  ;; * But new-fangled mail readers in the browser and phone are gonna
  ;; screw it all up anyway.
  (set (make-local-variable 'fill-column) 66)

  (use-hard-newlines nil t)
  (messages-are-flowing-use-and-mark-hard-newlines)
  (messages-are-flowing--mark-hard-newlines (mail-text) (point-max)))

(add-hook 'mime-edit-mode-hook 'my-mime-edit-hook)
(add-hook 'wl-mail-setup-hook 'my-mime-edit-hook)


(defun my-mime-view-hook ()
  ;(whitespace-mode 1)
  nil)

(add-hook 'mime-view-mode-hook 'my-mime-view-hook)

;;;;;;;;;;;;;
;; WL BBDB ;;
;;;;;;;;;;;;;

(require 'bbdb-wl)

; (bbdb-wl-setup)

;; ;; i don't want to store addresses from my mailing folders
;; (setq
;;  bbdb-wl-folder-regexp    ;; get addresses only from these folders
;;  "^\.\.gt$")    ;;

(define-key wl-draft-mode-map (kbd "C-c <tab>") 'bbdb-complete-mail)


;; (defun bbdb-offer-save () (bbdb-save))
;; (defun bbdb-flush-all-caches () )

;; (setq mime-view-type-subtype-score-alist
;;   '(((text . plain) . 4)
;;     ((text . enriched) . 3)
;;     ((text . html) . 2)
;;     ((text . richtext) . 1)))


;; (setq wl-insert-message-id nil
;;       wl-forward-subject-prefix "Fwd: "
;;       ;;wl-folder-check-async t
;;       wl-draft-always-delete-myself t
;;       wl-use-scoring nil
;;       wl-stay-folder-window t
;;       wl-folder-window-width 35
;;       wl-ask-range nil
;;       wl-summary-width nil
;;       mime-edit-split-message nil)
;
; (setq wl-default-sync-range "update"
;;       wl-folder-sync-range-alist '(("^.*$" . "update")))

;; ;;;;;;;;;;;;;;;;;;;
;; ;; FOLDER CONFIG ;;
;; ;;;;;;;;;;;;;;;;;;;

;; ;; Everybody else uses maildir, so let's use it too
;; ;;
;; ;; Note: we need two dots (..) for maildir folders. First one tells
;; ;; wanderlust this is a maildir, second one because that's how Dovecot
;; ;; and Courier name maildir folders.
;; (setq elmo-maildir-folder-path "~/.maildir"
;;       wl-draft-folder "..drafts"       ; wandlerlust's default is MH format
;;       wl-trash-folder "..trash"
;;       wl-spam-folder  "..trash"
;;       wl-queue-folder "..queue"        ; we don't use this
;;       wl-fcc "..sent"                  ; sent msgs go to the "sent"-folder
;;       wl-fcc-force-as-read t           ; mark sent messages as read
;;       wl-default-spec "..")

;; (setq wl-default-folder "..gt")

;; ;; ignore  all fields
;; (setq wl-message-ignored-field-list '("^.*:")
;;       ;; ..but these five
;;       wl-message-visible-field-list '("^From:"
;;                                       "^Newsgroups:"
;;                                       "^Subject:"
;;                                       "^Date:"
;;                                       "^To:"
;;                                       "^Cc:"
;;                                       "^User-Agent:")
;;       wl-message-sort-field-list '("^From:"
;;                                    "^Newsgroups:"
;;                                    "^Subject:"
;;                                    "^Date:"
;;                                    "^To:"
;;                                    "^Cc:"
;;                                    "^User-Agent:"))


;; ;;;;;;;;;;;;;;
;; ;; Accounts ;;
;; ;;;;;;;;;;;;;;


;; (defun ntd-from (user host tld)
;;   (concatenate 'string "Neil T. Dantam <"
;;                (ntd-email-addr user host tld) ">"))

;; (defun ntd-gmail-folder (name)
;;   (concatenate 'string
;;                ".." name "@" (concat (reverse (append "liamg" nil)))))

;; (defun ntd-wl-template (name user host tld)
;;   `(,name ("Bcc" . ,(ntd-email-addr user host tld))
;;           ("From" . ,(ntd-from user host tld))))

;; (defun ntd-from-regex (user host tld)
;;   (concat "^From: .*" user "@" host "\\." tld))

;; (setq wl-template-alist
;;       (list (ntd-wl-template "gatech" "ntd" "gatech" "edu")
;;             (ntd-wl-template "mechsoph-gmail" "mechsoph" "gmail" "com")
;;             (ntd-wl-template "ntd-gmail" "neil.dantam" "gmail" "com")))

;; (setq wl-nntp-posting-server "news.gmane.org")

;; ;; TODO: drafts and sent folders possibly on IMAP
;; (setq wl-draft-config-alist
;;       `(;; parent folder templates
;;         ((string-match "..gt" wl-draft-parent-folder)
;;          (template . "gatech"))
;;         ((string-match ,(ntd-gmail-folder "mechsoph") wl-draft-parent-folder)
;;          (template . "mechsoph-gmail"))
;;         ((string-match ,(ntd-gmail-folder "ntd") wl-draft-parent-folder)
;;          (template . "ntd-gmail"))
;;         ;; From header templates
;;         (,(ntd-from-regex "ntd" "gatech" "edu")
;;          (template . "gatech")
;;          (wl-from . ,(ntd-from "ntd" "gatech" "edu"))
;;          (wl-smtp-posting-server . "smtp.mail.gatech.edu")
;;          (wl-local-domain . "gatech.edu")
;;          (wl-smtp-connection-type . 'starttls)
;;          (wl-smtp-posting-port . 587)
;;          (wl-smtp-authenticate-type . "plain")
;;          (wl-smtp-posting-user . "ndantam3"))
;;         (,(ntd-from-regex "mechsoph" "gmail" "com")
;;          (template . "mechsoph-gmail")
;;          (wl-from . ,(ntd-from "mechsoph" "gmail" "com"))
;;          (wl-smtp-posting-user . ,(ntd-email-addr "mechsoph" "gmail" "com"))
;;          (wl-smtp-posting-server . "smtp.gmail.com")
;;          (wl-smtp-authenticate-type ."plain")
;;          (wl-smtp-connection-type . 'starttls)
;;          (wl-smtp-posting-port . 587)
;;          (wl-local-domain . "gmail.com")
;;          (wl-message-id-domain . "smtp.gmail.com"))
;;         (,(ntd-from-regex "neil.dantam" "gmail" "com")
;;          (template . "ntd-gmail")
;;          (wl-from . ,(ntd-from "neil.dantam" "gmail" "com"))
;;          (wl-smtp-posting-user . ,(ntd-email-addr "neil.dantam" "gmail" "com"))
;;          (wl-smtp-posting-server . "smtp.gmail.com")
;;          (wl-smtp-authenticate-type ."plain")
;;          (wl-smtp-connection-type . 'starttls)
;;          (wl-smtp-posting-port . 587)
;;          (wl-local-domain . "gmail.com")
;;          (wl-message-id-domain . "smtp.gmail.com"))))

;; ;; mailing lists
;; (labels ((coc-mail (&rest names)
;;                    (loop for name in names
;;                          collect (ntd-email-addr name "cc.gatech" "edu"))))
;;   (setq wl-subscribed-mailing-list
;;         (append (coc-mail "phd-list"
;;                           "robotics-phd"
;;                           "hubo"
;;                           "golemkran"
;;                           "humanoids")
;;               )))
