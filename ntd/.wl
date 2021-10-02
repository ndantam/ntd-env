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


(setq wl-icon-directory "~/.emacs.d/pixmaps/")




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
      wl-bcc user-mail-address
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
      wl-insert-message-id nil
      wl-forward-subject-prefix "Fwd: "

      wl-summary-width 160
      wl-folder-check-async t
      wl-draft-always-delete-myself t
      wl-use-scoring nil
      wl-stay-folder-window t
      wl-draft-use-frame t
      wl-folder-window-width 35
      wl-ask-range nil
      wl-summary-width nil
      mime-edit-split-message nil
      ;; Prefetch 1 MB
      wl-message-buffer-prefetch-threshold (expt 2 20)
      ;; No confirm for 10 MB
      elmo-message-fetch-threshold (* 10 (expt 2 20))
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




;; ;;;;;;;;;;;;;
;; ;; WL BBDB ;;
;; ;;;;;;;;;;;;;

;; (require 'bbdb-wl)

;; (bbdb-wl-setup)

;; ;; i don't want to store addresses from my mailing folders
;; (setq
;;  bbdb-wl-folder-regexp    ;; get addresses only from these folders
;;  "^\.\.gt$")    ;;

;; (define-key wl-draft-mode-map (kbd "C-c <tab>") 'bbdb-complete-name)
