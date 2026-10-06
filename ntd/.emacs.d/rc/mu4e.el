;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


(require 'mu4e)

;; these are actually the defaults
(setq mu4e-sent-folder   "/mines/Sent"       ;; folder for sent messages
      mu4e-drafts-folder "/mines/Drafts"     ;; unfinished messages
      mu4e-trash-folder  "/mines/Trash"     ;; trashed messages
      mu4e-refile-folder "/mines/Archive")   ;; saved messages

(setq mu4e-bookmarks
      '(
        ;; Default
        ;; (:name "Unread messages"
        ;;        :query "flag:unread AND NOT flag:trashed"
        ;;        :key ?u)
        (:name "Today's messages"
               :query "date:today..now"
               :key ?t)
        (:name "Last 7 days"
               :query "date:7d..now"
               :hide-unread t :key ?w)
        ;; (:name "Messages with images"
        ;;        :query "mime:image/*"
        ;;        :key ?p)
        ;; My
        (:name "Unread messages"
               :query "flag:unread AND maildir:/mines/INBOX"
               :key ?u)
        (:name "Starred Messages"
               :query "flag:flagged"
               :key ?f)))

(setq mu4e-maildir-shortcuts
      '(
        ("/mines/INBOX" . ?i)
        ("/mines/Drafts" . ?d)
        ("/mines/Sent" . ?s)
        ("/mines/Trash" . ?t)
        ))

(setq mu4e-get-mail-command "mbsync mines-sync"
      mu4e-update-interval nil)


;; Sending mail
(setq sendmail-program "msmtp"
      smtpmail-async-p nil
      wl-draft-send-mail-function 'wl-draft-send-mail-with-sendmail
      )

(setq sendmail-program "msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))



;;; Customization ;;;
;;; ------------- ;;;

(setq mu4e-split-view 'vertical)
(setq mu4e-headers-visible-columns 80)

;; does not sum to mu4e-headers-visible-columns-80, but somehow OK?
(setq mu4e-headers-fields
      '((:subject . 55)
        (:from . 16)
        (:human-date . 12)
        (:flags . 6)
        ;; (:mailing-list . 10) ; hide
        ))

(setq mu4e-headers-date-format "%Y-%m-%d"
      mu4e-headers-time-format "%I:%M %p")

;; mu4e uses gnus to display messages.  Need to edit this variable to
;; actually get the header.
(setq gnus-visible-headers
      (eval `(rx (or (regex "^User-Agent:")
                     (regex "^X-Mailer:")
                     (regex ,gnus-visible-headers)))))

(add-to-list 'mu4e-header-info-custom
  '(:user-agent . ( :name "User-Agent"
                    :shortname "UA"
                    :help "The User-Agent / Mailer used by the sender"
                    :function (lambda (msg)
                                (or (mu4e-fetch-field msg "User-Agent") "?")))))

(setq mu4e-view-fields
      '(:from
        :to
        :cc
        :subject
        ;; :flags
        :date
        ;; :mailing-list
        ; ;:tags
        :user-agent
        :maildir
        ))

(setq mu4e-headers-results-limit 4096)

(defun ntd/mu4e-compose-mode-hook ()
  "Automatically add a Bcc header to self when composing."
  (save-excursion
    (message-add-header "Bcc: ndantam@mines.edu\n")))

(add-hook 'mu4e-compose-mode-hook
          'ntd/mu4e-compose-mode-hook)


(setq mu4e-headers-advance-after-mark nil)

;;; Simplify Rendering
(add-to-list 'mm-discouraged-alternatives "text/html")
(add-to-list 'mm-discouraged-alternatives "text/richtext")

;; HTML email can just die
(setq shr-use-colors nil
      shr-use-fonts nil
      shr-allowed-images nil
      shr-max-width 80
      gnus-inhibit-images t)

;;; BBDB

;; Initialize BBDB and integrate it with mail/mu4e
(bbdb-initialize 'mu4e 'message)

;; Disable mu4e's built-in address completion to avoid conflicts
(setq mu4e-compose-complete-addresses nil)

;; Optional: Automatically update BBDB records when viewing messages
(setq mu4e-view-rendered-hook 'bbdb-mua-auto-update)

(setq bbdb-mail-user-agent 'mu4e-user-agent
      mu4e-view-show-addresses nil)

(defun ntd/message-mode-hook ()
  (add-to-list 'completion-at-point-functions
               ;; #'message-expand-name ; broken
               #'eudc-capf-complete
               ))

;; Hook BBDB completion into message-mode for address autocompletion
(add-hook 'message-mode-hook
          #'ntd/message-mode-hook)
