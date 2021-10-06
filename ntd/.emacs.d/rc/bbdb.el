;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;
;; BBDB ;;
;;;;;;;;;;

;; file where things will be saved
(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading

;; v3

;; Example
;; https://blog.petitepomme.net/post/28547901478/installing-and-configuring-bbdb-3

;; load bbdb
(require 'bbdb-loaddefs)
(require 'bbdb)


;; initialization
(bbdb-initialize 'wl)
(bbdb-mua-auto-update-init 'wl)

;; (bbdb-mua-auto-update-init 'gnus 'message)
;; (bbdb-initialize 'gnus 'message)
;; size of the bbdb popup

(setq bbdb-pop-up-window-size 10
      bbdb-mua-pop-up nil)


;; What do we do when invoking bbdb interactively
(setq bbdb-mua-update-interactive-p '(query . create))

;; Make sure we look at every address in a message and not only the
;; first one
(setq bbdb-message-all-addresses t)

;; Auto-Create records
(setq bbdb-update-records-p 'create)

(setq bbdb-ignore-message-alist ;; don't ask about fake addresses
      ;; NOTE: there can be only one entry per header (such as To, From)
      ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
      `(( "From" . ,(rx (or (regex "no.?reply")
                            "announce" "notification" "list" "announce" "DAEMON" "daemon"
                            "papercept.net"
                            "facebookmail" "twitter" "github" "from" "From")))
        ( "To"   . "Recipient")))


(setq bbdb-electric t                        ;; be disposable with SPC
      bbdb-pop-up-layout nil
      bbdb-mail-avoid-redundancy t)



;; (add-hook
;;  'gnus-summary-mode-hook
;;  (lambda ()
;;    (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)
;;    ))

;; ;; This is for non interactive bbdb invocation, but it was a bit
;; ;; too much
;; (setq bbdb-update-records-p 'query)


;; V2
;; (setq
;;     bbdb-offer-save 1                        ;; 1 means save-without-asking


;;     bbdb-use-pop-up nil                      ;; allow popups for addresses
;;     bbdb-popup-target-lines  1               ;; very small

;;     bbdb-dwim-net-address-allow-redundancy t ;; always use full name
;;     bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

;;     bbdb-always-add-address t                ;; add new addresses to existing...
;;                                              ;; ...contacts automatically
;;     bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

;;     bbdb-completion-type nil                 ;; complete on anything

;;     bbdb-complete-name-allow-cycling t       ;; cycle through matches
;;                                              ;; this only works partially

;;     bbbd-message-caching-enabled t           ;; be fast
;;     bbdb-use-alternate-names t               ;; use AKA


;;     bbdb-elided-display t                    ;; single-line addresses

;;     ;; auto-create addresses from mail
;;     bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
;;     bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
;;     ;; NOTE: there can be only one entry per header (such as To, From)
;;     ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

;;     '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter\\|github\\.com\\|from\\|From")
;;       ( "To"   . "Recipient")))
