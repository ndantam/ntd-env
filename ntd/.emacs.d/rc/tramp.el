;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(setq tramp-default-method "rsync"
      tramp-terminal-type "tramp"
      tramp-use-ssh-controlmaster-options nil)

(eval-after-load 'tramp
  '(add-to-list 'ntd/kill-emacs-hooks
                #'tramp-cleanup-all-connections))
