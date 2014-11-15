;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;; ERC ;;
;;;;;;;;;

(autoload 'erc-tls "erc")

(defun irobot-erc ()
  (interactive)
  (erc-tls :server  "leprosy.wardrobe.irobot.com" :port 6667
           :nick "ndantam" :full-name "Neil Dantam")
  (erc-join-channel "#research"))

(setq erc-autojoin-channels-alist '(("leprosy.wardrobe.irobot.com"
                                     "#research")))
