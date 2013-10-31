;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;; PGP ;;
;;;;;;;;;

(autoload 'pgg-encrypt-region "pgg" "Encrypt the current region." t)
(autoload 'pgg-encrypt-symmetric-region "pgg" "Encrypt the current region with symmetric algorithm." t)
(autoload 'pgg-decrypt-region "pgg" "Decrypt the current region." t)
(autoload 'pgg-sign-region "pgg" "Sign the current region." t)
(autoload 'pgg-verify-region "pgg" "Verify the current region." t)
(autoload 'pgg-insert-key "pgg" "Insert the ASCII armored public key." t)
(autoload 'pgg-snarf-keys-region "pgg" "Import public keys in the current region." t)
