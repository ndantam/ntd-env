;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;;  C  ;;
;;;;;;;;;

(c-add-style "ros-cc-style"
             '("k&r"
               (c-basic-offset . 2)
               (c-offsets-alist . ((innamespace . [0])
                                   (member-init-intro . [0])))))

(c-add-style "user"
             '("linux"
               (c-basic-offset . 4)
               (c-offsets-alist . ((innamespace . [0])
                                   (inextern-lang . [0])))))

(setq c-default-style "user")


;;;;;;;;;;;;;;
;;  clangd  ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.clangd\\'" . yaml-mode))
