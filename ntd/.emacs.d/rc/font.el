;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;
;;  FONT  ;;
;;;;;;;;;;;;

(defun ntd/colors ()
  (set-face-background 'region               "#333333")
  (set-face-foreground 'mode-line             "white")
  (set-face-background 'mode-line             "#333333")

  (set-face-background 'default              "black")
  (set-face-foreground 'default              "green")

  (set-mouse-color "green")

  (add-to-list 'default-frame-alist '(background-mode . dark)))

(ntd/colors)

(defun ntd/mines-colors ()
  (let ((mines-navy-blue "#21314d")
        (mines-light-blue "#92a2bd")
        (mines-silver "#8b8d8e")
        (mines-white "#ffffff")
        (mines-colorado-red "#d2492a")
        (mines-other-blue "#263f6a")
        (mines-pale-blue "#ced5dd")
        (mines-cool-gray "#b2b4b3"))

    (set-face-background 'region               mines-pale-blue)
    (set-face-foreground 'mode-line            "white")
    (set-face-background 'mode-line            mines-navy-blue)

    (set-face-background 'default              mines-white)
    (set-face-foreground 'default              "black")

    (set-face-attribute 'font-lock-comment-face nil
                       :slant 'italic
                       :foreground mines-colorado-red)

    (set-face-attribute 'font-lock-function-name-face nil
                        :foreground mines-other-blue)

    (set-face-attribute 'font-lock-keyword-face nil
                        :weight 'bold
                        :foreground mines-navy-blue)

    (set-face-attribute 'font-lock-string-face nil
                        :weight 'normal
                        :foreground mines-silver)

    (setq viper-insert-state-cursor-color mines-colorado-red
          viper-emacs-state-cursor-color mines-colorado-red
          viper-replace-overlay-cursor-color mines-colorado-red)

    (set-cursor-color mines-colorado-red)

    (add-to-list 'default-frame-alist '(background-mode . light)))
  )

;(ntd/mines-colors)


(mouse-wheel-mode t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :height 120)
