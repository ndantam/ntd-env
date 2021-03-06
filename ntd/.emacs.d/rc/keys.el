;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;;;;;;;;
;;  GLOBAL KEYS  ;;
;;;;;;;;;;;;;;;;;;;

;; woman
(global-set-key "\C-cm" (lambda () (interactive)
                          (if (one-window-p) (split-window))
                          (let ((buf (current-buffer)))
                            (other-window 1)
                            (switch-to-buffer buf))
                          (woman)))

;; quick compile
(defun my-recompile ()
  (interactive)
  (save-buffer)
  (command-execute 'compile))

(global-set-key [f1] 'my-recompile)

(global-set-key [f2] 'compile)

(defun my-recompile-hook ()
  (local-set-key (kbd "C-c C-c") 'my-recompile))

(add-hook 'autoconf-mode-hook 'my-recompile-hook)
(add-hook 'automake-mode-hook 'my-recompile-hook)
(add-hook 'c-mode-hook 'my-recompile-hook)
(add-hook 'c++-mode-hook 'my-recompile-hook)

(global-set-key "\C-ctK" 'tramp-compile)

;; commenting
(global-set-key "\C-cX" 'comment-region)
(global-set-key "\C-cU" 'uncomment-region)
(global-set-key "\C-cL" 'longlines-mode)

;; expand
(global-set-key "\M-\\" 'hippie-expand)

;; version control
(global-set-key "\C-xvp" 'vc-update)
(global-set-key "\C-xve" 'ediff-revision)
(global-set-key "\C-cc" 'magit-status)

(global-set-key "\C-cvl" 'magit-display-log)
(global-set-key "\C-cvL"  'magit-log-long)

(global-set-key "\C-cv=" 'magit-diff-working-tree)


;; toggle meubar
(global-set-key "\C-cM" (lambda () (interactive)
                           (if menu-bar-mode
                               (menu-bar-mode -1)
                             (menu-bar-mode 1))))
;; Window switching
(global-set-key "\C-co" 'other-window)
(global-set-key "\C-cp" (lambda () (interactive) (other-window -1)))

;; These seem unhelpful
;;(global-set-key "\C-ch" 'windmove-left)
;;(global-set-key "\C-cl" 'windmove-right)
;;(global-set-key "\C-ck" 'windmove-up)
;;(global-set-key "\C-cj" 'windmove-down)

(global-set-key "\C-ci" 'other-frame)

;; Encryption
(global-set-key "\C-ces" 'pgg-encrypt-symmetric-region)
(global-set-key "\C-ceS" 'pgg-encrypt-symmetric)
(global-set-key "\C-ced" 'pgg-decrypt-region)
(global-set-key "\C-ceD" 'pgg-decrypt)
(global-set-key "\C-cea" 'pgg-encrypt-region)
(global-set-key "\C-ceA" 'pgg-encrypt)

;; server (just use daemon mode)
;;(global-set-key "\C-cS" 'server-start)

;; from the emacswiki
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)


;; WL reload
(global-set-key [f6] 'wl-folder-check-all)

;; reload file
(global-set-key [f5] 'revert-buffer)
