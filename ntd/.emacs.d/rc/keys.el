;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-;;
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




(global-set-key [f1] 'ntd/recompile)
(global-set-key [f2] 'ntd/compile)
;(global-set-key (kbd "C-c C")  'compile)
;(global-set-key (kbd "C-c k")  'my-recompile)


;;(global-set-key [f1] 'my-recompile)

(defun my-recompile-hook ()
  (local-set-key (kbd "C-c C-c") 'ntd/recompile))

(add-hook 'autoconf-mode-hook 'my-recompile-hook)
(add-hook 'automake-mode-hook 'my-recompile-hook)
(add-hook 'c-mode-hook 'my-recompile-hook)
(add-hook 'c++-mode-hook 'my-recompile-hook)

(global-set-key "\C-ctK" 'tramp-compile)

;; commenting
(global-set-key "\C-cX" 'comment-region)
(global-set-key "\C-cU" 'uncomment-region)
;(global-set-key "\C-cL" 'longlines-mode)

;; clang-format
(defun ntd/clang-format-hook ()
  (local-set-key (kbd "M-q") 'clang-format-region))

(add-hook 'c-mode-hook 'ntd/clang-format-hook)
(add-hook 'c++-mode-hook 'ntd/clang-format-hook)

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
(global-set-key "\C-cO" (lambda () (interactive) (other-window -1)))
;(global-set-key "\C-cp" (lambda () (interactive) (other-window -1)))

;; These seem unhelpful
;;(global-set-key "\C-ch" 'windmove-left)
;;(global-set-key "\C-cl" 'windmove-right)
;;(global-set-key "\C-ck" 'windmove-up)
;;(global-set-key "\C-cj" 'windmove-down)

;; Tab Switching
;(global-set-key "\C-cu" 'tab-next)
;(global-set-key "\C-cU" (lambda () (interactive) (tab-next -1)))

;; Frame Switching
(global-set-key "\C-ci" 'other-frame)
(global-set-key "\C-cI" (lambda () (interactive) (other-frame -1)))

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


;; Browser
(global-set-key (kbd "C-c b") #'browse-url-at-point)


;; reload file
(global-set-key [f5] 'revert-buffer)

;; Shells
(global-set-key "\C-cs" #'ntd/term-zsh)
(global-set-key "\C-cr" #'ntd/term-ssh)


;; Lisp
(eval-after-load "slime"
  '(progn
     (global-set-key "\C-cl" 'slime-selector)

     (global-set-key "\C-cLs" 'slime)
     (global-set-key "\C-cLq" 'slime-quit-lisp)

     (global-set-key "\C-cLc" 'local-slime-connect)
     (global-set-key "\C-cLl" 'slime-load-system)
     (global-set-key "\C-cLr" 'slime-reload-system)

     (global-set-key "\C-cLd" 'slime-disconnect)))


;;;;;;;;;;;;;;
;;; AuCTeX ;;;
;;;;;;;;;;;;;;
(defun ntd/tex-view ()
  (interactive)
  ;;(TeX-command "View" #'TeX-master-file)
  (TeX-pdf-tools-sync-view))

(defun ntd/tex-mk ()
  (interactive)
  (command-execute 'save-buffer)
  ;; TODO: TeX-process-check asks if we want to kill latexmk.  It
  ;; should have a flag to always kill.
  (TeX-command  "LatexMk"
                #'TeX-master-file nil))

;;(define-key tex-mode-map (kbd "\C-c\C-v") #'ntd/tex-view)
;;(define-key tex-mode-map (kbd "\C-cv") #'ntd/tex-view)
(define-key LaTeX-mode-map (kbd "\C-c\C-v") #'ntd/tex-view)
(define-key LaTeX-mode-map (kbd "\C-cv") #'ntd/tex-view)

;;(define-key tex-mode-map (kbd "\C-x\C-s") #'ntd/tex-mk)
(define-key LaTeX-mode-map (kbd "\C-x\C-s") #'ntd/tex-mk)

;;(define-key tex-mode-map (kbd "\C-c\C-s") #'save-buffer)
(define-key LaTeX-mode-map (kbd "\C-c\C-s") #'save-buffer)

(define-key LaTeX-mode-map (kbd "\C-cf") #'pdf-sync-forward-search)

;;;;;;;;;;;
;;; PDF ;;;
;;;;;;;;;;;

(define-key pdf-view-mode-map (kbd "g")
  #'ntd/revert-buffer-dammit)

(define-key pdf-view-mode-map [mouse-2] #'pdf-sync-backward-search-mouse)
(define-key pdf-view-mode-map [mouse-8] #'pdf-history-backward)
(define-key pdf-view-mode-map [mouse-9] #'pdf-history-forward)

(define-key pdf-view-mode-map [C-mouse-5] #'pdf-view-shrink)
(define-key pdf-view-mode-map [C-mouse-4] #'pdf-view-enlarge)

;;;;;;;;;;
;;; WL ;;;
;;;;;;;;;;
(global-set-key [f6] 'wl-folder-check-all)

(defun ntd/bbdb-popup ()
  (local-set-key (kbd "C-c b") #'bbdb-mua-display-all-records))

(add-hook 'mime-view-mode-hook #'ntd/bbdb-popup)
(add-hook 'wl-summary-mode-hook #'ntd/bbdb-popup)

(defun ntd/mail-keys ()
  (local-set-key (kbd "b") #'browse-url-at-point)
  (local-set-key (kbd "j") #'next-line)
  (local-set-key (kbd "k") #'previous-line))

(add-hook 'mime-view-mode-hook #'ntd/mail-keys)

(defun ntd/wl-summary-keys ()
  ;; Folders
  (local-set-key (kbd "C-c f r")
                 (lambda () (interactive) (ntd/wl-goto-petname "recent")))
  (local-set-key (kbd "C-c f s")
                 (lambda () (interactive) (ntd/wl-goto-petname "starred")))
  (local-set-key (kbd "C-c f n")
                 (lambda () (interactive) (ntd/wl-goto-petname "new")))
  ;; Navigation
  ;;(local-set-key (kbd "j") #'wl-summary-jump-to-current-message) bind?
  (local-set-key (kbd "j") #'next-line)
  (local-set-key (kbd "k") #'previous-line))


(add-hook 'wl-summary-mode-hook #'ntd/wl-summary-keys)

(defun ntd/mime-edit-keys ()
  (local-set-key (kbd "M-Q") #'ntd/fill-mail))

(add-hook 'mime-edit-mode-hook  'ntd/mime-edit-keys)
