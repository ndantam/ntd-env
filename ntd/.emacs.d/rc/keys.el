;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
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

(defun ntd/recompile-hook ()
  (local-set-key (kbd "C-c C-c") 'ntd/recompile))

(dolist (x '(autoconf-mode-hook
             automake-mode-hook
             c-mode-hook
             c++-mode-hook
             dired-mode-hook))
  (add-hook x 'ntd/recompile-hook))

;(global-set-key "\C-ctK" 'tramp-compile)

;; commenting
(global-set-key "\C-cX" 'comment-region)
(global-set-key "\C-cU" 'uncomment-region)
;(global-set-key "\C-cL" 'longlines-mode)

(global-set-key "\C-cd" 'eldoc-print-current-symbol-info)

;; expand
(global-set-key "\M-\\" 'hippie-expand)

;; Text
(global-set-key "\C-ca" 'ntd/asciify-region)
(global-set-key "\C-cw" 'ntd/stylize-region)
(global-set-key "\C-cx" 'ntd/texify-region)
(global-set-key (kbd "C-c C-;") #'flyspell-check-previous-highlighted-word)

;;;;;;;;;;;
;; C/C++ ;;
;;;;;;;;;;;

(defun ntd/clang-format-hook ()
  (local-set-key (kbd "C-c M-q") 'ntd/c-format-buffer)
  (local-set-key (kbd "M-q") 'ntd/c-format-region)
  (local-set-key (kbd "TAB") 'ntd/c-tab))

(add-hook 'c-mode-hook 'ntd/clang-format-hook)
(add-hook 'c++-mode-hook 'ntd/clang-format-hook)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(defun ntd/python-format (begin end)
  (interactive)
  (cond
   ;; Try Eglot
   ((bound-and-true-p eglot--managed-mode)
    (eglot-format begin end))
   ;; Try py-autopep8
   ((fboundp 'py-autopep8-region)
    (py-autopep8-region begin end))))

(defun ntd/python-format-region ()
  (interactive)
  (if (region-active-p)
      (ntd/python-format (region-beginning) (region-end))
    (ntd/python-format (line-beginning-position) (line-end-position))))

(defun ntd/python-format-buffer ()
  (interactive)
  (cond
   ((fboundp 'py-autopep8-buffer)
    (py-autopep8-buffer))))

(defun ntd/python-hook ()
  (local-set-key (kbd "C-c M-q") 'ntd/python-format-buffer)
  (local-set-key (kbd "M-q") 'ntd/python-format-region))

(add-hook 'python-mode-hook 'ntd/python-hook)

;;;;;;;;;;
;;  LSP ;;
;;;;;;;;;;

(defun ntd/eglot-format-region ()
  (interactive)
  (eglot-format (region-beginning)
                (region-end)))

(defun ntd/eglot-format-line ()
  (interactive)
  (eglot-format (line-beginning-position)
                 (line-end-position)))

(defun ntd/flymake-keys ()
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "C-c f b") 'flymake-show-buffer-diagnostics))
(with-eval-after-load 'flymake
  (add-hook 'flymake-mode-hook 'ntd/flymake-keys))

(defun ntd/eglot-code-action-and-format (beg &optional end action-kind interactive)
  (interactive
   `(,@(eglot--code-action-bounds)
     ,(and current-prefix-arg
           (completing-read "[eglot] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))
     t))
  (eglot-code-actions beg end action-kind interactive)
  (eglot-format (line-beginning-position)
                (line-end-position)))

(defun ntd/eglot-keys ()
  (local-set-key (kbd "C-c TAB") 'completion-at-point)
  (local-set-key (kbd "M-RET") 'ntd/eglot-code-action-and-format)
   (local-set-key (kbd "C-c C-r") 'eglot-rename)
  ;; (local-set-key (kbd "C-c M-q") 'eglot-format-buffer)
  ;; (local-set-key (kbd "M-q") 'ntd/eglot-format-region)
  )

(with-eval-after-load 'eglot
  (add-hook 'eglot--managed-mode-hook 'ntd/eglot-keys))


;;;;;;;;;;;;;;;;;;;;;;;
;;; version control ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key "\C-xvp" 'vc-update)
;; (global-set-key "\C-xve" 'ediff-revision)
;; (global-set-key "\C-cv=" 'magit-diff-working-tree)
;; (global-set-key "\C-cvl" 'magit-display-log)
;; (global-set-key "\C-cvL"  'magit-log-long)

(global-set-key "\C-cc" 'magit-status)

;;;;;;;;;;;;;;;
;;; Display ;;;
;;;;;;;;;;;;;;;

;; toggle meubar
(global-set-key "\C-cM" (lambda () (interactive)
                          (if menu-bar-mode
                              (menu-bar-mode -1)
                            (menu-bar-mode 1))))
;; Window switching
(global-set-key "\C-co" 'other-window)
(global-set-key "\C-cO" (lambda () (interactive) (other-window -1)))
;(global-set-key "\C-cp" (lambda () (interactive) (other-window -1)))


(global-set-key "\C-ct" #'toggle-truncate-lines)
(global-set-key "\C-cv" #'visual-line-mode)

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

;;;;;;;;;;;;;;;;;;;
;;; Common Lisp ;;;
;;;;;;;;;;;;;;;;;;;

;; Lisp
(with-eval-after-load 'slime
  (global-set-key "\C-cl" 'slime-selector)

  (global-set-key "\C-cLs" 'slime)
  (global-set-key "\C-cLq" 'slime-quit-lisp)

  (global-set-key "\C-cLc" 'local-slime-connect)
  (global-set-key "\C-cLl" 'slime-load-system)
  (global-set-key "\C-cLr" 'slime-reload-system)

  (global-set-key "\C-cLd" 'slime-disconnect))

;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp ;;;
;;;;;;;;;;;;;;;;;;

(defun ntd/elisp-keys ()
  (local-set-key  (kbd "C-c C-c")  'eval-buffer))
(add-hook 'emacs-lisp-mode-hook  'ntd/elisp-keys)

;;;;;;;;;;;;;;;
;;; Linting ;;;
;;;;;;;;;;;;;;;
;; (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
;; (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;;;;;;;;;;;;;;
;;; AuCTeX ;;;
;;;;;;;;;;;;;;

(with-eval-after-load 'latex

  ;; (defun ntd/tex-command-master ()
  ;;   (interactive)
  ;;   ;; (command-execute 'save-buffer)
  ;;   ;;(command-execute 'TeX-save-document)
  ;;   (command-execute 'TeX-command-master))
  ;; (define-key LaTeX-mode-map (kbd "\C-c\C-c") #'ntd/tex-command-master)

  ;; Building
  (defun ntd/tex-mk ()
    (interactive)
    (command-execute 'save-buffer)
    (command-execute 'TeX-save-document)
    (when (let ((b (buffer-name))
                (m (TeX-master-file)))
            (or (string-match ".*\\.tex" b)
                ;(not (string= m b))
                (not (string= m (replace-regexp-in-string "\\.[^\\.]*$" "" b)))))
      ;; TODO: TeX-process-check asks if we want to kill latexmk.  It
      ;; should have a flag to always kill.
      ;;(TeX-command  "LatexMk" #'TeX-master-file nil)
      ;;(TeX-command  "LaTeX" #'TeX-master-file nil)
      (TeX-command-run-all nil)
      ))

  ;; (define-key tex-mode-map (kbd "\C-x\C-s") #'ntd/tex-mk)
  (define-key LaTeX-mode-map (kbd "\C-x\C-s") #'ntd/tex-mk)
  (define-key LaTeX-mode-map (kbd "\C-c\C-s") #'save-buffer)

  ;; Viewing
  (defun ntd/tex-view ()
    (interactive)
    ;;(TeX-command "View" #'TeX-master-file)
    (TeX-pdf-tools-sync-view))

  (define-key LaTeX-mode-map (kbd "\C-c\C-v") #'ntd/tex-view)
  ;;(define-key LaTeX-mode-map (kbd "\C-cv") #'ntd/tex-view)
  ;;(define-key LaTeX-mode-map (kbd "\C-cf") #'pdf-sync-forward-search)
  (define-key LaTeX-mode-map (kbd "\C-cf") #'ntd/tex-view)
)

;;;;;;;;;;;
;;; PDF ;;;
;;;;;;;;;;;

(defun ntd/revert-pdf ()
  (interactive)
  ;; TODO: rebuild the PDF if it's from latex
  ;; (when (fboundp 'TeX-master-file)
  ;;   (let* ((pdf (TeX-master-file))
  ;;          (synctex (pdf-sync-locate-synctex-file pdf)))
  ;;     (print synctex)))
  ;; Reload
  (ntd/revert-buffer-dammit))

;; (define-key pdf-view-mode-map (kbd "j") #'pdf-view-scroll-up-or-next-page)
;; (define-key pdf-view-mode-map (kbd "k") #'pdf-view-scroll-down-or-previous-page)

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "g")
    #'ntd/revert-pdf)

  ;; scrolling
  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)

  (define-key pdf-view-mode-map (kbd "l") #'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "h") #'image-backward-hscroll)

  (define-key pdf-view-mode-map [(shift mouse-4)] #'image-backward-hscroll)
  (define-key pdf-view-mode-map [(shift mouse-5)] #'image-forward-hscroll)

  ;; sync
  (define-key pdf-view-mode-map [mouse-2] #'pdf-sync-backward-search-mouse)

  ;; zoom
  (define-key pdf-view-mode-map [C-mouse-5] #'pdf-view-shrink)
  (define-key pdf-view-mode-map [C-mouse-4] #'pdf-view-enlarge))

(with-eval-after-load 'pdf-history
  ;; use l to scroll
  (define-key pdf-history-minor-mode-map (kbd "l") nil)

  (define-key pdf-history-minor-mode-map [mouse-8] #'pdf-history-backward)
  (define-key pdf-history-minor-mode-map [mouse-9] #'pdf-history-forward)

  (define-key pdf-history-minor-mode-map [backspace] #'pdf-history-backward)
  (define-key pdf-history-minor-mode-map [(shift backspace)] #'pdf-history-forward)

  (define-key pdf-history-minor-mode-map (kbd "M-<left>")  #'pdf-history-backward)
  (define-key pdf-history-minor-mode-map (kbd "M-<right>") #'pdf-history-forward))


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
