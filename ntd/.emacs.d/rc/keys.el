;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(defun ntd/woman ()
  (interactive)
  (if (one-window-p) (split-window))
  (let ((buf (current-buffer)))
    (other-window 1)
    (switch-to-buffer buf))
  (woman))

(defmacro ntd/with-region (begin-end &rest body)
  (cl-destructuring-bind (begin end) begin-end
    `(let (,begin ,end)
       ;; Bind
       (if (region-active-p)
           (setq ,begin (region-beginning)
                 ,end (region-end))
         (setq ,begin (line-beginning-position)
               ,end (line-end-position)))
       ;; Body
       ,@body)))

(defun ntd/comment ()
  (interactive)
  (ntd/with-region (b e)
                   (comment-region b e)))

(defun ntd/copy-comment ()
  (interactive)
  (ntd/with-region (b e)
                   (kill-ring-save b e)
                   (comment-region b e)))

(defun ntd/uncomment ()
  (interactive)
  (ntd/with-region (b e)
                   (uncomment-region b e)))

(defun ntd/toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
;;;;;;;;;;;;;;;;;;;
;; Key Semantics ;;
;;;;;;;;;;;;;;;;;;;

(defmacro ntd/prefix-map (name key)
  `(progn
     (setq ,name (make-keymap))
     (define-key global-map (kbd ,key) ,name)))

;;; User keys
;; a
(global-unset-key (kbd "C-c a"))
;; b
(global-set-key (kbd "C-c b") #'browse-url-at-point)
;; c
(global-set-key "\C-cc" 'magit-status)
;; d
(global-set-key (kbd "C-c d") 'eldoc-print-current-symbol-info)
;; e
(ntd/prefix-map ntd/emacs-prefix-map "C-c e")
;; f
(global-unset-key (kbd "C-c f"))
;; g
(global-unset-key (kbd "C-c g"))
;; h
(global-unset-key (kbd "C-c h"))
;; i
(global-set-key (kbd "\C-c i") 'other-frame)
(global-set-key (kbd "\C-c I") (lambda () (interactive) (other-frame -1)))
;; j
(global-unset-key (kbd "C-c j"))
;; k
(global-unset-key (kbd "C-c k"))
(global-set-key (kbd "C-c k") 'ntd/compile)
;; l
(ntd/prefix-map ntd/lisp-prefix-map "C-c l")
;; m
(global-set-key (kbd "C-c m") 'ntd/woman)
;; n
(global-unset-key (kbd "C-c n"))
;; o
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-c O") (lambda () (interactive) (other-window -1)))
;; p
(global-unset-key (kbd "C-c p"))
;; q
(global-unset-key (kbd "C-c q"))
;; r
(global-set-key (kbd "C-c r") #'ntd/term-ssh)
;; s
(global-set-key (kbd "C-c s") #'ntd/term-zsh)
;; t
(ntd/prefix-map ntd/text-prefix-map "C-c t")
;; u
(global-set-key (kbd "C-c u") 'ntd/uncomment)
(global-set-key (kbd "C-c U") 'uncomment-region)
;; v
(global-unset-key (kbd "C-c v"))
;; w
(global-unset-key (kbd "C-c w"))
;; x
(global-set-key (kbd "C-c x") 'ntd/comment)
(global-set-key (kbd "C-c X") 'ntd/copy-comment)
;; y
(global-unset-key (kbd "C-c y"))
;; z
(global-unset-key (kbd "C-c z"))

;; f5
(global-set-key [f5] 'revert-buffer)
;; f6
(global-unset-key [f6])
;; f7
(global-unset-key [f7])
;; f8
(global-unset-key [f8])
;; f9
(global-unset-key [f9])
;; f10
(global-unset-key [f10])
;; f11
(global-set-key [f11] 'ntd/toggle-fullscreen)
;; f12
(global-unset-key [f12])

;; Other keys

(setq ntd/key-format-region (kbd "M-q")
      ntd/key-format-buffer (kbd "C-c M-q")
      ntd/key-do-buffer (kbd "C-c C-c")
      ntd/key-complete (kbd "C-c TAB")
      )

;;;;;;;;;;;;;;;;;;;
;;  GLOBAL KEYS  ;;
;;;;;;;;;;;;;;;;;;;

(defun ntd/compile-hook ()
  (local-set-key ntd/key-do-buffer 'ntd/compile))

(dolist (x '(autoconf-mode-hook
             automake-mode-hook
             c-mode-hook
             c++-mode-hook
             dired-mode-hook))
  (add-hook x 'ntd/compile-hook))

;(global-set-key "\C-ctK" 'tramp-compile)

;; expand
(global-set-key "\M-\\" 'hippie-expand)

;; Fill
(define-key global-map "\C-\M-Q" 'unfill-paragraph)

;;;;;;;;;;
;; Text ;;
;;;;;;;;;;

(define-key ntd/text-prefix-map (kbd "a") 'ntd/asciify-region)
(define-key ntd/text-prefix-map (kbd "s") 'ntd/stylize-region)
(define-key ntd/text-prefix-map (kbd "t") 'ntd/texify-region)
(define-key ntd/text-prefix-map (kbd "x")  #'toggle-truncate-lines)
(define-key ntd/text-prefix-map (kbd "v")  #'visual-line-mode)

(global-set-key (kbd "C-c C-;") #'flyspell-check-previous-highlighted-word)

;;;;;;;;;;;
;; C/C++ ;;
;;;;;;;;;;;

(defun ntd/clang-format-hook ()
  (local-set-key ntd/key-format-buffer 'ntd/c-format-buffer)
  (local-set-key ntd/key-format-region 'ntd/c-format-region)
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
  (local-set-key ntd/key-format-buffer 'ntd/python-format-buffer)
  (local-set-key ntd/key-format-region 'ntd/python-format-region))

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

(defun ntd/eglot-code-action-and-format ()
  (interactive)
  (command-execute 'eglot-code-actions)
  (eglot-format (line-beginning-position)
                (line-end-position)))

(defun ntd/eglot-next-action ()
  (interactive)
  (flymake-goto-next-error)
  (ntd/eglot-code-action-and-format))

(defun ntd/eglot-keys ()
  (local-set-key (kbd "C-c TAB") 'completion-at-point)
  (local-set-key (kbd "M-RET") 'ntd/eglot-code-action-and-format)
  (local-set-key (kbd "C-c C-r") 'eglot-rename)
  (local-set-key (kbd "M-N") 'ntd/eglot-next-action)
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


;;;;;;;;;;;;;;;
;;; Display ;;;
;;;;;;;;;;;;;;;

;; Tab Switching
;(global-set-key "\C-cu" 'tab-next)
;(global-set-key "\C-cU" (lambda () (interactive) (tab-next -1)))

;; Frame Switching

;; Encryption
;; (global-set-key "\C-ces" 'pgg-encrypt-symmetric-region)
;; (global-set-key "\C-ceS" 'pgg-encrypt-symmetric)
;; (global-set-key "\C-ced" 'pgg-decrypt-region)
;; (global-set-key "\C-ceD" 'pgg-decrypt)
;; (global-set-key "\C-cea" 'pgg-encrypt-region)
;; (global-set-key "\C-ceA" 'pgg-encrypt)

;;;;;;;;;;;;;
;;; EMACS ;;;
;;;;;;;;;;;;;

(define-key ntd/emacs-prefix-map (kbd "f") 'ntd/toggle-fullscreen)
(define-key ntd/emacs-prefix-map (kbd "i") 'revert-buffer)
(define-key ntd/emacs-prefix-map (kbd "i") 'ielm)
(define-key ntd/emacs-prefix-map (kbd "m") 'menu-bar-mode)
(define-key ntd/emacs-prefix-map (kbd "t") 'tab-bar-mode) ; tabbar vs tab-bar
(define-key ntd/emacs-prefix-map (kbd "t") 'tab-bar-mode) ; tabbar vs tab-bar
(define-key ntd/emacs-prefix-map (kbd "h") 'windmove-left)
(define-key ntd/emacs-prefix-map (kbd "l") 'windmove-right)
(define-key ntd/emacs-prefix-map (kbd "k") 'windmove-up)
(define-key ntd/emacs-prefix-map (kbd "j") 'windmove-down)
;;(global-set-key "\C-ch"
;;(global-set-key "\C-cl" 'windmove-right)
;;(global-set-key "\C-ck" 'windmove-up)
;;(global-set-key "\c-cj" 'windmove-down)


;;;;;;;;;;;;;;;;;;;
;;; Common Lisp ;;;
;;;;;;;;;;;;;;;;;;;

;; Lisp
(with-eval-after-load 'slime
  (define-key ntd/lisp-prefix-map (kbd "l") 'slime-selector)
  (define-key ntd/lisp-prefix-map (kbd "s") 'slime-scratch)
  (define-key ntd/lisp-prefix-map (kbd "r") 'slime-repl)
  (define-key ntd/lisp-prefix-map (kbd "RET") 'slime-repl)
  (define-key ntd/lisp-prefix-map (kbd "DEL") 'slime-quit-lisp)
  (define-key ntd/lisp-prefix-map (kbd "c") 'slime-connect)
  (define-key ntd/lisp-prefix-map (kbd" SPC") 'slime-load-system)
  (define-key ntd/lisp-prefix-map (kbd "C-SPC") 'slime-reload-system)
  (define-key ntd/lisp-prefix-map (kbd "<delete>") 'slime-disconnect))

;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp ;;;
;;;;;;;;;;;;;;;;;;

(defun ntd/elisp-keys ()
  (local-set-key  ntd/key-do-buffer 'eval-buffer))
(add-hook 'emacs-lisp-mode-hook  'ntd/elisp-keys)

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

  ;; (define-key tex-mode-map (kbd "C-x C-s") #'ntd/tex-mk)
  (define-key LaTeX-mode-map (kbd "C-x C-s") #'ntd/tex-mk)
  (define-key LaTeX-mode-map (kbd "C-c C-s") #'save-buffer)

  ;; Viewing
  (defun ntd/tex-view ()
    (interactive)
    ;;(TeX-command "View" #'TeX-master-file)
    (TeX-pdf-tools-sync-view))

  (define-key LaTeX-mode-map (kbd "C-c C-v") #'ntd/tex-view)
  ;;(define-key LaTeX-mode-map (kbd "C-c v") #'ntd/tex-view)
  ;;(define-key LaTeX-mode-map (kbd "C-c f") #'pdf-sync-forward-search)
  (define-key LaTeX-mode-map (kbd "C-c f") #'ntd/tex-view)
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

;;;;;;;;;;;;;
;;; Viper ;;;
;;;;;;;;;;;;;

;; clobbers viper-scroll-down
(define-key viper-vi-global-user-map (kbd "C-u") 'universal-argument)
;; bind scrolling RET / C-ret
(define-key viper-vi-global-user-map (kbd "RET") 'viper-scroll-up)
(define-key viper-vi-global-user-map (kbd "SPC") 'viper-scroll-up)
(define-key viper-vi-global-user-map (kbd "C-<return>") 'viper-scroll-down)

;; Preserve the help map
(define-key viper-vi-global-user-map (kbd "C-h") help-map)
(define-key viper-insert-global-user-map (kbd "C-h") help-map)

;; VI Unused Keys
;; --------------
;; g
(define-key viper-vi-global-user-map (kbd "g") ntd/emacs-prefix-map)
;; q
;; _
;; K
;; C-RET
;; C-!
;; <insert>

;; VI duped keys
;; -------------
;;
;; SPC (l)
;; (define-key viper-vi-global-user-map (kbd "SPC") 'magit-status)
;; U (u)
;; DEL,<backspage> (h)
;; RET,<+>
