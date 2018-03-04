;; -*- mode: emacs-lisp -*-;;
;; .emacs
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

(labels ((try-add-dir (dir)
           (when (file-exists-p dir)
             (add-to-list 'load-path dir)))
         (try-add-site (dir)
           (when (file-exists-p dir)
             (add-to-list 'load-path dir)
             (let ((default-directory dir))
               (normal-top-level-add-subdirs-to-load-path)))))
  (try-add-site "~/.emacs.d/site-lisp")
  (try-add-site "/usr/share/emacs/site-lisp/bbdb/")
  (try-add-site "~/share/emacs/site-lisp")
  )

;;;;;;;;;;;
;; LOADS ;;
;;;;;;;;;;;
(labels ((try-load (prefix name)
                   (when (file-exists-p prefix)
                     (load (concat prefix "/" name ".el"))))
         (loadit (name)
                 (or (try-load "~/.emacs.d/rc" name )
                     (try-load "~/git/ntd-env/ntd/.emacs.d/rc" name))))
  (loadit "packages")
  (loadit "viper")
  (loadit "defs")

  (loadit "auctex")
  ;(loadit "bbdb")
  (loadit "c")
  (loadit "cl")
  (loadit "compilation")
  (loadit "erc")
  (loadit "font")
  (loadit "fortran")
  (loadit "greek")
  (loadit "magit")
  (loadit "org")
  (loadit "keys")
  (loadit "pgp")
  (loadit "whitespace")

  (loadit "el-get")
  (loadit "maximarc")

  (try-load "~/.quicklisp" "slime-helper")
  )


;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

;; maybe speeds up wanderlust?
(setq-default bidi-display-reordering nil)

(setq confirm-kill-emacs 'yes-or-no-p)
(desktop-save-mode 1)

(setq make-backup-files nil)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode 1)

(set-scroll-bar-mode nil)
(setq x-select-enable-clipboard t)

(setq column-number-mode t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq vc-handled-backends '(Git SVN))


(setq vc-follow-symlinks t)


(setq ediff-split-window-function 'split-window-horizontally)

;;;;;;;;;;;;;;
;;  PYTHON  ;;
;;;;;;;;;;;;;;
;;push "~/src/elisp/python-mode" load-path)
;;load "~/src/elisp/python-mode/python-mode.el")
;;setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                              interpreter-mode-alist))
;;autoload 'python-mode "python-mode" "Python editing mode." t)
;;add-hook 'python-mode-hook
;;         (lambda () (setq whitespace-style '(spaces space-mark))))

;;add-hook 'python-mode-hook 'whitespace-mode)


;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;;;;;;;;;;;;
;;  js2   ;;
;;;;;;;;;;;;
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jixrc$" . js2-mode))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'js2-mode-hook 'js2-custom-setup)
(defun js2-custom-setup ()
  (moz-minor-mode 1))

;;;;;;;;;;;
;;  m3   ;;
;;;;;;;;;;;

;;push "~/src/elisp/cm3/" load-path)
;;equire 'modula3)

;;utoload 'modula-3-mode "modula3")

;;etq auto-mode-alist
;;    (append '(("\\.ig$" . modula-3-mode)
;;("\\.mg$" . modula-3-mode)
;;("\\.i3$" . modula-3-mode)
;;("\\.m3$" . modula-3-mode))
;;     auto-mode-alist))

;;;;;;;;;;;
;;  XML  ;;
;;;;;;;;;;;
;;autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;;autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '( "xml" "xsd"  "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))
(push '("\\.mxsl$" . nxml-mode) auto-mode-alist )

;;;;;;;;;;;
;;  ADA  ;;
;;;;;;;;;;;
;;set 'ada-prj-default-project-file "~/src/ada/project.adp")

;;;;;;;;;;;;;;
;;  SCHEME  ;;
;;;;;;;;;;;;;;
(when nil
  (autoload 'bdb "bdb" "bdb mode" t)
  (autoload 'bee-mode "bee-mode" "bee mode" t)

  (setq auto-mode-alist
        (append '(("\\.scm$" . bee-mode)
                  ("\\.sc$" . bee-mode)
                  ("\\.sch$" . bee-mode)
                  ("\\.scme$" . bee-mode)
                  ("\\.bgl$" . bee-mode)
                  ("\\.bee$" . bee-mode))
                auto-mode-alist)))

;;;;;;;;;;;;
;;  MAIL  ;;
;;;;;;;;;;;;
(push  '("\\.eml$" . mail-mode) auto-mode-alist )
(push  '("\\.tsj$" . mail-mode) auto-mode-alist )
(push  '("\\.tse$" . mail-mode) auto-mode-alist )
(setq mail-self-blind nil)

;; (push
;;  '("\\.pdf$" "application" "pdf"
;;    nil "base64" "attachment"
;;    (("filename" . file)))



;;;;;;;;;;;;;;;;;;
;;  Wanderlust  ;;
;;;;;;;;;;;;;;;;;;

(defun ntd-email-addr (a b c)
  (concatenate 'string a "@" b "." c))

(setq user-mail-address (ntd-email-addr "ntd" "gatech" "edu"))
(setq wl-user-mail-address-list (list user-mail-address))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(defun my-wl-summary-sort-hook ()
  (wl-summary-rescan "date"))

(add-hook 'wl-summary-prepared-hook 'my-wl-summary-sort-hook)




;; SEE ALSO: ~/.wl

;;;;;;;;;;;;
;;  HTML  ;;
;;;;;;;;;;;;
;;setq load-path (cons "/home/mechsoph/src/elisp" load-path))
;;autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;setq auto-mode-alist (cons '("\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.md$" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;
;; UTF-8 ;;
;;;;;;;;;;;
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)


;;;;;;;;;;;
;; SHELL ;;
;;;;;;;;;;;
(setq explicit-shell-file-name "/bin/zsh" )


;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-add ((((class color) (background dark)) (:foreground "#ccccff"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :foreground "#ff00ff" :slant italic))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "#333333"))))
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "#333")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "evince %o %(outpage)")
     ("^html?$" "." "netscape %o"))))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(imaxima-fnt-size "LARGE" t)
 '(imaxima-pt-size 11)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil)
 '(package-selected-packages
   (quote
    (yaml-mode markdown-mode magit-svn graphviz-dot-mode dockerfile-mode auctex-latexmk)))
 '(safe-local-variable-values
   (quote
    ((Package . CHUNGA)
     (Package . CL-INTERPOL)
     (Package . CL-UNICODE)
     (Package . DRAKMA)
     (Package . FLEXI-STREAMS)
     (c-file-offsets
      (innamespace . 0))
     (Readtable . PY-AST-USER-READTABLE)
     (Package . CLPYTHON)
     (Package . CL-WHO)
     (Package . CL-USER)
     (Package . DOCUMENTATION-TEMPLATE)
     (Base . 10)
     (Package . CL-PPCRE)
     (Syntax . COMMON-LISP)
     (Package . CL-User)
     (Syntax . ANSI-Common-Lisp)
     (Package . FSet)
     (org-export-html-postamble))))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))



;;;;;;;;;;;;;;
;;  BROWSE  ;;
;;;;;;;;;;;;;;
(autoload 'w3m-brows-url "w3m" "" t)
(setq browse-url-browser-function 'w3m-browse-url)

(eval-after-load "w3m"
  '(setq w3m-use-cookies t
         w3m-default-display-inline-images t
         ;; w3m-pop-up-windows t
         ))

;; (setq w3m-session-file "~/.emacs.d/w3m-session")
;; (setq w3m-session-save-always nil)
;; (setq w3m-session-load-always nil)
;; (setq w3m-session-show-titles t)
;; (setq w3m-session-duplicate-tabs 'ask)
;; (setq w3m-language nil)

;; (require 'w3m-session)
;;require 'w3m-cookie)


;; (defun w3m-browse-url-other-window (url &optional newwin)
;;   (let ((w3m-pop-up-windows t))
;;     (if (one-window-p) (split-window))
;;     (other-window 1)
;;     (w3m-browse-url url newwin)))

;; Quirk: localhost may resolve to an ipv6 address,
;; apache may not be supporting ipv6
(autoload 'mediawiki-site "mediawiki" "" t)
(eval-after-load "mediawiki"
  '(setq mediawiki-site-alist
         '(("infosphere"   "http://127.0.0.1:8080/infosphere/"
            "ntd"     "" "Main Page")
           ("Wikipedia" "http://en.wikipedia.org/w/"  "ndantam" "" "Main Page"))))


;; (global-set-key "\C-c\w"
;;                 (lambda (name)
;;                   (interactive "sWiki Page: ")
;;                   (mediawiki-open name)
;;                   (if (one-window-p) (split-window))
;;                   (other-window 1)
;;                   (browse-url
;;                    (concat (mediawiki-site-url mediawiki-site)
;;                            "index.php/"
;;                            (replace-regexp-in-string " " "_"
;;                                                      name)))
;;                   (other-window 1)))

;;;;;;;;;
;; ROS ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.rosinstall$" . yaml-mode))

;;;;;;;;;;;;;;;;
;;  SEMANTIC  ;;
;;;;;;;;;;;;;;;;
;; ;; (require 'ede)
;; ;; (require 'ede-dired)
;; ;; (load "/usr/share/emacs/site-lisp/ede/ede-dired.el")
;; ;; (global-ede-mode 1)

;(require 'cedet)
;(require 'semantic-load)
;(require 'semantic-ia)
;; ;; (require 'semantic-gcc) ; use system include files

(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")
;; (setq semantic-load-turn-useful-things-on nil)
;; ;;(semantic-load-enable-code-helpers)

;; ;; (setq semantic-load-turn-everything-on t)
;; ;; (require 'semantic-gcc) ; use system include files
;; ;; (semantic-load-enable-guady-code-helpers)

;; ;; TODO: make open-paren show summary
;; (add-hook 'c-mode-common-hook
;;          (lambda ()
;;            ;(setq semantic-load-turn-useful-things-on t)
;;            ;;(semantic-load-enable-code-helpers)
;;            (setq semantic-idle-summary t)
;;            (setq semantic-idle-scheduler-idle-time 0.1)
;;            ;; (local-set-key "." 'semantic-complete-self-insert)
;;            ;; (local-set-key ">" 'semantic-complete-self-insert))
;;            (define-key c-mode-base-map (kbd "<C-tab>")
;;                        'semantic-complete-analyze-inline)
;;            ;; TODO: this
;;            (define-key c-mode-base-map "\C-cd"
;;                        'semantic-ia-show-doc)
;;            ;(define-key c-mode-base-map "\C-cc"
;;                        ;'semantic-ia-complete-symbol-menu)
;;            (define-key c-mode-base-map (kbd "<C-return>")
;;                        'semantic-ia-complete-symbol-menu)))



;; ;; (semantic-idle-completions-mode nil)


;; ;; TODO: write function to print types here
;; (setq semantic-idle-summary-function 'semantic-format-tag-prototype)

(setq semanticdb-project-roots
      (list "~/git/thebrain.golems.org/lib/somatic"
            "~/git/thebrain.golems.org/drivers/pcio"
            "~/git/thebrain.golems.org/lib/amino"
            "~/git/thebrain.golems.org/drivers/lwa4"
            "~/git/thebrain.golems.org/lib/ach"))


;;(semantic-add-system-include "/usr/include")

;; (semantic-add-system-include "~/git/thebrain.golems.org/lib/ach/include")
;; (semantic-add-system-include "~/git/thebrain.golems.org/lib/amino/include")
;; (semantic-add-system-include "~/git/thebrain.golems.org/drivers/lwa4/include")

;; ;; TODO: set search throttle properly
;; (setq-mode-local c-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))
;; (setq-mode-local c++-mode semanticdb-find-default-throttle
;;                  '(project unloaded system recursive))

;;;;;;;;;;;;;;;
;; RUN SHELL ;;
;;;;;;;;;;;;;;;
(eshell)
