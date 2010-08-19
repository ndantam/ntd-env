;; .emacs
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/imaxima")



(require 'cl)

;;require 'w3m-load)

(add-to-list 'vc-handled-backends 'Git)

;;;;;;;;;;;;
;;  DEFS  ;;
;;;;;;;;;;;;

(defun all (args)
  (cond
   ((null args) t)
   ((car args) (all (cdr args)))
   (t nil)))

(defun any (args)
  (cond
   ((null args) nil)
   ((car args) t)
   (t (any (cdr args)))))


(defmacro when-host (name &rest forms)
  (declare (indent 1))
  `(when ,(if (atom name)
              `(string= (system-name) ,name)
            `(any (mapcar (lambda (name)
                            (string= (system-name) name))
                          (quote ,name))))
     ,@forms))


(defmacro unless-host (name &rest forms)
  (declare (indent 1))
  `(unless ,(if (atom name)
                `(string= (system-name) ,name)
              `(any (mapcar (lambda (name)
                              (string= (system-name) name))
                            (quote ,name))))
     ,@forms))


;;;;;;;;;;;;;;;;
;;  SEMANTIC  ;;
;;;;;;;;;;;;;;;;
;;setq semantic-load-turn-everything-on t)
;;require 'semantic-load)
;;require 'semantic-ia)
;;add-hook 'c-mode-common-hook
;;         (lambda ()
;;           (define-key c-mode-base-map (kbd "\C-c TAB")
;;                       'semantic-complete-analyze-inline)
;;           (define-key c-mode-base-map (kbd "\C-c m")
;;                       'semantic-ia-complete-symbol-menu)))


;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (local-set-key "\C-c\C-c"
;;                            (execute-command #'save-buffer)
;;                            (execute-command #'recompile))))

;; Semantic project;; s

;;when-host "daneel"
;; (setq semanticdb-project-roots
;;       (list "~/cc/sparky/src")))



;;semantic-load-enable-code-helpers)

;;setq semantic-load-turn-useful-things-on t)


;;;;;;;;;;;;;;;;;;;
;;  GLOBAL KEYS  ;;
;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\k" (lambda ()
                           (interactive)
                           (command-execute 'save-buffer)
                           (command-execute 'recompile)))

(global-set-key "\C-c\l" 'compile)
(global-set-key "\C-ctk" 'tramp-compile)

(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-c#" 'server-start)
(global-set-key "\C-cbe" (lambda () (interactive)
                           (switch-to-buffer "*eshell*")))
(global-set-key "\C-xvp" 'vc-update)


(global-set-key "\M-\\" 'hippie-expand)

;;;;;;;;;;;;;;
;;  GENTOO  ;;
;;;;;;;;;;;;;;
;;load "/usr/share/emacs/site-lisp/site-gentoo.el")
(when nil
  (require 'site-gentoo))

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

(setq make-backup-files nil)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode 1)

(set-scroll-bar-mode nil)
(setq x-select-enable-clipboard t)


;;;;;;;;;;;;;;;;;;;
;;  Remote File  ;;
;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
;;require 'tramp-util)

;;add-to-list 'tramp-remote-path "~/bin")
;;pushnew "/opt/csw/bin" tramp-remote-path) ;niagara path to cvs

;;;;;;;;;;;;
;;  FONT  ;;
;;;;;;;;;;;;

(set-background-color "black")
(add-to-list 'default-frame-alist '(background-color . "black"))

(set-foreground-color "green")
(add-to-list 'default-frame-alist '(foreground-color . "green"))



(add-to-list 'default-frame-alist '(foreground-color . "green"))

(if (eq window-system 'x)
    (progn
      (set-face-background 'region                   "#555555")
      (set-face-foreground 'modeline             "white")
      (set-face-background 'modeline             "#333333"))
  (progn
    (set-face-background 'region                   "gray")
    (set-face-foreground 'region                   "black")))


(mouse-wheel-mode t)
(add-to-list 'default-frame-alist '(background-mode . dark))
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)


;;;;;;;;;
;;  C  ;;
;;;;;;;;;

;; Other people are annoyed by emacs 2-space default
(setq c-basic-offset 4) ; I've written to much java,
                                        ; but then so have many other people...
(setq c-default-style "bsd")

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



;;;;;;;;;;;;;
;;  VIPER  ;;
;;;;;;;;;;;;;
(setq viper-mode t)
(setq viper-always t)
(setq viper-vi-state-cursor-color "green")
(require 'viper)

;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;;TeX-PDF-mode)

;;;;;;;;;;;;;
;; AUCTeX  ;;
;;;;;;;;;;;;;
;;load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setenv "TEXINPUTS" ":/home/ntd/src/ntd-latex:")

;; make "C-c C-c" save buffer first
(add-hook 'LaTeX-mode-hook
          '(lambda()
             (local-set-key "\C-c\C-c"
                            (lambda ()
                              (interactive)
                              (command-execute 'save-buffer)
                              (command-execute 'TeX-command-master)
                              ))))





;;;;;;;;;;;;
;; SLIME  ;;
;;;;;;;;;;;;

(when-host ("daneel" "hesh" "leela" "IRBT-2914")
           (setq inhibit-splash-screen t)
           (pushnew "~/src/clbuild/source/slime/" load-path)
           (pushnew "~/src/clbuild/source/slime/contrib/" load-path)
           (setq slime-backend "~/src/clbuild/source/slime/swank-loader.lisp")
           (load "~/src/clbuild/source/slime/slime")
           (setq inferior-lisp-program "/usr/local/bin/sbcl")
           (slime-require :swank-listener-hooks))

(unless-host ("daneel" "hesh" "leela" "IRBT-2914")
             (eval-after-load "slime"
               '(progn
                  (setq inferior-lisp-program "sbcl --dynamic-space-size 512")
                  (require 'slime)
                  (require 'slime-tramp)
                  (require 'slime-autoloads))))

(slime-setup '(slime-fancy slime-asdf))
(global-set-key "\C-cs" 'slime-selector)


(setq browse-url-browser-function 'w3m-browse-url)

;;(setq slime-use-autodoc-mode nil)

;; ;(require 'slime-autoloads)
;; ;require 'slime-tramp)
;; (slime-setup '(slime-fancy))

;; ;(push (slime-create-filename-translator :machine-instance "daneel"
;;                                         ;:remote-host "daneel"
;;                                         ;:username "ntd")
;;       ;slime-filename-translations)
;; ;(setq slime-filename-translations nil)

;; (when-host ("daneel" "hesh" "olivaw" "babel")
;;   (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/"))

;; (setq slime-lisp-implementations
;;       '((sbcl ("/usr/bin/sbcl"))
;;         (clisp ("/usr/bin/clisp"))
;;         (ecl ("/usr/bin/ecl"))))


(require 'slime)
(require 'slime-autoloads)
(require 'slime-tramp)

;;push (slime-create-filename-translator :machine-instance "daneel"
                                        ;:remote-host "daneel"
                                        ;:username "ntd")
                                        ;slime-filename-translations
;;

;;push (slime-create-filename-translator :machine-instance "daneel"
                                        ;:remote-host "daneel"
                                        ;:username "ntd")
                                        ;slime-filename-translations)
;;(setq slime-filename-translations nil)

(when-host ("daneel" "hesh" "olivaw" "babel")
           (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/"))



;;;;;;;;;;;;
;; CL  ;;
;;;;;;;;;;;;
(push '("\\.sbclrc$" . lisp-mode)  auto-mode-alist )
(push '("\\.asdf-install$" . lisp-mode) auto-mode-alist )
(push '("\\.asd$" . lisp-mode) auto-mode-alist )


;;;;;;;;;;;;
;;  js2   ;;
;;;;;;;;;;;;

;;autoload 'js2-mode "js2" nil t)

;;add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;add-to-list 'auto-mode-alist '("\\.jixrc$" . js2-mode))



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
(setq mail-self-blind t)

;;;;;;;;;;;;
;;  HTML  ;;
;;;;;;;;;;;;
;;setq load-path (cons "/home/mechsoph/src/elisp" load-path))
;;autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;setq auto-mode-alist (cons '("\.html$" . html-helper-mode) auto-mode-alist))

;;;;;;;;;;;;
;;  TABS  ;;
;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)



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



;;(load "doctor")
;;load "tnt")
;;load "net-doctor")


;;;;;;;;;;;;;
;; FORTRAN ;;
;;;;;;;;;;;;;
(defun f90-return( ) (interactive) (f90-indent-line) (newline-and-indent))

(add-hook 'f90-mode-hook
          '(lambda()
             (local-set-key [13] 'f90-return)    ; RET with automatic indent
             (imenu-add-to-menubar "Program-Units") ; Add index of func. to menu bar
             ))

;;;;;;;;;;;;;
;; OCTAVE  ;;
;;;;;;;;;;;;;
;;;
;;autoload 'octave-mode "octave-mod" nil t)
(autoload 'run-octave "octave-inf" nil t)
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))
                                        ;(turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))

;;add-hook 'octave-mode-hook
                                        ;(lambda ()
                                        ;(run-octave)))

(add-hook 'octave-mode-hook
          'viper-mode)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;define-key octave-mode-map "\C-m"
                                        ;'octave-reindent-then-newline-and-indent)

(setq inferior-octave-startup-args `("-q"))

;;;;;;;;;;;;;;;;;;;
;; EMACS SERVER  ;;
;;;;;;;;;;;;;;;;;;;
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;;;;;;;;;;;;;
;; MAXIMA  ;;
;;;;;;;;;;;;;

(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
(setq imaxima-use-maxima-mode-flag nil)
(setq imaxima-fnt-size "Large")
(require 'maxima)

;;setq load-path (cons  "/usr/share/maxima/5.9.1/emacs" load-path ))

;;autoload 'maxima "maxima" "Running Maxima interactively" t)

;;autoload 'maxima-mode "maxima" "Maxima editing mode" t)


;;setq load-path (cons "/usr/share/maxima/5.9.0/emacs" load-path))
;;autoload 'maxima-mode "maxima" "Maxima mode" t)
;;autoload 'maxima "maxima" "Maxima interactive" t)
;;setq auto-mode-alist (cons '("\.max" . maxima-mode) auto-mode-alist))
;;autoload 'emaxima-mode "emaxima" "EMaxima" t)
;;add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)


;;;;;;;;;;;;
;; CUSTOM ;;
;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "#333")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "evince %o %(outpage)") ("^html?$" "." "netscape %o"))))
 '(case-fold-search t)
 '(current-language-environment "English")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(imaxima-fnt-size "LARGE")
 '(imaxima-pt-size 11)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil)
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display greek characters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Based on pretty-greek by of BenignoUria
;; ΣΤΥΦΧΨΩ
(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta"
                 "epsilon" "zeta" "eta" "theta"
                 "iota" "kappa" "lambda" "mu" "nu"
                 "xi" "omicron" "pi" "rho" "varsigma"
                 "sigma" "tau" "upsilon" "phi" "chi" "psi"
                 "omega")))
    (loop for word in greek
                                        ;for code = 97 then (+ 1 code)
          for greek-char across "αβγδεζηθικλμνξοπρςστυφχψω"
          do  (progn
                (font-lock-add-keywords
                 nil
                 `((,(concatenate 'string
                                  "\\(^\\|[^a-zA-Z0-9]\\)\\("
                                  word "\\)[a-zA-Z]")
                    (0 (progn (decompose-region (match-beginning 2)
                                                (match-end 2))
                              nil)))))
                (font-lock-add-keywords
                 nil
                 `((,(concatenate 'string
                                  "\\(^\\|[^a-zA-Z0-9]\\)\\("
                                  word "\\)[^a-zA-Z]")
                    (0 (progn (princ (buffer-substring (match-beginning 2)
                                                       (match-end 2)))
                              (compose-region (match-beginning 2)
                                              (match-end 2)
                                              ,greek-char)
                              nil)))))))))

(define-minor-mode pretty-greek-mode
  "Displays greek characters")

(add-hook 'pretty-greek-mode-hook 'pretty-greek)


(add-hook 'lisp-mode-hook 'pretty-greek-mode)
(add-hook 'f90-mode-hook 'pretty-greek-mode)

;;(add-hook 'emacs-lisp-mode-hook 'pretty-greek-mode)


;; erc

(require 'erc)



(defun irobot-erc ()
  (interactive)
  (erc-tls :server  "leprosy.wardrobe.irobot.com" :port 6667
           :nick "ndantam" :full-name "Neil Dantam")
  (erc-join-channel "#research"))

(setq erc-autojoin-channels-alist '(("leprosy.wardrobe.irobot.com"
                                     "#research")))

;;;;;;;;;;;;;;;;
;; RUN ESHELL ;;
;;;;;;;;;;;;;;;;
(eshell)


