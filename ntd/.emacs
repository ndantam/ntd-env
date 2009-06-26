;; -*- Emacs-Lisp -*-

;;|=============================|;;
;;| FILE:    .emacs             |;;
;;| AUTHOR:  Neil T. Dantam     |;;
;;| Emacs Initialization File   |;;
;;|=============================|;;


(setq gentoo nil      ; running on gentoo
      do-slime t
      do-viper t
      do-bigloo-bee nil
      )


(add-to-list 'load-path "~/.emacs.d")

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
   (t (all (cdr args)))))


(defmacro when-host (name &rest forms)
  (declare (indent 1))
  `(when ,(if (atom name)
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

;; Semantic projects

;;when-host "daneel"
;; (setq semanticdb-project-roots
;;       (list "~/cc/sparky/src")))


;;semantic-load-enable-code-helpers)

;;setq semantic-load-turn-useful-things-on t)


;;;;;;;;;;;;;;;;;;;
;;  GLOBAL KEYS  ;;
;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\k" 'compile)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\M-c" 'uncomment-region)
(global-set-key "\C-c#" 'server-start)
(global-set-key "\C-cbe" (lambda () (interactive)
                           (switch-to-buffer "*eshell*")))

;;;;;;;;;;;;;;
;;  GENTOO  ;;
;;;;;;;;;;;;;;
;;load "/usr/share/emacs/site-lisp/site-gentoo.el")
(if gentoo
    (require 'site-gentoo))

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

(setq make-backup-files nil)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode 1)

(set-scroll-bar-mode nil)



;;;;;;;;;;;;;;;;;;;
;;  Remote File  ;;
;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
;;add-to-list 'tramp-remote-path "~/bin")
;;pushnew "/opt/csw/bin" tramp-remote-path) ;niagara path to cvs

;;;;;;;;;;;;
;;  FONT  ;;
;;;;;;;;;;;;

(set-background-color "black")
(add-to-list 'default-frame-alist '(background-color . "black"))

(set-foreground-color "green")
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

;;;;;;;;;;;;;;
;;  PYTHON  ;;
;;;;;;;;;;;;;;
;;push "~/src/elisp/python-mode" load-path)
;;load "~/src/elisp/python-mode/python-mode.el")
;;setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                              interpreter-mode-alist))
;;autoload 'python-mode "python-mode" "Python editing mode." t)




;;;;;;;;;;;;;
;;  VIPER  ;;
;;;;;;;;;;;;;
(when do-viper
  (setq viper-mode t)
  (setq viper-always t)
  (require 'viper))

;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;
;; AUCTeX  ;;
;;;;;;;;;;;;;
;;load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


;;;;;;;;;;;;
;; SLIME  ;;
;;;;;;;;;;;;
(when do-slime
  (eval-after-load "slime"
    '(progn
       (setq inferior-lisp-program "/usr/bin/sbcl")
       (setq browse-url-browser-function 'w3m-browse-url)
       (slime-setup))))


(when-host ("daneel" "hesh" "olivaw")
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

;;push "~/src/elisp/js2/" load-path)
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
(when do-bigloo-bee
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
(setq-default tab-width 4)



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
(setq explicit-shell-file-name "/bin/bash" )



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
 '(case-fold-search t)
 '(current-language-environment "English")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-mirror-mode nil)
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t))


;;;;;;;;;;;;;;;;
;; RUN ESHELL ;;
;;;;;;;;;;;;;;;;
(eshell)
