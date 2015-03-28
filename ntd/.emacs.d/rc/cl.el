;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;; CL  ;;
;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asdf-install$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))


(global-set-key "\C-cll" 'slime-load-system)
(global-set-key "\C-clr" 'slime-reload-system)

;;;;;;;;;;;;
;; SLIME  ;;
;;;;;;;;;;;;

(let ((file (expand-file-name "~/.quicklisp/slime-helper.el")))
  (when (file-exists-p file)
    (load file)))


(autoload 'slime "slime" "slime" t)
(autoload 'slime-connect "slime" "slime" t)

;;(load-library "slime")
;;(load-library "slime-autodoc")
;;(setq slime-use-autodoc-mode t)

(eval-after-load "slime"
  '(progn
     (require 'slime-fancy)
     (require 'slime-autodoc)
     (setq slime-net-coding-system 'utf-8-unix)
     (setq slime-use-autodoc-mode t)
     (slime-setup '(slime-fancy slime-asdf))
     (global-set-key "\C-cs" 'slime-selector)

     (let ((path (concatenate 'string
                              temporary-file-directory (user-login-name) "-cache/slime/")))
       (make-directory path t)
       (setq slime-compile-file-options `(:fasl-directory ,path)))))

(when-host ("daneel" "leela")
  (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/"))

(setq slime-lisp-implementations
      `((sbcl ,(cond
                ((host-match "apollo")
                 '("sbcl" "--dynamic-space-size" "8GB"))
                (t '("sbcl"))))
        (clisp ("/usr/bin/clisp"))
        (ccl ("ccl"))
        (ecl ("/usr/bin/ecl"))))

(setq slime-default-lisp 'sbcl)
