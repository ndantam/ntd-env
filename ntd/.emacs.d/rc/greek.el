;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

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
                    (0 (progn
                         (compose-region (match-beginning 2)
                                         (match-end 2)
                                         ,greek-char)
                         nil)))))))))

(define-minor-mode pretty-greek-mode
  "Displays greek characters")

(add-hook 'pretty-greek-mode-hook 'pretty-greek)

(defun ucs-code (name)
  (cdr (assoc name (ucs-names))))


(defun def-small-greek (key name)
  (lexical-let ((code (ucs-code (concat "GREEK SMALL LETTER " (upcase name)))))
    (global-set-key (concat "\C-cg" key)
                    (lambda ()
                      (interactive)
                      (insert (make-string 1 code))))))


(let ((greeks '(("a" "alpha")
                ("b" "beta")
                ("g" "gamma")
                ("d" "delta")
                ("e" "epsilon")
                ("z" "zeta")
                ("h" "eta")
                ("j" "theta")
                ("i" "iota")
                ("k" "kappa")
                ("l" "lambda")
                ("m" "mu")
                ("n" "nu")
                ("x" "xi")
                ("o" "omicron")
                ("p" "pi")
                ("r" "rho")
                ("s" "sigma")
                ("t" "tau")
                ("y" "upsilon")
                ("f" "phi")
                ("c" "chi")
                ("q" "psi")
                ("w" "omega"))))
  (loop for (key name) in greeks
        do (def-small-greek key name)))


;(add-hook 'lisp-mode-hook 'pretty-greek-mode)
;(add-hook 'f90-mode-hook 'pretty-greek-mode)

;;(add-hook 'emacs-lisp-mode-hook 'pretty-greek-mode)
