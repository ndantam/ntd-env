;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
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
(defun ntd/pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta"
                 "epsilon" "zeta" "eta" "theta"
                 "iota" "kappa" "lambda" "mu" "nu"
                 "xi" "omicron" "pi" "rho" "varsigma"
                 "sigma" "tau" "upsilon" "phi" "chi" "psi"
                 "omega")))
    (cl-loop for word in greek
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

(define-minor-mode ntd/pretty-greek-mode
  "Displays greek characters")

(add-hook 'ntd/pretty-greek-mode-hook 'ntd/pretty-greek)

(defun ntd/ucs-code (name)
  (let ((x (ucs-names)))
    (cl-etypecase x
      (hash-table (gethash name x))
      (list (cdr (assoc name (ucs-names)))))))


(defun ntd/def-small-greek (key name)
  (let ((code (ntd/ucs-code (concat "GREEK SMALL LETTER " (upcase name)))))
    (global-set-key (concat "\C-cg" key)
                    (lambda ()
                      (interactive)
                      (insert (make-string 1 code))))))

(defun ntd/def-capital-greek (key name)
  (let ((code (ntd/ucs-code (concat "GREEK CAPITAL LETTER " (upcase name)))))
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
  (cl-loop for (key name) in greeks
        do (ntd/def-small-greek key name)))

(ntd/def-capital-greek "G" "gamma")
(ntd/def-capital-greek "J" "theta")
(ntd/def-capital-greek "L" "lambda")
(ntd/def-capital-greek "X" "xi")
(ntd/def-capital-greek "P" "pi")
(ntd/def-capital-greek "S" "sigma")
(ntd/def-capital-greek "F" "phi")
(ntd/def-capital-greek "Q" "psi")
(ntd/def-capital-greek "W" "omega")


;(add-hook 'lisp-mode-hook 'pretty-greek-mode)
;(add-hook 'f90-mode-hook 'pretty-greek-mode)

;;(add-hook 'emacs-lisp-mode-hook 'pretty-greek-mode)
