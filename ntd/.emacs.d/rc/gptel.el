;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

(defvar ntd/gptel-backend-gh)
(setq ntd/gptel-backend-gh
        (gptel-make-gh-copilot "Copilot" :stream t))

(defvar ntd/gptel-backend-gemini-personal)
(setq ntd/gptel-backend-gemini-personal
      (gptel-make-gemini "Gemini"
        :key (with-temp-buffer
               (insert-file-contents "~/doc/private/gemini-token-personal")
               (string-trim (buffer-string)))
        :stream t))
(cl-pushnew 'gemini-3.8-live-extended-thinking
            (gptel-backend-models ntd/gptel-backend-gemini-personal))

;; (setq gptel-backend
;;         (gptel-make-gh-copilot "Copilot" :stream t))

;; Copilot
;; https://docs.github.com/en/copilot/reference/copilot-billing/models-and-pricing
;; gpt-4.1
;; gpt-4o
;; gpt-5-mini
;; gpt-5.3-codex
;; gpt-5.4
;; gpt-5.4-mini
;; gpt-5.5
;; gpt-5.6-sol
;; gpt-5.6-terra
;; gpt-5.6-luna
;; claude-haiku-4.5
;; claude-opus-4.5
;; claude-opus-4.6
;; claude-opus-4.7
;; claude-opus-4.8
;; claude-opus-5
;; claude-fable-5
;; claude-sonnet-4.5
;; claude-sonnet-4.6
;; claude-sonnet-5
;; gemini-3.1-pro-preview
;; gemini-3.5-flash
;; gemini-3.6-flash
;; gemini-3.7-flash
;; gemini-3.8-flash

(setq gptel-default-mode 'org-mode      ; Default chat buffer format
      gptel-use-curl t)                 ; Ensure curl is preferred

(defun ntd/gptel-select (backend model name)
  (let ((gptel-backend backend)
        (gptel-model model))
    (let ((buffer (if name
                      (gptel name)
                    (call-interactively 'gptel))))
      (set-buffer buffer)
      (setq-local gptel-backend backend
                  gptel-model model)
      buffer)))

(defun ntd/gptel-gh (&optional name)
  (interactive)
  (ntd/gptel-select ntd/gptel-backend-gh
                    'gpt-5.6-luna
                    name))

(defun ntd/gptel-gemini (&optional name)
  (interactive)
  (ntd/gptel-select ntd/gptel-backend-gemini-personal
                    'gemini-flash-latest
                    name))
