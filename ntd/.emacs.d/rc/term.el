;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.



;;; SSH ;;;

(defvar ntd/term-ssh-last-host "")

(defun ntd/term-kill-sentinel (proc msg)
  (term-sentinel proc msg)
  (let ((buffer (process-buffer proc)))
    (unless (get-buffer-process buffer)
      (kill-buffer buffer))))

(defun ntd/term-kill ()
  (interactive)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'ntd/term-kill-sentinel)
  (kill-process))

(defun ntd/term-setup ()
  (term-line-mode)
  (local-set-key (kbd "C-c <C-backspace>") 'ntd/term-kill))


;; TODO: Prompt to create new if shell/connections already exist and pro

(defun ntd/term-ssh (host)
  (interactive (list (read-from-minibuffer "Remote Host: " ntd/term-ssh-last-host )))
  (let* ((n (concat "ssh-" host))
         (nn (concat "*" n "*")))
    (unless (get-buffer nn)
      (set-buffer (make-term n "/usr/bin/ssh" nil host))
      (ntd/term-setup))
    (switch-to-buffer nn)))

(defun ntd/term-zsh ()
  (interactive)
  (if (get-buffer "*terminal*")
      (switch-to-buffer "*terminal*")
    (progn
      (term "/usr/bin/zsh")
      ;; (rename-uniquely)
      (ntd/term-setup))))
