(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(setq viper-ESC-moves-cursor-back nil
      viper-no-multiple-ESC nil)

(defun ntd/fix-viper-esc ()
  ;; Fix C-[ to do a viper escape
  (when (equal viper-ESC-key [escape])
    (define-key input-decode-map [?\e] [escape])))

;; Maybe need this to work on the first frame
(ntd/fix-viper-esc)

(push (lambda (frame)
        (raise-frame frame)
        (select-frame frame)
        (ntd/fix-viper-esc))
      after-make-frame-functions)
