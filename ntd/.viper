(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(defun ntd/fix-viper-esc ()
  (if (equal viper-ESC-key [escape])
      (define-key input-decode-map [?\e] [escape])))

;; Maybe need this to work on the first frame
(ntd/fix-viper-esc)

(push (lambda (frame)
        (raise-frame frame)
        (select-frame frame)
        (ntd/fix-viper-esc))
      after-make-frame-functions)
