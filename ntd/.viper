(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(push (lambda (frame)
        (raise-frame frame)
        (select-frame frame)
        (if (equal viper-ESC-key [escape])
            (define-key input-decode-map [?\e] [escape])))
      after-make-frame-functions)
