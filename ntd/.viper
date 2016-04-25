(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

;; (if (equal viper-ESC-key [escape])
;;     (define-key input-decode-map [?\e] [escape]))

(define-key viper-insert-global-user-map [?\e] [escape])
