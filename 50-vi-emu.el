;;;_. viper 
(setq viper-expert-level '3)
(setq viper-inhibit-startup-message 't)

(eval-after-load "viper"
  '(require 'vimpulse))


;;;_. Ex commands without entering viper-mode
;;; from http://www.advogato.org/person/chalst/diary/277.html

(require 'viper-ex)
(require 'viper-keym)
(require 'viper-cmd)

(define-key global-map (kbd "ESC ESC :") 'viper-ex)
