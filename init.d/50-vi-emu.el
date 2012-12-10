(setq q "This iw not Vi. Use `C-x k' to kill this buffer.")
(setq w "This is not VI. Use C-x C-s instead!")
(setq wq "This is not VI. Use C-x C-s instead!!")



;;;_. viper 
(setq viper-expert-level '3)
(setq viper-inhibit-startup-message 't)

(defun viper-cua-region-fix()
  (define-key viper-vi-global-user-map [backspace] 'backward-delete-char-untabify)
  (define-key viper-vi-global-user-map "\C-d" 'delete-char)
  (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
  (define-key viper-insert-global-user-map "\C-d" 'delete-char))

(eval-after-load "viper" '(viper-cua-region-fix))

;;;_.. vimpulse
(eval-after-load "viper"
  '(require 'vimpulse))

;;;_. Ex commands without entering viper-mode
;;; from http://www.advogato.org/person/chalst/diary/277.html
;;for ex commands supported by viper, refer `ex-token-alist'

(require 'viper-ex)
(require 'viper-keym)
(require 'viper-cmd)

(define-key global-map (kbd "ESC ESC :") 'viper-ex)
