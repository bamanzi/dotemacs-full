
;;** icomplete
(icomplete-mode t)  ;; completion for minibuffer (commands (M-x)
                    ;; variables (C-h v, customize-variable), functions (C-h f))

;;** ido
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(require 'ido)
(unless (fboundp 'ido-common-initialization)   ;;workaround for emacs 23.1's bug(?)
  (defun ido-common-initialization ()
    (ido-init-completion-maps)
    (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
    (add-hook 'choose-completion-string-functions 'ido-choose-completion-string))

  (defadvice ido-completing-read (before ido-completing-read-fix)
    (ido-common-initialization))
  )
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
;;(ido-mode t)
(ido-mode 'buffers)


;;** smex : ido for M-x
(autoload 'smex "smex" nil t)
(autoload 'smex-major-mode-commands "smex" nil t)
(setq smex-save-file "~/.emacs.d/smex-items")
(global-set-key (kbd "ESC M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(eval-after-load "smex"
  `(progn
      (smex-initialize)))


;;** history
(define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)

;;** misc
(define-key minibuffer-local-map (kbd "ESC ESC") 'minibuffer-keyboard-quit)



