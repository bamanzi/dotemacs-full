;;;_. icomplete
(icomplete-mode t)  ;; completion for minibuffer (commands (M-x)
                    ;; variables (C-h v, customize-variable), functions (C-h f))

;;;_. ido
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
(ido-mode t)

;;;_. smex : ido for M-x
(if (require 'smex nil t)
    (progn
      (smex-initialize)
  
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      ;; This is your old M-x.
      (global-set-key (kbd "ESC M-x") 'execute-extended-command))
  (progn
    (message "%s: failed to load `smex'." load-file-name)))



(define-key minibuffer-local-map (kbd "ESC ESC") 'minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)
