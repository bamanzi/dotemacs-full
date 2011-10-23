;;* General settings for programming

;;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(which-func-mode t)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(define-key goto-map "e" 'find-tag)

;;;_. compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))
(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)


;;;_ S(@* "buffer navigations")
;;;_. imenu
(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)
(define-key goto-map "i" 'idomenu)
(define-key goto-map "I" 'imenu)
