(defun bmz/check-parens ()
  (interactive)
  (check-parens)
  (message "%s: OK" (buffer-file-name)))

(define-key emacs-lisp-mode-map (kbd "<M-f9>") 'bmz/check-parens)

(defun bmz/byte-compile-file ()
   (interactive)
   (let ( (emacs-lisp-mode-hook '()) )
     (byte-compile-file (buffer-file-name))))

(define-key emacs-lisp-mode-map (kbd "<C-f9>") 'bmz/byte-compile-file)



(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; auto-complete
(if (fboundp 'ac-emacs-lisp-mode-setup)
    (add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup))
