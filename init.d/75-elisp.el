
(global-set-key (kbd "<f3> f") 'find-function-at-point)
(global-set-key (kbd "<f3> F") 'find-function)
(global-set-key (kbd "<f3> v") 'find-variable-at-point)
(global-set-key (kbd "<f3> V") 'find-variable)

(global-set-key (kbd "<f3> k")   'find-function-on-key)

(progn
  (global-set-key (kbd "<f10> : e b") 'eval-buffer)
  (global-set-key (kbd "<f10> : e r") 'eval-region)
  ;;(global-set-key (kbd "<f10> : e f") 'eval-defun)
  ;;(global-set-key (kbd "<f10> : e s") 'eval-sexp)

  (global-set-key (kbd "<f10> : l l") 'load-library)
  (global-set-key (kbd "<f10> : f l") 'find-library)
  (global-set-key (kbd "<f10> : c p") 'check-parens)
  (global-set-key (kbd "<f10> : b c") 'byte-compile-file)
  )

(defun bmz/check-parens ()
  (interactive)
  (check-parens)
  (message "%s: OK" (buffer-file-name)))

(define-key emacs-lisp-mode-map (kbd "<M-f9>") 'bmz/check-parens)

(defun bmz/byte-compile-file (arg)
   (interactive "P")
   (let ( (emacs-lisp-mode-hook '()) )
     (if arg
         (call-interactively 'byte-compile-file)
       (byte-compile-file (buffer-file-name)))))

(define-key emacs-lisp-mode-map (kbd "<C-f9>") 'bmz/byte-compile-file)


(defun bmz/dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (let ( (emacs-lisp-mode-hook '()) )
    (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile t)))

(define-key dired-mode-map "B" 'bmz/dired-do-byte-compile)


;;** code folding
(defun emacs-lisp-mode-init-fold ()
  (if (fboundp 'hideshowvis-enable)
      (hideshowvis-enable)
    (hs-minor-mode t))
  
  (setq outline-regexp "[ \t]*(defun ")
  )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-init-fold)


;;** code complete

(if (fboundp 'ac-emacs-lisp-mode-setup)
    (add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup))


;;;_. documentatin
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;; this would make eldoc-mode to show symbol description
(idle-require 'eldoc-extension)

(defun anything-info-elisp ()
  (interactive)
  (anything
   :input (thing-at-point 'symbol)
   :prompt "Info about: "
   :candidate-number-limit 20
   :sources
   '( anything-c-source-info-elisp
      ;;         anything-c-source-info-elib
      anything-c-source-info-cl)))

(define-key emacs-lisp-mode-map       (kbd "M-s <f1>")  'anything-info-elisp)
(define-key lisp-interaction-mode-map (kbd "M-s <f1>")  'anything-info-elisp)

(defun anything-help-on-elisp-symbol ()
  (interactive)
  (anything
   :input (thing-at-point 'symbol)
   :prompt "Help on: :"
   :sources '( anything-c-source-emacs-functions
               anything-c-source-emacs-variables)))

;;** misc
(defun emacs-lisp-mode-init-misc ()

  ;; disable CEDET's stupid overriden on emacs-lisp-mode
  (if (fboundp 'reset-imenu-function)
      (add-hook 'emacs-lisp-mode-hook 'reset-imenu-function))
  (if (fboundp 'setq-mode-local)
      (setq-mode-local emacs-lisp-mode imenu-create-index-function 'imenu-default-create-index-function))
  
  )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-init-misc)
  
