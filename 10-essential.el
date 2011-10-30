;;;_ S(@* "gui options")
(setq use-dialog-box nil
      menu-prompting nil)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)
    )


;;;_. key modifiers and prefix keys

(when (eq window-system 'w32)
  ;;(setq w32-lwindow-modifier 'super)
  
  (setq w32-lwindow-modifier 'super) 
  (setq w32-pass-lwindow-to-system nil) ;;if set to nil, a single press <lwindow> would prevent Start Menu

  (setq w32-rwindow-modifier 'alt)      
  (setq w32-pass-rwindow-to-system nil)
  
  (setq w32-apps-modifier 'hyper)
  (setq w32-pass-apps-to-system nil)
  
  (setq w32-scroll-lock-modifier nil)
  )
  
;;FIXME: not work
(when (eq window-system 'x)
  (global-unset-key (kbd "<menu>"))
  (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier)  
  )



;;;_. anything
(if (and (load "anything" t)
         (load "anything-config" t))
    (progn
      ;;enable multiple keyword/regexp match
      ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
      ;;(global-set-key (kbd "M-x") 'anything-M-x)
  
      (global-set-key (kbd "<f5> r") 'anything-recentf)
      (global-set-key (kbd "<f5> b") 'anything-buffers+)
      (global-set-key (kbd "<f5> B") 'anything-bookmarks)
      (global-set-key (kbd "<f5> l") 'anything-locate)
      (global-set-key (kbd "<f5> c") 'anything-browse-code)
      (global-set-key (kbd "<f5> i") 'anything-imenu)
      (global-set-key (kbd "<f5> o") 'anything-occur)

      (define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)
      )
  (message "%s: failed to load `anything'." load-file-name))


;;;_. recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)





;;;_. utils
(define-key goto-map "d" 'dired-jump) ;;C-x C-j

	 
  
