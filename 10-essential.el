;;** gui options
(setq use-dialog-box nil
      menu-prompting t) ;;NOTE: set `menu-prompting' to nil would cause mode menu not poping up
                                        ; when clicking on mode line

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

;;(set-frame-parameter (car (frame-list)) 'icon-type "~/.emacs.d/emacs_07.ico")
;;(set-frame-parameter (car (frame-list)) 'icon-type "~/.emacs.d/emacs_07.png")

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)
    )

;;** session
(setq desktop-restore-eager 5)
(require 'desktop)
(desktop-save-mode t)

;;*** save & load desktop session to/from bookmarks
;;(idle-require 'bookmark+)
(autoload 'bmkp-set-desktop-bookmark "bookmark+"
  "Save the desktop as a bookmark." t)
(autoload 'bmkp-desktop-jump "bookmark+"
  "Jump to a desktop bookmark." t)

(global-set-key (kbd "C-x r K") 'bmkp-set-desktop-bookmark)
(global-set-key (kbd "C-x j K") 'bmkp-desktop-jump)


;;** key modifiers and prefix keys

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



;;** anything
(global-set-key (kbd "<f5> r") 'anything-recentf)
(global-set-key (kbd "<f5> b") 'anything-buffers+)
(global-set-key (kbd "<f5> B") 'anything-bookmarks)
(global-set-key (kbd "<f5> l") 'anything-locate)
(global-set-key (kbd "<f5> c") 'anything-browse-code)
(global-set-key (kbd "<f5> i") 'anything-imenu)
(global-set-key (kbd "<f5> o") 'anything-occur)


(if (and (load "anything" t)
         (load "anything-config" t))
    (progn
      ;;enable multiple keyword/regexp match
      ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
      ;;(global-set-key (kbd "M-x") 'anything-M-x)

      ;;latest `anything-config.el' canceled "<f5> a", but I like it
      (unless (global-key-binding (kbd "<f5> a"))
        (global-set-key (kbd "<f5> a") anything-command-map))

      (define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)
      )
  (message "%s: failed to load `anything'." load-file-name))


;;** themes
(when (< emacs-major-version 24)
    ;; if we not loaded color-theme yet (load your faviourite theme in customize.el)
    (when (not (featurep 'color-theme))
      (when (require 'color-theme nil t)
        (require 'color-theme-tangotango nil t)
        (when (featurep 'color-theme-tangotango)
           (color-theme-tangotango))))
    )

(when t
    ;; (idle-require 'color-theme-zenburn)
    
    (autoload 'color-theme-tango  "color-theme-tango"
      "A color theme based on Tango Palette." t)
    (autoload 'color-theme-tango-light  "color-theme-tango"
      "A color theme based on Tango Palette." t)

    (autoload 'color-theme-sanityinc-dark  "color-theme-sanityinc"
      "Based on `color-theme-subdued" t)
    (autoload 'color-theme-sanityinc-light  "color-theme-sanityinc"
      "Based on `color-theme-pierson`" t)

    (autoload 'color-theme-solarized-dark  "color-theme-solarized"
      "Undocumented." t)
    (autoload 'color-theme-solarized-light "color-theme-solarized"
      "Undocumented." t)

    (autoload 'color-theme-zenburn "color-theme-zenburn"
      "Just some alien fruit salad to keep you in the zone." t)
    (autoload 'color-theme-tangotango "color-theme-tangotango"
      "A color theme based on Tango Palette colors." t)
    )

;;put face-adjusting code to hook `after-make-frame-functions'
;;then use this to call them
(global-set-key (kbd "<f12> <f12>")
                #'(lambda ()
                    (interactive)
                    (run-hook-with-args 'after-make-frame-functions
                                        (selected-frame))))

;;** recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)





