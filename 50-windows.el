
;;;_ M-1, M-2 to different window
(if (require 'window-numbering nil t)
    (window-numbering-mode t)
  (if (require 'window-number nil t)
      (window-number-meta-mode t)))

;;;_ winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)


;;;_ tabbar-mode
;;(unless (require 'ide-skel nil t)  ;; `ide-skel' would load `tabbar' and  make its own tab group settings
;;        (require 'tabbar nil t))
(when (require 'tabbar nil t)
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )

;;;_. tabbar-rules
;;;.....

;;;_ window-numbering (M-1 jump to first window, M-2 to the second...)
(autoload 'window-numbering-mode "window-numbering" "A minor mode that assigns a number to each window" t)
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
  t)
(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(condition-case nil
  (window-numbering-mode t)
  (window-numer-meta-mode t))

;;;_ popwin
;;;TODO: ?


;;;_ misc
;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.


;;;_. maximize frame
(when (and window-system
           (or (require 'maxframe nil t)
               (require 'fit-frame nil t)))
           
  ;; (setq mf-max-width 1600)  ;; Pixel width of main monitor.
  (maximize-frame)
  ;; maximize any new frame
  (add-hook 'window-setup-hook 'maximize-frame t))

;;;_. opening server files always in a new frame
;;http://www.emacswiki.org/emacs/EmacsClient#toc21

(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

