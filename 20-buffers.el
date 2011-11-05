;;* buffer settings

;;** buffer list 
(global-set-key (kbd "C-c C-b") 'ibuffer)

;;TODO: buffer-menu

;;** buffer-menu
(idle-require 'buff-menu+)


;;** uniquify buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;** show buffer changes
(defun bmz/diff-buffer-with-file (arg)
  (interactive "P")
  (if arg
      (diff-buffer-with-file (current-buffer))
    (call-interactively 'diff-buffer-with-file)))

(global-set-key (kbd "C-c d") 'bmz/diff-buffer-with-file)


(defun bmz/revert-buffer ()
  "revert bufer with close & reopen the file, so local variable would be re-inited."
  (interactive)
  (let ( (file-name (buffer-file-name)) )
    (if (kill-buffer (current-buffer))
        (find-file file-name))))

(global-set-key (kbd "C-x M-r") 'bmz/revert-buffer)


;;** switch buffer
(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)

;;***wcy-swbuffer
(autoload 'wcy-switch-buffer-forward "wcy-swbuff" nil t)
(autoload 'wcy-switch-buffer-backward "wcy-swbuffer" nil t)
(global-set-key (kbd "<C-tab>")      'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)
;;NOTE: if `tabbar-mode' is on, <C-tab> in `tabbar-mode-map' override this

;;** auto-close
;;*** midnight-mode
;;...
;;*** tempbuf
;;(autoload 'turn-on-tempbuf-mode "tempbuf")
(when (load "tempbuf" t)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'view-mode-hook 'turn-on-tempbuf-mode))


