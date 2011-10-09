;;;_. uniquify buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;;_. show buffer changes
(defun bmz/diff-buffer-with-file (arg)
  (interactive "P")
  (if arg
      (diff-buffer-with-file (current-buffer))
    (call-interactively 'diff-buffer-with-file)))

(global-set-key (kbd "C-c d") 'bmz/diff-buffer-with-file)

;;;_. revert-buffer+
(defun bmz/revert-buffer ()
  "revert bufer with close & reopen the file, so local variable would be re-inited."
  (interactive)
  (let ( (file-name (buffer-file-name)) )
    (if (kill-buffer (current-buffer))
        (find-file file-name))))

(global-set-key (kbd "C-x M-r") 'bmz/revert-buffer)


;;;_. wcy-swbuffer
(autoload 'wcy-switch-buffer-forward "wcy-swbuff" nil t)
(autoload 'wcy-switch-buffer-backward "wcy-swbuffer" nil t)
(global-set-key (kbd "<C-tab>")      'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)

;;;_. TODO: ibuffer/buffer-menu