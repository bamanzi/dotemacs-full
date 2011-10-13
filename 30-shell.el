;;**  A quick pop-up shell for emacs
;;Based on code stolen from http://tsdh.wordpress.com/2011/10/12/a-quick-pop-up-shell-for-emacs/
(defvar th-shell-popup-buffer nil)

(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
	(dir (file-name-directory (or (buffer-file-name)
				      user-init-file
				      default-directory))))
    (if (and win (eq (current-buffer) th-shell-popup-buffer))
        (delete-window win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " (if (eq system-type 'windows-nt)
						(concat "/d " (replace-regexp-in-string "/" "\\\\" dir))
					      dir)
					      "\n")))))

(global-set-key (kbd "<f11> ~") 'th-shell-popup)
