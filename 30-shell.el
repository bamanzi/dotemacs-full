;;stolen from http://linuxtoy.org/archives/emacs-eshell.html
(eval-after-load "auto-complete"
  `(progn
     (ac-define-source eshell-pcomplete
       '((candidates . pcomplete-completions)))

     (add-to-list 'ac-modes 'eshell-mode)
     ))

;; (defun ac-complete-eshell-pcomplete ()
;;   (interactive)
;;   (auto-complete '(ac-source-eshell-pcomplete)))

;;FIXME: (add-to-list hs-special-modes-alist
;;             '(eshell-mode "^.* $" nil nil))

(defun my-eshell-mode-init ()
  ;; swap <home> and C-a
  (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)

  (setq outline-regexp "^.* $")
  (outline-minor-mode t)

  (when (boundp 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
    (add-to-list 'ac-sources 'ac-source-eshell-pcomplete))
  )

(add-hook 'eshell-mode-hook 'my-eshell-mode-init)


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
