;;** eshell
(setq eshell-cp-interactive-query t
      eshell-mv-interactive-query t
      eshell-ln-interactive-query t
      eshell-rm-interactive-query t
      eshell-cp-overwrite-files nil
      eshell-mv-overwrite-files nil
      eshell-ln-overwrite-files nil)

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

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

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

;;*** eshell commands
(defun eshell/vim (&rest args)
  "Invoke find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\\+\([0-9]+\)\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defalias 'eshell/vi 'eshell/vim)

;;** shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(autoload 'msys-shell    "w32shell" "Run `shell' with MSYS as the shell." t)
(autoload 'cygwin-shell  "w32shell" "Run `shell' with Cygwin as the shell." t)
(autoload 'cmd-shell     "w32shell" "Run `shell' with Windows Command Prompt as the shell." t)

;;** misc
;;*** oneliner: a special shell supporing piping to/from buffer
;; http://oneliner-elisp.sourceforge.net/
;; a special shell that support piping input/output from/to emacs buffer
(autoload 'oneliner "oneliner" "shell-mode hooks for Oneliners" t)


;;*** A quick pop-up shell for emacs
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

;;*** shell-toggle
;;TODO: shell-toggle.el
