
;;* shell
;;** which shell?


(autoload 'msys-shell    "w32shell" "Run `shell' with MSYS as the shell." t)
(autoload 'cygwin-shell  "w32shell" "Run `shell' with Cygwin as the shell." t)
(autoload 'cmd-shell     "w32shell" "Run `shell' with Windows Command Prompt as the shell." t)

(defun w32-toggle-gnu-shell ()
  "Toggle shell between `cmdproxy' and `bash'"
  (interactive)
  (if (string-match "cmdproxy" shell-file-name)
      (progn
        (setq shell-file-name "bash")
        (setq explicit-shell-file-name shell-file-name
              explicit-bash-args "-c")
;;        (setq shell-quote-argument
        )
    (progn
      (setq shell-file-name "cmdproxy")
      (setq explicit-shell-file-name shell-file-name
            explicit-bash-args "-c")
      ))
  (message "`shell-file-name' now set to %s" shell-file-name)
  )
            
        

;;** shell-mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;*** show current dir in mode-line
(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification 
               '(:propertize (" " default-directory " ") face dired-directory)))
;;(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)


;;** eshell
(setq eshell-cp-interactive-query t
      eshell-mv-interactive-query t
      eshell-ln-interactive-query t
      eshell-rm-interactive-query t
      eshell-cp-overwrite-files nil
      eshell-mv-overwrite-files nil
      eshell-ln-overwrite-files nil)

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

;;FIXME: (add-to-list hs-special-modes-alist
;;             '(eshell-mode "^.* $" nil nil))

(defun bmz/eshell-mode-init ()
  ;; swap <home> and C-a
  (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol)

  (setq outline-regexp "^.* $")
  (outline-minor-mode t)
  )

(add-hook 'eshell-mode-hook 'bmz/eshell-mode-init)

;;*** completion
;;stolen from http://linuxtoy.org/archives/emacs-eshell.html
(eval-after-load "auto-complete"
  `(progn
     (ac-define-source eshell-pcomplete
       '((candidates . pcomplete-completions)))

     (add-to-list 'ac-modes 'eshell-mode)

     ))

(defun bmz/eshell-mode-init-ac ()
  (if (require 'pcase nil t) ;;for Emacs 23, you need fetch a pcase.el from Emacs 24
      (require 'pcmpl-args nil t))
  (require 'pcmpl-apt nil t)
  (require 'pcmpl-git nil t)
  ;; `monky.el' would provide pcomplete support for `hg'
  
  (when (boundp 'ac-sources)
    (add-to-list 'ac-sources 'ac-source-files-in-current-dir)
    (add-to-list 'ac-sources 'ac-source-eshell-pcomplete))
  )

(add-hook 'eshell-mode-hook 'bmz/eshell-mode-init-ac)

;; (defun ac-complete-eshell-pcomplete ()
;;   (interactive)
;;   (auto-complete '(ac-source-eshell-pcomplete)))

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

(defun eshell/start (file)
    "Invoke system's associated application for FILE.
On Windows, baskslashes is substituted with slashes."
    (if (eq system-type 'gnu/linux)
        (shell-command (concat "gnome-open "
                               (shell-quote-argument (file))))
      (w32-shell-execute "Open"
                       (subst-char-in-string ?\\ ?/ (expand-file-name file))
		       nil)))

(defun eshell/clear()
  "to clear the eshell buffer."
  (interactive)  
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;*** esh-toggle
(autoload 'eshell-toggle "esh-toggle"
  "Toggles between the *eshell* buffer and the current buffer." t)
(autoload 'eshell-toggle-cd "esh-toggle"
  "Calls `eshell-toggle' and let it cd to path of current buffer." t)

(global-set-key (kbd "<f12> E") 'eshell-toggle)
(global-set-key (kbd "<f12> e") 'eshell-toggle-cd)

;;** shell-toggle
;;*** term-toggle
(autoload 'term-toggle "term-toggle" 
  "Toggles between the *terminal* buffer and whatever buffer you are editing."
  t)
(autoload 'term-toggle-cd "term-toggle" 
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)

(global-set-key (kbd "<f12> T") 'term-toggle)
(global-set-key (kbd "<f12> t") 'term-toggle-cd)

;;*** shell-toggle
(autoload 'shell-toggle "shell-toggle" 
  "Toggles between the *shell* buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle" 
  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)

(global-set-key (kbd "<f12> S") 'shell-toggle)
(global-set-key (kbd "<f12> s") 'shell-toggle-cd)

;;*** comint-toggle
(autoload 'comint-toggle "comint-toggle"
  "Switch to comint buffer from editing buffer or switch back." t)

(global-set-key (kbd "C-`") 'comint-toggle) ;;C-` not available on xterm
(global-set-key (kbd "<f12> `") 'comint-toggle)

                
;;*** A quick pop-up shell for emacs (lightweight one)
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

(global-set-key (kbd "<f12> ~") 'th-shell-popup)


;;** misc
;;*** oneliner: a special shell supporing piping to/from buffer
;; http://oneliner-elisp.sourceforge.net/
;; a special shell that support piping input/output from/to emacs buffer
(autoload 'oneliner "oneliner" "shell-mode hooks for Oneliners" t)


