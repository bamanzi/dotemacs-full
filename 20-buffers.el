;;* buffer settings

;;(global-set-key (kbd "<C-f4>") 'kill-buffer)
(autoload 'close-current-buffer "ergoemacs-functions" "Close the current buffer." t)
(global-set-key (kbd "<C-f4>") 'close-current-buffer)
(global-set-key (kbd "C-c b c")  'close-current-buffer)


;;** buffer navigation
(global-set-key (kbd "<C-tab>")   'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)

;;*** wcy-swbuffer
(autoload 'wcy-switch-buffer-forward "wcy-swbuff" nil t)
(autoload 'wcy-switch-buffer-backward "wcy-swbuffer" nil t)
(global-set-key (kbd "<C-tab>")      'wcy-switch-buffer-forward)
(global-set-key (kbd "<C-S-kp-tab>") 'wcy-switch-buffer-backward)
;;NOTE: if `tabbar-mode' is on, <C-tab> in `tabbar-mode-map' override this

(defun ido-switch-user-buffer ()
  "Choose and switch to another buffer, by its name. Special buffers ignored."
  (interactive)
  (let* ( (buffers (delq nil (mapcar #'(lambda (buf)
                                      (if (string-match "^[ *]" (buffer-name buf))
                                          nil
                                        (buffer-name buf)))
                                  (buffer-list))))
          (choice (ido-completing-read "Switch to: " (cdr buffers))) )
    (switch-to-buffer (get-buffer choice))))

(global-key-binding (kbd "C-x B") 'ido-switch-user-buffer)

;;** buffer management
;;C-x C-b : buffer-menu
(idle-require 'buff-menu+)
(global-set-key (kbd "C-c b m")     'buffer-menu)

;;`electric-buffer-list' derived from `buffer-menu'
;;but you can mark multiple buffers and perform operations at a single ENTER key
(global-set-key (kbd "C-c b e")     'electric-buffer-list)

(global-set-key (kbd "C-c b <f5>")  'anything-buffers+)

;;*** ibuffer
(global-set-key (kbd "C-c b i")     'ibuffer)

;;**** ibuffer-vc
;;(idle-require 'ibuffer-vc)

;;** temporary buffers
;;*** midnight-mode
(setq midnight-mode t)
(idle-require 'midnight)

;;*** tempbuf
;;(autoload 'turn-on-tempbuf-mode "tempbuf")
(idle-require 'tempbuf)
(eval-after-load "tempbuf"
  `(progn
     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
  ))


;;** misc

;;*** mode-line
;;(setq mouse-buffer-menu-maxlen 20)
(define-key mode-line-buffer-identification-keymap (kbd "<mode-line> <down-mouse-2>") 'mouse-buffer-menu)

;;*** uniquify buffer name
(idle-require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;(setq uniquify-buffer-name-style 'forward)

;;(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
;;(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;;*** show buffer changes
(defun bmz/diff-buffer-with-file (arg)
  (interactive "P")
  (if arg
      (diff-buffer-with-file (current-buffer))
    (call-interactively 'diff-buffer-with-file)))

(global-set-key (kbd "C-c d") 'bmz/diff-buffer-with-file)


(defun bmz/revert-buffer ()
  "revert bufer with close & reopen the file, so local variable would be re-inited."
  (interactive)
  (let ( (file-name (buffer-file-name))
         (pt        (point)) )
    (when (kill-buffer (current-buffer))
        (find-file file-name)
        (goto-char pt))))

(global-set-key (kbd "C-x M-r") 'bmz/revert-buffer)



;;*** Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-speicial-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified, Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (not (string-equal name "*Messages*"))
	  ;; (not (string-equal name "*Buffer List*"))
	   (not (string-equal name "*Shell Command Output*"))
	   (not (string-equal name "*scratch*"))
	   (not (string-equal name "*nav*"))
	   (not (string-equal name "*imenu-tree*"))
	   (= (aref name 0) ?*)
	   (if (buffer-modified-p buffer)
	       (if (yes-or-no-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))

;;This function kills all the buffer except the *scratch* buffer. Call it with "alt-x nuke-all-buffers"

;; Kills all them buffers except scratch
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
	  (buffer-list))
  (delete-other-windows))


(defun nuke-unmodified-buffers (&optional list)
  "For each buffer in LIST, kill it if unmodified."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           (not (string-equal name "*Messages*"))
           ;; (not (string-equal name "*Buffer List*"))
           (not (string-equal name "*buffer-selection*"))
           (not (string-equal name "*Shell Command Output*"))
           (not (string-equal name "*scratch*"))
           (not (string-equal name "*nav*"))
           (not (string-equal name "*imenu-tree*"))       
           (/= (aref name 0) ? )
           (unless (buffer-modified-p buffer)
             (kill-buffer buffer))))
    (setq list (cdr list))))
