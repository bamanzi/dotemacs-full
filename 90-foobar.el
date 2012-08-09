;;* Misc stuff

;;** misc enhancement libraries
(idle-require 'menu-bar+)
;(idle-require 'info+)
(idle-require 'help-fns+)
(idle-require 'dired+)
(idle-require 'buff-menu+)
;;(idle-require 'facemenu+)

;;(idle-require 'mouse3)
;;(idle-require 'second-sel)


;;** misc
;;*** count region
;; http://xahlee.org/emacs/elisp_count-region.html
;; see also: M-= (M-x count-lines-region)
(defun count-region (begin end)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting ...")
  (save-excursion
    (let (wCnt charCnt)
      (setq wCnt 0)
      (setq charCnt (- end begin))
      (goto-char begin)
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq wCnt (1+ wCnt)))

      (message "Words: %d. Chars: %d." wCnt charCnt)
      )))


;;** if no region marked, taken current line as region
;;stolen from https://github.com/andrewsardone/emacs-config/blob/master/modules/aps-core.el#L69
(defmacro allow-line-as-region-for-function (orig-function)
  "Adds an *-or-line version of the given function that
normally requires region arguments.

This code comes from Aquamac's osxkeys.el and is licensed under
the GPL."
`(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
   ()
   ,(format "Like `%s', but acts on the current line if mark is not active."
            orig-function)
   (interactive)
   (if mark-active
       (call-interactively (function ,orig-function))
     (save-excursion
       ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (call-interactively (function ,orig-function))))))

;; taken from Chris Wanstrath's textmate.el
(defun aps-define-comment-or-uncomment-line ()
  (unless (fboundp 'comment-or-uncomment-region-or-line)
    (allow-line-as-region-for-function comment-or-uncomment-region)))




;;** w3m
(define-key search-map (kbd "RET") 'browse-url)
(define-key goto-map   (kbd "RET") 'browse-url)

;; use w3m as default browser. you need to install w3m binary[1] and w3m elisp library[2]
;; [1]  http://w3m.sourceforge.net
;;      cygwin binrary:  http://www.gi.kernel.org/sites/sourceware.cygnus.com/pub/cygwin/release/w3m/
;; [2]  http://emacs-w3m.namazu.org/
;;      latest snapshot: http://packages.debian.org/testing/w3m-el-snapshot
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;(setq browse-url-browser-function 'w3m-browse-url)

(if (boundp 'cygwin-root-path)
    (setq w3m-command (concat cygwin-root-path "/bin/w3m.exe")))

(define-key goto-map (kbd "M-RET") 'w3m-browse-url)

;; default to new tab
(defun w3m-new-tab ()
  (require 'w3m)
  (interactive)
  (w3m-copy-buffer nil nil nil t))

 (defun w3m-browse-url-new-tab (url &optional new-session)
   (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
   (w3m-new-tab)
   (w3m-browse-url url new-session))
;;(setq browse-url-browser-function 'w3m-browse-url-new-tab)

;;(define-key search-map (kbd "C-M-RET") 'w3m-browse-url-new-tab)
;;NOTE: use C-u M-x w3m-browse-url for a new tab


 (eval-after-load "w3m"
   `(progn
      (setq w3m-use-title-buffer-name nil)
     (setq browse-url-browser-function 'w3m-browse-url)
	 
     (define-key w3m-mode-map (kbd "<down>") 'next-line)
     (define-key w3m-mode-map (kbd "<up>") 'previous-line)
     (define-key w3m-mode-map (kbd "<left>") 'backward-char)
     (define-key w3m-mode-map (kbd "<right>") 'forward-char)

     (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
     (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
     (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
     (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-next-page)
       
    ))
;;** CNBlogs Ing
(defun search-cnblogs-ing ()
  (interactive)
  (require 'hide-lines)
  (let ((file (car (last (directory-files "~/Downloads"
                                          t
                                          "CNBlogs_Ing_Backup_[0-9]+.txt")))))           
    (find-file file)
    (with-current-buffer (file-name-nondirectory file)
      (local-set-key (kbd "C-c C-s") 'hide-non-matching-lines)
      (local-set-key (kbd "C-c C-a") 'show-all-invisible)
      
      (show-all-invisible)
      (call-interactively 'hide-non-matching-lines))))
