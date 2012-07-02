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
