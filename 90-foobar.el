;;;_. go to scratch buffer
(defun goto-scratch-buffer-on-botton-window ()
  (interactive)
  (require 'windmove)
  (let ( (win (selected-window)) )
    (while (windmove-find-other-window 'down nil win)
      (setq win (windmove-find-other-window 'down nil win)))
    (when win
      (select-window win)
      (switch-to-buffer "*scratch*"))))

(global-set-key (kbd "<f11> s") 'goto-scratch-buffer-on-botton-window)




;;;_. block movement
;;stolen from http://xahlee.org/emacs/xah_emacs_cursor_movement.el
;;(modified: now it move to next occurrence of 3rd newline char)
(defun forward-block ()
  "Move cursor forward to next occurrence of double newline char.
In most major modes, this is the same as `forward-paragraph', however,
this function behaves the same in any mode.
forward-paragraph is mode dependent, because it depends on
syntax table that has different meaning for ¡°paragraph¡±."
  (interactive)
  (skip-chars-forward "\n")
  (when (not (search-forward-regexp "\n[[:blank:]]*\n[[:blank:]]*\n" nil t))
    (goto-char (point-max)) ) )

(defun backward-block ()
  "Move cursor backward to previous occurrence of double newline char.
See: `forward-block'"
  (interactive)
  (skip-chars-backward "\n")
  (when (not (search-backward-regexp "\n[[:blank:]]*\n[[:blank:]]*\n" nil t))
    (goto-char (point-min))
    )
  )

(global-set-key (kbd "C-c n") 'forward-block)
(global-set-key (kbd "C-c p") 'backward-block)


;;;_. count region
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





;;;_. misc enhancement libraries
(idle-require 'menu-bar+)
;(idle-require 'info+)
(idle-require 'help-fns+)
(idle-require 'dired+)
(idle-require 'buff-menu+)
;;(idle-require 'facemenu+)

;;(idle-require 'mouse3)
;;(idle-require 'second-sel)

;;;_. color-theme
(if (< emacs-major-version 24)
    ;; if we not loaded color-theme yet (load your faviourite theme in customize.el)
    (if (not (featurep 'color-theme))
        (and (require 'color-theme nil t)
             (require 'color-theme-tangotango nil t)
             (color-theme-tangotango))))

;; code folding for my init scripts
(add-hook 'find-file-hook '(lambda ()
                             (when (string-match "/.emacs.d/init.d" buffer-file-name)
                                 (if (load "outline-org-like" t)
                                     (outline-org-mode t))
                                 (if (require 'qtmstr-outline nil t)
                                     (qtmstr-outline-mode-hook)))))

;; highlight FIXME/TODO etc in any file
(add-hook 'find-file-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '(("\\<\\(FIXME\\|TODO\\|NOTE\\|BUG\\):" 1 font-lock-warning-face prepend)
                ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))))

         
;;;_. copy without sel
(load "copy-without-sel" 'noerror)

;;;_. language help
(if (eq window-system 'windows-nt)
    (load "keyword-help" 'noerror))

(idle-require 'scratch-log)


