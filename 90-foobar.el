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


;;;_. anything-show-kill-ring ¤òÊ¹¤¦¤è¤¦¤ËÐÞÕý¤·¤¿
;; http://dev.ariel-networks.com/articles/emacs/part4/
(defadvice yank-pop (around anything-kill-ring-maybe activate)
  (if (not (eq last-command 'yank))
      (anything-show-kill-ring)
    ad-do-it))

(defadvice cua-paste-pop (around anything-kill-ring-maybe activate)
  (if (not (eq last-command 'yank))
      (anything-show-kill-ring)
    ad-do-it))


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


;;;_. use `pos-tip' to fix the popup window position issue
;; `auto-complete' 1.4 already use `pos-tip'
(when (require 'popup-pos-tip nil t)
  (defadvice popup-tip
    (around popup-pos-tip-wrapper (string &rest args) activate)
    (if (memq window-system '(x windows-nt))
        (apply 'popup-pos-tip string args)
      ad-do-it)))


;;;_. misc enhancement libraries
(idle-require 'menu-bar+)
(idle-require 'info+)
(idle-require 'help-fns+)
(idle-require 'dired+)
(idle-require 'buff-menu+)
;;(idle-require 'facemenu+)

;;(idle-require 'mouse3)
;;(idle-require 'second-sel)

(if (not (featurep 'ide-skel))
    (idle-require 'tabbar-ruler)) 
         

(load "copy-without-sel" 'noerror)
(if (eq window-system 'windows-nt)
    (load "keyword-help" 'noerror))
