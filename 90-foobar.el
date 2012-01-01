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

(idle-require 'scratch-log)

;;*** copy without sel
(load "copy-without-sel" 'noerror)

;;*** language help
(if (eq window-system 'windows-nt)
    (load "keyword-help" 'noerror))

;;** go to scratch buffer
(defun goto-scratch-buffer-on-botton-window ()
  (interactive)
  (require 'windmove)
  (let ( (win (selected-window)) )
    (while (windmove-find-other-window 'down nil win)
      (setq win (windmove-find-other-window 'down nil win)))
    (when win
      (select-window win)
      (switch-to-buffer "*scratch*"))))

;;(global-set-key (kbd "<f11> s") 'goto-scratch-buffer-on-botton-window)


;;** block movement
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


;;**  count region
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




;;** color-theme
(if (< emacs-major-version 24)
    ;; if we not loaded color-theme yet (load your faviourite theme in customize.el)
    (if (not (featurep 'color-theme))
        (and (require 'color-theme nil t)
             (require 'color-theme-tangotango nil t)
             (color-theme-tangotango))))


;;** force `info' showing in another frame
(defvar special-display-buffer-other-frame-regexps
  '("*info*")
  "The buffer names that would be forced to display in another frame.")
 
(defvar display-buffer-function-orig nil
  "Old value of `display-buffer-function'.")
   
(defun display-buffer-use-other-frame-first  (buffer &optional other-window frame)
  "A `display-buffer-function' implementation.
This one would force using other frame (if none, this would create a new one)
to display some special buffers specified in `. For non-special"
  (let* ((buffer-name (buffer-name buffer))
         (use-other-frame   (catch 'found
                              (dolist (regexp special-display-buffer-other-frame-regexps)
                                (cond
                                 ((stringp regexp)
                                  (when (string-match-p regexp buffer-name)
                                    (throw 'found t)))
                                 ((and (consp regexp) (stringp (car regexp))
                                       (string-match-p (car regexp) buffer-name))
                                  (throw 'found (cdr regexp)))))))
         frame
         window)
    ;;(message "use-other-frame=%s" use-other-frame)
    (if (and use-other-frame (display-graphic-p))
        (progn
          (setq frame (if (eq (selected-frame) (next-frame))
                          (make-frame)
                        (next-frame)))
          (setq window (car (window-list frame)))
          (set-window-buffer window buffer)
          window)
      (let ((display-buffer-function display-buffer-function-orig)) ;;Emacs default
        (display-buffer buffer other-window frame)))))
 
   
(defadvice info (around info-other-frame activate)
  ;;In current Emacs's implementation of `display-buffer',
  ;;`special-display-function' is too late for special buffers.
  ;;I have to override `display-buffer' temporarily.
  (setq display-buffer-function-orig  display-buffer-function)
  (let ((display-buffer-function      'display-buffer-use-other-frame-first)
        (after-make-frame-functions   '()))
    ad-do-it
    ))
 
;;for testing
;;(ad-deactivate 'info)
;;(ad-activate 'info)
;; (info "(emacs)Top")

