
;;** line numbers & column 
;;*** linum-mode: emacs built-in 
;; linum-mode

;;*** setnu.el
(autoload 'setnu-mode "setnu" "Toggle setnu-mode." t)

;;*** ruler: should column ruler on header-line
;;ruler-mode


;;** iedit: variable/function refactoring
(autoload 'iedit-mode "iedit" "Edit multiple regions with the same content simultaneously." t)
(global-set-key (kbd "C-;") 'iedit-mode)

;;FIXME: newer iedit.el already has `iedit-mode-on-function'
(defun iedit-symbol-in-defun ()
  "Enter `iedit-mode' to rename the symbol in current function, or exit it."
  (interactive)
  (let ( (symbol (thing-at-point 'symbol)) )
    ;;FIXME: judge the symbol type
    (if symbol
        (if (assq 'iedit-mode minor-mode-map-alist)
            (progn
              (iedit-mode -1)
              (widen))
          (narrow-to-defun)
          (iedit-mode t)
          (message "When done, run `iedit-symbol-in-defun' again to quit."))
      (message "You need to put cursor on an identifier."))))


;;** kill & yank
;;*** copy from above line
;; http://www.emacswiki.org/emacs-en/CopyFromAbove
(autoload 'copy-from-above-command "misc"
    "Copy characters from previous nonblank line, starting just above point." t)
(global-set-key (kbd "C-c <down>") 'copy-from-above-command)

(defun copy-char-from-above ()
  (interactive)
  (copy-from-above-command 1))

(global-set-key [C-s-right] 'copy-char-from-above)

;;*** copy/cut current line if no region marked
;;NOTE: not used. as sometimes I use the non-visible region
(when nil
;; Change cutting behavior:
;; "Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea stolen
;; from Slickedit, if you press copy or cut when no region is active, you'll
;; copy or cut the current line."
;; <http://www.zafar.se/bkz/Articles/EmacsTips>
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
)


;;** secondary selection
(autoload 'secondary-to-primary "second-sel" nil t)
(autoload 'primary-to-secondary "second-sel" nil t)
(autoload 'secondary-swap-region "second-sel" nil t)
(define-key search-map (kbd "2") 'primary-to-secondary)
(define-key search-map (kbd "1") 'secondary-to-primary)
(define-key search-map (kbd "`") 'secondary-swap-region)

;;*** swap contents
;; http://www.cnblogs.com/bamanzi/archive/2011/06/04/emacs-secondary-selection.html
(defun transpose-selections ()
  "Transpose the content of the primary region and of the secondary."
  (interactive)
  (let ( (osecondary  (x-get-selection 'SECONDARY)) )
    (unless (and osecondary (overlayp mouse-secondary-overlay))
      (error "No secondary selection"))
    (unless (eq (current-buffer) (overlay-buffer mouse-secondary-overlay))
      (error "Primary selection and secondary selection should be in same buffer."))
    (let* ( (pri-start (region-beginning))
            (pri-end   (region-end))
            (pri-content (buffer-substring pri-start pri-end))
            (sec-start (overlay-start mouse-secondary-overlay))
            (sec-end   (overlay-end mouse-secondary-overlay))
            (sec-content (buffer-substring sec-start sec-end)) )
      ;;(message "swap `%s' with `%s'." pri-content sec-content)
      ;;FIXME: ugly code. any good idea?
      (if (> sec-start pri-start)
          (progn
            ;; move primary's content to secondary's location
            (delete-region sec-start sec-end)
            (goto-char sec-start)
            (insert-string pri-content)

            ;; move secondary's to primary
            (delete-region pri-start pri-end)
            (goto-char pri-start)
            (insert-string sec-content))
        (progn
            ;; move secondary's to primary
            (delete-region pri-start pri-end)
            (goto-char pri-start)
            (insert-string sec-content))

            ;; move primary's content to secondary's location
            (delete-region sec-start sec-end)
            (goto-char sec-start)
            (insert-string pri-content)
      ))))

(define-key search-map (kbd "~") 'transpose-selections)
(global-set-key (kbd "C-x M-t") 'transpose-selections)

;;*** swap contents (method 2)
;; transpose portions of a region around an anchor phrase.  In other
;; words it swaps two regions.
(autoload 'anchored-transpose "anchored-transpose"
  "Transpose portions of the region around an anchor phrase." t)

(global-set-key (kbd "C-x t") 'anchored-transpose)

;;** go to somewhere
;;*** go-to-char
(defun go-to-char (arg char)
  (interactive "p\ncGo to char: ")
  (forward-char 1)
  (if (if arg
          (search-forward (char-to-string char) nil nil arg)
        (search-forward (char-to-string char)))
      (backward-char 1))
  )

(defun go-back-to-char (arg char)
  (interactive "p\ncGo back to char: ")
  (forward-char -1)
  (if arg
      (search-backward (char-to-string char) nil nil arg)
    (search-backward (char-to-string char)))
  )

(global-set-key (kbd "M-g c")      'go-to-char)
(global-set-key (kbd "ESC M-g c")  'go-back-to-char)

;;*** ace-jump-mode
(autoload 'ace-jump-line-mode  "ace-jump-mode"
  "AceJump line mode." t)
(autoload 'ace-jump-char-mode "ace-jump-mode"
  "AceJump char mode" t)

(define-key goto-map "l"  'ace-jump-line-mode)
(define-key goto-map " "  'ace-jump-mode)

;;*** goto-chg
;;   (require 'goto-chg)
(autoload 'goto-last-change "goto-chg"
  "Go to the point where the last edit was made in the current buffer." t)
(autoload 'goto-last-change-reverse "goto-chg"
  "Undocumented." t)


;;   (global-set-key [(control ?.)] 'goto-last-change)
;;   (global-set-key [(control ?,)] 'goto-last-change-reverse)

;;*** goto-last-change
;;(autoload 'goto-last-change "goto-last-change"
;;  "Set point to the position of the last change." t)

;;** misc
;;*** extend selection incrementally (ergoemacs-functions.el)
;; http://xahlee.org/emacs/syntax_tree_walk.html
(autoload 'extend-selection "ergoemacs-functions" nil t)
(global-set-key (kbd "M-#") 'extend-selection)
;; see also: mark-sexp (C-M-SPC), mark-word (M-@)


;;*** quickly swap lines
;; Move line up/down. Stolen from org-mode's M-up/down
;; TODO: support region (move region line up/down)
;; see also:  (@file :file-name "drag-stuff.el" :to "define-minor-mode drag-stuff-mode")
(defun swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

(define-key global-map (kbd "<M-up>")    'swap-line-up)
(define-key global-map (kbd "<M-down>")  'swap-line-down)

;;*** title case
;; http://xahlee.blogspot.com/2011/11/emacs-lisp-example-title-case-string.html
(autoload 'title-case-string-region-or-line  "xeu_elisp_util"
  "Capitalize the current line or text selection, following title conventions." t)
(global-set-key (kbd "ESC M-c") 'title-case-string-region-or-line)


