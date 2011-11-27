
;;** CUA
(setq shift-select-mode t)
(delete-selection-mode t)
(transient-mark-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'meta) 
(cua-mode t)

;; to avoid key conflicting with org-mode
(define-key cua-global-keymap (kbd "<C-return>") nil)
(global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)

;;** where I am
(line-number-mode t)
(column-number-mode t)

(global-set-key (kbd "C-`")    'set-mark-command)
(global-set-key (kbd "M-`")    'exchange-point-and-mark)
(global-set-key (kbd "C-M-`")  'pop-to-mark-command)

(autoload 'visible-mark-mode "visible-mark.el" "A mode to make the mark visible." t)
(global-set-key (kbd "<f10> vm") 'visible-mark-mode)
(eval-after-load "visible-mark"
  `(global-visible-mark-mode))

;;** tab key & indent
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;** parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;; http://www.emacswiki.org/emacs/ParenthesisMatching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;*** autopair
(autoload 'autopair-mode "autopair" "Automagically pair braces and quotes like in TextMate." t)
(autoload 'autopair-on   "autopair" "Undocumented." t)
(global-set-key (kbd "<f10> ap") 'autopair-mode)

;;** newline & line-wrap
(setq require-final-newline 't)
(setq-default truncate-lines t)
(setq-default fill-column 100)
;;(auto-fill-mode t)

(global-set-key (kbd "C-c C-w") 'toggle-truncate-lines)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;;** changes
(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

;;*** undo
(if (require 'undo-tree nil 'noerror)
    (progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)

      (define-key undo-tree-map (kbd "C-/") nil)
      )
  (message "%s: failed to load `undo-tree'."  load-file-name))


;;** kill & yank
(setq mouse-yank-at-point t) ;;rather than the click point

;; C-y is too far for single-palm 
(if cua-mode
    (define-key global-map (kbd "C-c C-v")  'cua-paste)
  (define-key global-map (kbd "C-c C-v") 'yank))
  

;;*** anything-show-kill-ring を使うように修正した
;; http://dev.ariel-networks.com/articles/emacs/part4/
(defadvice yank-pop (around anything-kill-ring-maybe activate)
  (if (and (not (eq last-command 'yank))
           (fboundp 'anything-show-kill-ring))
      (anything-show-kill-ring)
    ad-do-it))

(defadvice cua-paste-pop (around anything-kill-ring-maybe activate)
  (if (and (not (eq last-command 'yank))
           (fboundp 'anything-show-kill-ring))
      (anything-show-kill-ring)
    ad-do-it))

;;*** kill/yank a line
;;(setq kill-whole-line t)

(defun copy-line (arg)
  "Copy current line."
  (interactive "p")
  (save-excursion
	(let ( (beg (progn (beginning-of-line) (point)))
		   (end (progn (end-of-line arg)   (point))) )
	  (if (fboundp 'pulse-momentary-highlight-region)
		  (pulse-momentary-highlight-region beg end))
	  (kill-ring-save beg end)
	  )))

(global-set-key (kbd "H-l") 'copy-line)


;;** misc
(global-set-key (kbd "C-=") 'align-regexp)

;;*** move line up/down
(autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode in every possible buffer." t)
(autoload 'drag-stuff-mode "drag-stuff.elc" "Drag stuff around." t)
(idle-require 'drag-stuff)
(eval-after-load "drag-stuff"
  `(progn
;;    (setq drag-stuff-modifier 'hyper)
      (add-to-list 'drag-stuff-except-modes 'org-mode)
      (drag-stuff-global-mode t)))

;;*** vi style join-line
(defun join-line ()
  "Join the following line with current line"
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-c J") 'join-line)


;;*** make M-z behave more as zap-up-to-char
(defun zap-up-to-char (arg char)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (interactive "p\ncZap to char: ")
    (zap-to-char arg char)
    (insert char)
    (forward-char -1))

(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun zap-back-to-char (arg char)
  (interactive "p\ncBack-zap to char: ")
  (zap-to-char (- arg) char))

(global-set-key (kbd "ESC M-z") 'zap-back-to-char)

;;*** smart-operator
(autoload 'smart-operator-mode     "smart-operator" "Insert operators with surrounding spaces smartly." t)
(autoload 'smart-operator-mode-on  "smart-operator" "Undocumented." t)
(define-key global-map (kbd "<f10> so") 'smart-operator-mode)

