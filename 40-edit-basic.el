
;;** CUA
(setq shift-select-mode t)
(delete-selection-mode t)
(transient-mark-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'meta) 
(cua-mode t)

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
(setq tab-always-indent t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;;** parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

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

;;(setq kill-whole-line t)

;;;_. anything-show-kill-ring を使うように修正した
;; http://dev.ariel-networks.com/articles/emacs/part4/
(defadvice yank-pop (around anything-kill-ring-maybe activate)
  (if (not (eq last-command 'yank)
      (anything-show-kill-ring)
    ad-do-it))

(defadvice cua-paste-pop (around anything-kill-ring-maybe activate)
  (if (not (eq last-command 'yank))
      (anything-show-kill-ring)
    ad-do-it))


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

