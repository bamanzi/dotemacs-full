
;;** CUA
(setq shift-select-mode t)
(delete-selection-mode t)
(transient-mark-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'meta) 
(cua-mode t)

;; to avoid key conflicting with org-mode
(define-key cua-global-keymap (kbd "<C-return>") nil)
(global-set-key (kbd "C-x r RET") 'cua-set-rectangle-mark)

;;** where I am
(line-number-mode t)
(column-number-mode t)

;;*** hl-line-mode
;;Avaialbe in GNU Emacs 23.2+


;;*** marks
(global-set-key (kbd "C-`")    'set-mark-command)
;;(global-set-key (kbd "M-`")    'exchange-point-and-mark)
(global-set-key (kbd "C-M-`")  'pop-to-mark-command)

(autoload 'visible-mark-mode "visible-mark" "A mode to make the mark visible." t)
(global-set-key (kbd "<f10> vm") 'visible-mark-mode)
(eval-after-load "visible-mark"
  `(global-visible-mark-mode))


;;*** goto last change
(autoload 'goto-last-change  "goto-chg" "Go to the point where the last edit was made in the current buffer." t)
(autoload 'goto-last-change-reverse "goto-chg" "Undocumented." t)

(define-key goto-map "." 'goto-last-change)
(define-key goto-map "," 'goto-last-change-reverse)

;;** tab key & indent
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq tab-stop-list
      (loop for i from 4 to 120 by tab-width collect i))

;;*** abs-indent: to mimic other simple editor's indent/unindent
;; unlike emacs' default settings, this would not use syntax-based indent, but:
;;  - if region selected, indent/unindent the region (tab-width)
;;    * the region mark would not deactivated automatically
;;  - if no region selected, <TAB> would
;;    * if cursor lies in line leading, always indent tab-width
;;    * if cursor lies in word ending and `tab-always-indent' is `complete', try complete
;;    * otherwise, always insert a TAB char or SPACEs
;;  - if no region selected, <S-TAB> would
;;    * if cursor lies in line leading, always unindent tab-width
;;    * otherwise, the cursor would move backwards (tab-width)
;; Note: this implementation would hornor `tab-always-indent', `indent-tabs-mode' and `tab-with'.
 
(defvar abs-indent-complete-function 'dabbrev-expand
  "The function used in `abs-indent' for completion.")
(make-variable-buffer-local 'abs-indent-complete-function)
  
(defun abs-indent (arg)
  "Absolutely indent current line or region. Mimic other editors' indent."
  (interactive "P")
  (let ( (width (or arg tab-width)) )
  (if mark-active
      ;;DONE: how to restore region after `indent-rigidly'
      (let ( (deactivate-mark nil) )
        (indent-rigidly (region-beginning) (region-end) width))
    (let ( (pt           (point))
           (pt-bol       (line-beginning-position))
           (pt-bol-nonws (save-excursion (back-to-indentation) (point))) )
      (if (<= pt pt-bol-nonws)  ;;in leading whitespaces
          (progn
            (back-to-indentation)
            (if (looking-at "$")  ;;all chars in this line are whitespaces or tabs
                  (indent-to (+ (current-column) width))
                (progn
                  (indent-rigidly pt-bol (line-end-position) width)
                  (back-to-indentation))))
        (if (and (eq tab-always-indent 'complete)
                 (looking-at "\\>"))
            (call-interactively abs-indent-complete-function)
          (if indent-tabs-mode
              (insert-char ?\t 1)
            (insert-char ?  width))))))))
  
(defun abs-unindent (arg)
  "Absolutely unindent current line or region."
  (interactive "P")
  (if mark-active
      (let ( (deactivate-mark nil) )
        (indent-rigidly (region-beginning) (region-end) (- tab-width)))
    (let ( (pt           (point))
           (pt-bol       (line-beginning-position))
           (pt-bol-nonws (save-excursion (back-to-indentation) (point))) )
      (if (> pt pt-bol-nonws)  ;;in content
          (move-to-column (max 0 (- (current-column) tab-width)))
        (progn
          (back-to-indentation)
          (backward-delete-char-untabify (min tab-width (current-column))))))))
 
 (defvar abs-indent-mode-map
  (let ( (map (make-sparse-keymap)) )
    (define-key map "\t"        'abs-indent) 
    (define-key map [S-tab]     'abs-unindent)
    map)
  "keymap for `abs-indent-mode'.")

(define-minor-mode abs-indent-mode
  "simple indent just like other editors."
  nil
  " ai"
  abs-indent-mode-map
  (if abs-indent-mode
      t
    t))

;; to used it
;; (add-hook 'ahk-mode-hook #'(lambda ()
;;                            (local-set-key (kbd "<TAB>")    'abs-indent)
;;                            (local-set-key (kbd "<S-TAB>")  'abs-unindent)))

(define-key global-map (kbd "<S-tab>") 'abs-unindent)
(eval-after-load "help-mode"
  `(progn
     (define-key help-mode-map (kbd "<S-tab>") 'backward-button)
     ))


;;** guess indent
(idle-require 'judge-indent)
(eval-after-load "judge-indent"
  `(progn
     (setq judge-indent-default-indent-width 4)  ;;default value of line indent
     (setq judge-indent-default-tab-width 4)
     (setq judge-indent-prefer-tabs-mode nil)  ;;I prefer spaces
     
     (setq judge-indent-major-modes '(c-mode python-mode sh-mode
                                             js-mode espresso-mode))
     (global-judge-indent-mode t)))

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

(define-key goto-map "%" 'goto-match-paren)


(defun select-parened-expression ()
  (interactive)
  (if (re-search-backward "[({]")
      (set-mark (save-excursion
                 (goto-match-paren 1)
                 (point)
                 ))))

(define-key global-map (kbd "C-c (") 'select-parened-expression)

;;*** autopair
(autoload 'autopair-mode "autopair"
  "Automagically pair braces and quotes like in TextMate." t)
(autoload 'autopair-on   "autopair" "Undocumented." t)
(global-set-key (kbd "<f10> ap") 'autopair-mode)

(global-set-key (kbd "<f10> hp") 'highlight-parentheses-mode)
(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(global-set-key (kbd "<f10> hp") 'highlight-parentheses-mode)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(global-set-key (kbd "<f10> rd") 'rainbow-delimiters)


;;** newline & line-wrap
(setq-default truncate-lines t)

(global-set-key (kbd "C-c C-w") 'toggle-truncate-lines)


(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

(setq require-final-newline 't)

;;*** fill
;;(setq-default fill-column 72)
(setq comment-auto-fill-only-comments t)
;;(auto-fill-mode t)

;;stolen from http://sdpconfig.wordpress.com/2011/09/26/writing-a-function-to-unwrap-text-in-emacs/
(defun unfill-region-or-buffer ()
  "Unwrap hard-wrapped text in buffer or region."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (if (region-active-p)
        (fill-region (region-beginning) (region-end))
      (fill-region (point-min) (point-max)))))

(define-key esc-map (kbd "M-q") 'unfill-region-or-buffer)

;;*** eol char
(setq eol-mnemonic-dos   "(dos)"
      eol-mnemonic-unix  "(unix)"
      eol-mnemonic-mac   "(mac)")

;; Convert a buffer from dos ^M end of lines to unix end of lines
;; stolen from http://zhengdong.me/2012/02/05/talk-about-emacs/
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
;; vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))


;;** changes
(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

(global-set-key (kbd "<f10> hc") 'highlight-changes-visible-mode)


;;*** undo
(if (require 'undo-tree nil 'noerror)
    (progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)

      (define-key undo-tree-map (kbd "C-/") nil)
      )
  (message "%s: failed to load `undo-tree'."  load-file-name))


;;** mark, kill & yank
(setq mouse-yank-at-point t) ;;rather than the click point
(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
    )

;; C-y is too far for single-palm 
(if cua-mode
    (define-key global-map (kbd "C-c C-v")  'cua-paste)
  (define-key global-map (kbd "C-c C-v") 'yank))
  

(unless (boundp 'mark-map)
  (defvar mark-map (make-sparse-keymap "Mark...")))
(define-key global-map (kbd "M-`") mark-map)

(unless (boundp 'copy-map)
  (defvar copy-map (make-sparse-keymap "Copy...")))

 
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


(defun mark-current-line (arg)
  "Mark current line."
  (interactive "p")
  (let ( (beg (progn (beginning-of-line) (point)))
         (end (progn (end-of-line arg)   (point))) )
    (push-mark beg 'nomsg 'activate)
    (goto-char (point))))

(define-key mark-map "l" 'mark-current-line)


(defun copy-current-line (arg)
  "Copy current line."
  (interactive "p")
  (save-excursion
    (let ( (beg (progn (beginning-of-line) (point)))
           (end (progn (end-of-line arg)   (point))) )
      (if (fboundp 'pulse-momentary-highlight-region)
          (pulse-momentary-highlight-region beg end))
      (kill-ring-save beg end)
      )))

(define-key copy-map "l" 'copy-current-line)

;;kill current line
;;TIP: C-u C-k would backward kill to the beginning of line
;;(setq kill-whole-line t) ;;ensure the newline char deleted, avoid two C-k


;;*** other 
(idle-require 'mark-copy-something)


;;** misc
(global-set-key (kbd "C-=") 'align-regexp)

;;*** move line up/down
(autoload 'drag-stuff-global-mode "drag-stuff" "Toggle Drag-Stuff mode in every possible buffer." t)
(autoload 'drag-stuff-mode "drag-stuff" "Drag stuff around." t)
(idle-require 'drag-stuff)
(global-set-key (kbd "<f10> ds") 'drag-stuff-mode)

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

