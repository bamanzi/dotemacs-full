;;; mark-copy-something.el --- mark (and copy) something

;; Maintainer: BaManzi <bamanzi@gmail.com>
;; Keywords: mark, copy

;; This file is not part of GNU Emacs.

;; Part of the code was based on one got from
;;    http://www.emacswiki.org/emacs/CopyWithoutSelection

;;; Commentary:

;; This package provide some commands for easy mark something, such as:
;;   `mark-big-word'    - mark an 'indentifier'
;;   `mark-line'        - mark a line
;;   `mark-string'      - mark a string (syntax based)
;;   `mark-parenthesis' - mark a pair of parenthesis: ( [ {
;;
;;   `mark-between-pattern' - mark the region between the previous
;;                            and next occurrence of a pattern
;;   `mark-between-char'    - mark the region between the previous
;;                            and next occurrence of a char

;; For word and sexp, here provided another commands to mark the whole
;; part of it (`mark-whole-word' & `mark-whole-sexp'), while
;; `mark-word' and `mark-sexp' provided by Emacs itself only start at
;; current point (you have to go back to the beginning first).

;; Upone the mark-xx commands, here also provides the corresponding
;; copy-xx and copy-xx-to-mark commands.

;; Refer function `mark-copy-something--bind-keys' for default keybindings.

;; Note: The code was not based on `thing-at-point', because `thing' depends
;; heavily on syntax table, which make it hard to implement these.


;;; Code:
(require 'pulse nil t)

;;;_. the base functions
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun mark-between (begin-of-thing end-of-thing &optional arg)
  "Mark thing between begin & end.
A second call would expand the mark region."
  (if mark-active
      (funcall end-of-thing (+ arg 1))
   (progn
     (push-mark (get-point begin-of-thing 1) 'nomsg 'activate)
     (funcall end-of-thing arg))))

(defun copy-between (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (if (fboundp 'pulse-momentary-highlight-region)
          (pulse-momentary-highlight-region beg end))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark.
When used in shell-mode, it will paste to the shell prompt."
  (let ((pasteMe
         (lambda()
           (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))

;;;_. word
(defun mark-whole-word (&optional arg)
  "Mark whole word.
Different from `mark-word', this would mark from beginning of the word."
  (interactive "p")
  (mark-between 'backward-word 'forward-word arg))

(defun copy-whole-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "p")
  (copy-between 'backward-word 'forward-word arg)
  )

(defun copy-whole-word-to-mark (&optional arg)
  (interactive "*p")
  (copy-word arg)
  (paste-to-mark arg))

;;;_. big word (i.e. identifier in most languages)
;;; (but in Emacs, nearly any char is valid in symbol. thus we can't use \s_)
(defun beginning-of-big-word (&optional arg)
  (interactive "p")
  (re-search-backward "[^a-z0-9_-]" nil 'noerror 1)
  (if (looking-at "[^a-z0-9_-]")  (goto-char (+ (point) 1)))
  )

(defun end-of-big-word (&optional arg)
  (interactive "p")
  (dotimes (i arg)
    (re-search-forward "[^a-z0-9-_]" nil 'noerror 1)
    (backward-char 1)
    (if (< i (- arg 1))
        ;; skip punctations
        (re-search-forward "[a-z0-9-_]" nil 'noerror 1)))
  )

(defun mark-big-word (&optional arg)
  "Mark whole identifier."
  (interactive "p")
  (mark-between 'beginning-of-big-word 'end-of-big-word arg))

(defun copy-big-word (&optional arg)
  "Copy big word at point"
  (interactive "p")
  (copy-between 'beginning-of-big-word 'end-of-big-word arg)
  )

(defun copy-big-word-to-mark (&optional arg)
  "Copy big word at point"
  (interactive "*p")
  (copy-big-word arg)
  (paste-to-mark arg)
  )

;;;_. line
(defun mark-line (&optional arg)
  "Mark current line(s)."
  (interactive "p")
  (mark-between 'beginning-of-line 'end-of-line arg))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "p")
  (copy-between 'beginning-of-line 'end-of-line arg)
  )

(defun copy-line-to-mark (&optional arg)
  (interactive "p")
  (copy-line arg)
  (paste-to-mark arg))


;;;_. paragraph
;; (defun mark-paragraph (&optional arg)
;;   "Copy paragraphes at point"
;;   (interactive "p")
;;   (mark-between 'backward-paragraph 'forward-paragraph arg)
;;   )

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "p")
  (copy-between 'backward-paragraph 'forward-paragraph arg)
  )

(defun copy-paragraph-to-mark (&optional arg)
  "Try to copy current paragraph and paste it to the mark.
When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "p")
  (copy-paragraph arg)
  (paste-to-mark arg))


;;;_. string
(defun beginning-of-string(&optional arg)
  (re-search-backward "\\s\"" nil 'noerror 1)
  (if (looking-at "\\s\"")  (goto-char (point)))
  )

;; (defun end-of-string(&optional arg)
;;   " "
;;   (re-search-forward "\\s\"" nil t arg)
;;   (if (looking-back "\\s\"") (goto-char (- (point) 1)) )
;;   )

(defun mark-string (&optional arg)
  "Try to mark a string"
  (interactive "p")
  (if (not (memq (face-at-point) '(font-lock-string-face font-lock-doc-face)))
      (message "Current point is not on a STRING.")
    (mark-between 'beginning-of-string 'forward-sexp arg) ;;use sexp to go to the matching quote mark
    )
  )

(defun copy-string (&optional arg)
  "Copy a string."
  (interactive "p")
  (if (not (memq (face-at-point) '(font-lock-string-face font-lock-doc-face)))
      (message "Current point is not on a STRING.")
    (copy-between 'beginning-of-string 'forward-sexp arg) ;;use sexp to go to the matching quote mark
    )
  )

(defun copy-string-to-mark (&optional arg)
  "Try to copy a string and paste it to the mark."
  (interactive "p")
  (if (not (memq (face-at-point) '(font-lock-string-face font-lock-doc-face)))
      (message "Current point is not on a STRING.")
    (copy-string arg)
    (paste-to-mark arg)
    )
  )


;;;_. parenthesis
(defun beginning-of-parenthesis(&optional arg)
  (let* ((event-char (format "%c" last-command-event))
         (paren      (if (string-match event-char "[{(") event-char "(")))
    (search-backward paren nil 'noerror 1)
    (if (looking-at "[\\[<({]")  (goto-char (point))))
  )

;; (defun end-of-parenthesis(&optional arg)
;;   " "
;;   (re-search-forward "]>)}" nil 'noerror arg)
;;   (if (looking-back "]>)}") (goto-char (- (point) 1)) )
;;   )

(defun mark-parenthesis (&optional arg)
  " Try to mark content between a pair of parenthesis."
  (interactive "p")
  (mark-between 'beginning-of-parenthesis 'forward-list arg) ;;use list to find the matching bracket
  )

(defun copy-parenthesis (&optional arg)
  "Try to copy a parenthesis.
When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "p")
  (copy-between 'beginning-of-parenthesis 'forward-list arg) ;;use list to find the matching bracket
  )

(defun copy-parenthesis-to-mark (&optional arg)
  " Try to copy a parenthesis and paste it to the mark.
When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "p")
  (copy-parenthesis arg)
  (paste-to-mark arg)
  )


;;;_. sexp
(defun mark-whole-sexp (&optional arg)
  "Mark the whole expression."
  (interactive "p")
  (mark-between 'backward-sexp 'forward-sexp arg))

(defun copy-sexp (&optional arg)
  "Copy the expression at point "
  (interactive "p")
  (copy-between 'backward-sexp 'forward-sexp arg))  ;;FIXME: not I want

(defun copy-sexp-to-mark (&optional arg)
  (interactive "p")
  (copy-sexp arg)
  (paste-to-mark arg))

;;;_. defun
;;;`mark-defun' already provided by Emacs


;;;_. any pattern
(defun mark-between-pattern (within pattern)
  "Mark the region between the previous and the next occurrence of the PATTERN.
If WITHIN is non-nil, only text within marked, otherwise leading and ending occurrence of
the PATTERN is included."
  (interactive "P\nsPattern: ")
  (let ( (start (progn (search-backward pattern) (point))) )
    (if within (forward-char (length pattern)))
    (push-mark (point) 'nomsg 'activate)
    (forward-char 1)
    (search-forward pattern)
    (if within (backward-char 1))))
 
(defun mark-between-char (within char)
  "Mark the region between a pair of CHAR."
   (interactive (list current-prefix-arg
                      (format "%c" last-command-event)))
   (mark-between-pattern within char))
 
(defun copy-between-pattern (within pattern)
  (interactive "*P\nsPattern: ")
  (mark-between within pattern)
  (copy-region-as-kill (region-beginning) (region-end)))
 
(defun copy-between-char (within char)
  "Copy the region between a pair of CHAR."
  (interactive (list current-prefix-arg
                     (format "%c" last-command-event)))
  (mark-between-pattern within char)
  (copy-region-as-kill (region-beginning) (region-end)))

;;;_. key bindings

(defun mark-copy-something--bind-keys ()
  (unless (boundp 'mark-map)
    (defvar mark-map (make-sparse-keymap "Mark...")))
  (global-set-key (kbd "C-c m") mark-map)

  (define-key mark-map "w" 'mark-whole-word)
  (define-key mark-map "W" 'mark-big-word)
  (define-key mark-map "l" 'mark-line)
  (define-key mark-map "p" 'mark-paragraph)  ;;Emacs built-in
  (define-key mark-map "s" 'mark-string)
  (define-key mark-map "(" 'mark-parenthesis)
  (define-key mark-map "[" 'mark-parenthesis)
  (define-key mark-map "{" 'mark-parenthesis)
  (define-key mark-map "e" 'mark-sexp)       ;;Emacs built-in
  (define-key mark-map "f" 'mark-defun)      ;;Emacs built-in

  (define-key mark-map "\"" 'mark-between-char)
  (define-key mark-map "'"  'mark-between-char)
  (define-key mark-map " "  'mark-between-char)
  (define-key mark-map "-"  'mark-between-char)
  (define-key mark-map "P"  'mark-between-pattern)

  (unless (boundp 'copy-map)
    (defvar copy-map (make-sparse-keymap "Copy...")))
  (global-set-key (kbd "C-c c") copy-map)

  (define-key copy-map "w" 'copy-whole-word)
  (define-key copy-map "W" 'copy-big-word)
  (define-key copy-map "l" 'copy-line)
  (define-key copy-map "p" 'copy-paragraph)
  (define-key copy-map "s" 'copy-string)
  (define-key copy-map "(" 'copy-parenthesis)
  (define-key copy-map "[" 'copy-parenthesis)
  (define-key copy-map "{" 'copy-parenthesis)
  (define-key copy-map "e" 'copy-sexp)

  (define-key copy-map "\"" 'copy-between-char)
  (define-key copy-map "'"  'copy-between-char)
  (define-key copy-map " "  'copy-between-char)
  (define-key copy-map "-"  'copy-between-char)
  (define-key copy-map "P"  'copy-between-pattern)

  (unless (boundp 'copy-to-mark-map)
    (defvar copy-to-mark-map (make-sparse-keymap "Copy to mark...")))
  (global-set-key (kbd "C-c p") copy-to-mark-map)

  ;;Hint: press C-SPC twice to set a mark without activate the region
  (define-key copy-to-mark-map "w" 'copy-whole-word-to-mark)
  (define-key copy-to-mark-map "W" 'copy-big-word-to-mark)
  (define-key copy-to-mark-map "l" 'copy-line-to-mark)
  (define-key copy-to-mark-map "p" 'copy-paragraph-to-mark)
  (define-key copy-to-mark-map "s" 'copy-string-to-mark)
  (define-key copy-to-mark-map "(" 'copy-parenthesis-to-mark)
  (define-key copy-to-mark-map "{" 'copy-parenthesis-to-mark)
  (define-key copy-to-mark-map "e" 'copy-sexp-to-mark)

  t
  )

(defvar mark-copy-something--dont-bind-keys nil
  "Whether to use key bindings provided in this file.")

(unless mark-copy-something--dont-bind-keys
    (mark-copy-something--bind-keys))

(provide 'mark-copy-something)

;;;mark-copy-something.el ends here
