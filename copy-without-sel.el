;; based on code from http://www.emacswiki.org/emacs/CopyWithoutSelection

(require 'pulse nil t)

;;;_. the base functions
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (if (fboundp 'pulse-momentary-highlight-region)
          (pulse-momentary-highlight-region beg end))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
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

;;;_. copy word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  )

(defun copy-word-to-mark (&optional arg)
  (interactive "P")
  (copy-word arg)
  (paste-to-mark arg))
  
;;;_. copy line
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  )

(defun copy-line-to-mark (&optional arg)
  (interactive "P")
  (copy-line arg)
  (paste-to-mark arg))


;;;_. copy paragraph
(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  )

(defun copy-paragraph-to-mark (&optional arg)
  "Try to copy current paragraph and paste it to the mark.
When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (copy-paragraph arg)
  (paste-to-mark arg))
  

;;;_. copy string
(defun beginning-of-string(&optional arg)
  "  "
  (re-search-backward "\\s\"" nil 'noerror 1)
  (if (looking-at "\\s\"")  (goto-char (+ (point) 1)) )
  )

(defun end-of-string(&optional arg)
  " "
  (re-search-forward "\\s\"" nil t arg)
  (if (looking-back "\\s\"") (goto-char (- (point) 1)) )
  )

(defun copy-string (&optional arg)
  "Try to copy a string and put it into kill-ring & clipboard."
  (interactive "P")
  (if (not (eq (face-at-point) 'font-lock-string-face))
      (message "Current point is not on a STRING.")
    (copy-thing 'beginning-of-string 'end-of-string arg)
    ;;(paste-to-mark arg)
    )
  )

(defun copy-string-to-mark (&optional arg)
  "Try to copy a string and paste it to the mark.
When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (if (not (eq (face-at-point) 'font-lock-string-face))
      (message "Current point is not on a STRING.")
    (copy-thing 'beginning-of-string 'end-of-string arg)
    (paste-to-mark arg)
    )
  )


;;;_. copy parenthesis
(defun beginning-of-parenthesis(&optional arg)
  "  "
  (re-search-backward "\[<({" nil 'noerror 1)
  (if (looking-at "\[<({")  (goto-char (+ (point) 1)) )
  )

;;FIXME: should search for a matching bracket
(defun end-of-parenthesis(&optional arg)
  " "
  (re-search-forward "]>)}" nil 'noerror arg)
  (if (looking-back "]>)}") (goto-char (- (point) 1)) )
  )

(defun copy-parenthesis (&optional arg)
  " Try to copy a parenthesis and paste it to the mark
     When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "P")
  (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
  )

(defun copy-parenthesis-to-mark (&optional arg)
  " Try to copy a parenthesis and paste it to the mark.
When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "P")
  (copy-parenthesis arg)
  (paste-to-mark arg)
  )


;;;_. sexp
(defun copy-sexp (&optional arg)
  " "
  (interactive "P")
  (copy-thing 'backward-sexp 'forward-sexp arg))  ;;FIXME: not I want

(defun copy-sexp-to-mark (&optional arg)
  (interactive "P")
  (copy-sexp arg)
  (paste-to-mark arg))


;;;_. key bindings
(defvar copy-map (make-sparse-keymap "Copy without selection"))
(global-set-key (kbd "C-c c") copy-map)

(define-key copy-map "w" 'copy-word)
(define-key copy-map "l" 'copy-line)
(define-key copy-map "p" 'copy-paragraph)
(define-key copy-map "s" 'copy-string)
(define-key copy-map "(" 'copy-parenthesis)
(define-key copy-map "e" 'copy-sexp)

(defvar copy-to-mark-map (make-sparse-keymap "Copy to mark without selection"))
(global-set-key (kbd "C-c p") copy-to-mark-map)

;;Hint: press C-SPC twice to set a mark without activate the region 
(define-key copy-to-mark-map "w" 'copy-word-to-mark)
(define-key copy-to-mark-map "l" 'copy-line-to-mark)
(define-key copy-to-mark-map "p" 'copy-paragraph-to-mark)
(define-key copy-to-mark-map "s" 'copy-string-to-mark)
(define-key copy-to-mark-map "(" 'copy-parenthesis-to-mark)
(define-key copy-to-mark-map "e" 'copy-sexp-to-mark)
  

