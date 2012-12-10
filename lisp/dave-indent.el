;;; dave-indent.el -- a simple indentation behaviour just other simple editors

;;
;;

;;
;; turn on transient mark mode
;;(that is, we highlight the selected text)
;(transient-mark-mode t)


(defun dave-indent-block()
  (shift-region tab-width)
  (setq deactivate-mark nil))

(defun dave-unindent-block()
  (shift-region (- tab-width))
  (setq deactivate-mark nil))

(defun dave-shift-region(numcols)
" my trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point)(mark))
      (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(defvar dave-indent-complete-function 'dabbrev-expand
  "The function for code completion used in `dave-indent-or-complete'.")
(make-variable-buffer-local 'dave-indent-complete-function)
                            
(defun dave-indent-or-complete ()
  "Indent region selected as a block; if no selection present either indent according to mode,
or expand the word preceding point. "
  (interactive)
  (if  mark-active
      (dave-indent-block)
    (if (looking-at "\\>")
        (call-interactively 'dave-indent-complete-function)
      (if indent-tabs-mode
          (insert "\t")
        (insert-char 32 tab-width)))))


(defun dave-unindent()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up
to the beginning of line.  Now it correctly stops at the
beginning of the line when the pointer is at the first char of an
indented line. Before the command would (unconveniently) kill all
the white spaces, as well as the last word of the previous line."
  (interactive)
  (if mark-active
      (dave-unindent-block)
    (unless(bolp)
      (if (looking-back "^[ \t]*")
          (backward-delete-char-untabify (min tab-width (current-column)))        
        (move-to-column (max 0 (- (current-column) tab-width)))))))

(if nil  ;;original implementation
(defun my-unindent()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up
to the beginning of line.  Now it correctly stops at the
beginning of the line when the pointer is at the first char of an
indented line. Before the command would (unconveniently) kill all
the white spaces, as well as the last word of the previous line."
  (interactive)
  (if mark-active
      (unindent-block)
    (progn
      (unless(bolp)
        (if (looking-back "^[ \t]*")
            (progn
              ;;"a" holds how many spaces are there to the beginning of the line
              (let ((a (length(buffer-substring-no-properties (point-at-bol) (point)))))
                (progn
                  ;; delete backwards progressively in my-tab-width steps, but without going further of the beginning of line.
                  (if (> a my-tab-width)
                      (delete-backward-char my-tab-width)
                    (backward-delete-char a)))))
          ;; delete tab and spaces first, if at least 2 exist, before removing words
          (progn
            (if(looking-back "[ \t]\\{2,\\}")
                (delete-horizontal-space)
              (backward-kill-word 1))))))))
)


(defvar dave-indent-mode-map
  (let ( (map (make-sparse-keymap)) )
    (define-key map "\t"        'dave-indent-or-complete) 
    (define-key map [S-tab]     'dave-unindent)
    map)
  "keymap for `dave-indent-mode'.")

(define-minor-mode dave-indent-mode
  "simple indent just like other editors."
  nil
  dave-indent-mode-map
  (if dave-indent-mode
      t
    t))
      

;; mac and pc users would like selecting text this way
(defun dave-shift-mouse-select (event)
  "Set the mark and then move point to the position clicked on with
 the mouse. This should be bound to a mouse click event type."
  (interactive "e")
  (mouse-minibuffer-check event)
  (if mark-active (exchange-point-and-mark))
  (set-mark-command nil)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (posn-set-point (event-end event)))

;; be aware that this overrides the function for picking a font. you can still call the command
;; directly from the minibufer doing: "M-x mouse-set-font"
(define-key global-map [S-down-mouse-1] 'dave-shift-mouse-select)

;; to use in into emacs for  unix I  needed this instead
;(define-key global-map [S-mouse-1] 'dave-shift-mouse-select)



(provide 'dave-indent)

