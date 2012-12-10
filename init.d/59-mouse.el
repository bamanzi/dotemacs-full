;;* basic
(defmacro defun-mouse (newfunc function)
  `(defun ,newfunc (event)
     (interactive "e")
     (mouse-set-point event)
     (call-interactively ',function)))

;;** on Linux, wheel-up/-down is called mouse-4/-5
(define-key key-translation-map (kbd "<C-mouse-4>") (kbd "<C-wheel-up>"))
(define-key key-translation-map (kbd "<C-mouse-5>") (kbd "<C-wheel-down>"))
(define-key key-translation-map (kbd "<M-mouse-4>") (kbd "<M-wheel-up>"))
(define-key key-translation-map (kbd "<M-mouse-5>") (kbd "<M-wheel-down>"))
(define-key key-translation-map (kbd "<S-mouse-4>") (kbd "<S-wheel-up>"))
(define-key key-translation-map (kbd "<S-mouse-5>") (kbd "<S-wheel-down>"))
(define-key key-translation-map (kbd "<C-M-mouse-4>") (kbd "<C-M-wheel-up>"))
(define-key key-translation-map (kbd "<C-M-mouse-5>") (kbd "<C-M-wheel-down>"))


(define-key key-translation-map (kbd "<s-mouse-4>") (kbd "<s-wheel-up>"))
(define-key key-translation-map (kbd "<s-mouse-5>") (kbd "<s-wheel-down>"))
(define-key key-translation-map (kbd "<H-mouse-4>") (kbd "<H-wheel-up>"))
(define-key key-translation-map (kbd "<H-mouse-5>") (kbd "<H-wheel-down>"))



;;* without any modifier
(autoload 'highlight-symbol-at-point "highlight-symbol" "Toggle highlighting of the symbol at point." t)
(global-set-key (kbd "<double-mouse-1>") 'highlight-symbol-at-point)



;;* Ctrl:  code folding
(global-unset-key (kbd "<C-down-mouse-1>")) ;;moved to <mode-line>
(global-unset-key (kbd "<C-down-mouse-2>"))

;; (defun outline-toggle-children-by-mouse (event)
;;   (interactive "e")
;;   (mouse-set-point event)
;;   (call-interactively 'outline-toggle-children))
(defun-mouse outline-toggle-children-by-mouse outline-toggle-children)

(global-set-key (kbd "<C-wheel-up>")   'outline-previous-visible-heading)
(global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)

(global-set-key (kbd "<C-mouse-1>")    'outline-toggle-children-by-mouse)
(global-set-key (kbd "<C-mouse-2>")    'hide-sublevels)
(global-set-key (kbd "<C-mouse-3>")    'show-all)

(eval-after-load "allout"
  `(progn
     (define-key allout-mode-map (kbd "<C-wheel-up>")    'allout-previous-visible-heading)
     (define-key allout-mode-map (kbd "<C-wheel-down>")  'allout-next-visible-heading)

     (defun-mouse allout-toggle-current-subtree-exposure-by-mouse allout-toggle-current-subtree-exposure)
     (define-key allout-mode-map (kbd "<C-mouse-1>")     'allout-toggle-current-subtree-exposure-by-mouse)
     (define-key allout-mode-map (kbd "<C-mouse-3>")     'allout-show-all)
     ))


;;* Shift: some special marks overlays
(global-unset-key (kbd "<S-down-mouse-1>")) ;;mouse-appearance-menu moved to mode-line
(global-set-key (kbd "<S-down-mouse-3>") 'facemenu-menu) ;;moved from C-down-mouse-2

(global-set-key (kbd "<S-wheel-up>") 'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")  'highlight-symbol-next)

;; S-click to mark region, just as other editors' style
;;FROM: http://ignaciopp.wordpress.com/2009/06/17/emacs-indentunindent-region-as-a-block-using-the-tab-key/
(progn
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

  (define-key global-map [S-down-mouse-1] 'dave-shift-mouse-select)

  ;; to use in into emacs for  unix I  needed this instead
  (define-key global-map [S-mouse-1] 'dave-shift-mouse-select)
  )


;;* Meta
;;keep the original behaviour for secondary selection
;; M-mouse-1: mouse-drag-secondary
;; M-mouse-2: mouse-yank-secondary
;; M-mouse-3: mouse-secondary-save-then-kill


(define-key global-map [M-wheel-up] 'previous-error)
(define-key global-map [M-wheel-down]  'next-error)



;;* other modifiers: super/hyper/alt
(global-set-key (kbd "<C-M-wheel-up>")        'text-scale-increase)
(global-set-key (kbd "<C-M-wheel-down>")      'text-scale-decrease)


;;(global-set-key (kbd "<A-wheel-up>") 'highlight-symbol-prev)
;;(global-set-key (kbd "<A-wheel-down>")  'highlight-symbol-next)


;;; select rectangle using H-mouse-1 (FIXME: could it work?)
(require 'cua-rect)	
(defun bmz/mouse-mark-cua-rectangle (event)
  (interactive "e")
  (if (not cua--rectangle)	  
      (cua-mouse-set-rectangle-mark event)
    (cua-mouse-resize-rectangle event)))
(global-set-key (kbd "<H-mouse-1>") 'bmz/mouse-mark-cua-rectangle)
(define-key cua--rectangle-keymap (kbd "<H-mouse-1>") 'bmz/mouse-mark-cua-rectangle)



;;* left-fringe
;;** left-fringe without modifiers: hideshow
;;; refer `hideshow-fringe'
;;

;;** Ctrl on <left-fringe>: outline
;;; refer `qtmstr-outline'
;;(global-set-key (kbd "<left-fringe> <C-mouse-1>")   'outline-toggle-children-by-mouse)
;;(global-set-key (kbd "<left-fringe> <C-mouse-2>")   'show-all)

;;** Shift on <left-fringe> :  buffer-local bookmarks
(autoload 'bm-toggle-mouse  "bm" "Toggle a bookmark with a mouse click." t)
(autoload 'bm-previous-mouse  "bm" "Go to the previous bookmark with the scroll wheel." t)
(autoload 'bm-next-mouse  "bm" "Go to the next bookmark with the scroll wheel." t)

(progn
  (global-set-key (kbd "<left-fringe> <S-mouse-1>") 'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-up>") 'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-down>") 'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <S-mouse-2>") 'bm-show-all)
)

;;linemark.el from CEDET
;; see also: (@file :file-name "linemark.el" :to "enable-visual-studio-bookmarks")
(if nil 
    (progn
      (require 'linemark)
      (global-set-key (kbd "<left-fringe> <S-mouse-1>") 'viss-bookmark-toggle)
      (global-set-key (kbd "<left-fringe> <S-wheel-up>") 'viss-bookmark-next-buffer)
      (global-set-key (kbd "<left-fringe> <S-wheel-down>") 'viss-bookmark-prev-buffer))
    )




;;** Meta on <left-fringe> : empty

;;* mode-line
(global-set-key (kbd "<mode-line> <double-mouse-1>")   'widen-current-window)
(global-unset-key (kbd "<mode-line> <mouse-2>"))
(global-unset-key (kbd "<mode-line> <mouse-3>"))
;;** Ctrl on <mode-line>
(global-set-key (kbd "<mode-line> <C-down-mouse-1>")    'mouse-buffer-menu)

(global-set-key (kbd "<mode-line> <C-wheel-up>")        'text-scale-increase)
(global-set-key (kbd "<mode-line> <C-wheel-down>")      'text-scale-decrease)

;;** Shift on <mode-line>
(global-set-key (kbd "<mode-line> <S-down-mouse-1>") 'mouse-appearance-menu)
(global-set-key (kbd "<mode-line> <S-down-mouse-3>") 'facemenu-menu)

;;** mode-line parts
;;*** on which-func label
;;... (refer to prog-basic.el)

;;*** on buffer-name label
;;... (refer to buffers.el)
;;(setq mouse-buffer-menu-maxlen (- (frame-parameter nil 'height) 10))
(define-key mode-line-buffer-identification-keymap
  (kbd "<mode-line> <C-mouse-3>") 'mouse-buffer-menu)

;;* other stuff

;;** FIXME: markerpen.el
(autoload 'markerpen-show-all-pens "markerpen" "Display a buffer with samples of the markerpens in use" t)
;;(global-set-key (kbd "<S-mouse-1>")      'markerpen-mark-start-point-by-mouse)
;;(global-set-key (kbd "<S-mouse-3>")      'makrerpen-mark-end-point-by-mouse)
;;(global-set-key (kbd "<S-mouse-2>")      'markerpen-show-all-pens)
;;(global-set-key (kbd "<S-wheel-up>")      'makrerpen-previouse-pen)
;;(global-set-key (kbd "<S-wheel-down>")      'makrerpen-next-pen)

(idle-require 'mouse3)


;;** super+click to jump to declaration/implementation
(defun symbol-jump-on-mouse-down (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ( (begin  (save-excursion (re-search-backward "\\_<") (point)))
          (end    (save-excursion (re-search-forward  "\\_>") (point)))
          (symbol (buffer-substring begin end))
          (ov     (make-overlay begin end)) )
    (overlay-put ov 'category "linkify-imenu")
    (overlay-put ov 'face '(:underline t)    )
  ))

(defun symbol-jump-remove-overlay (point)
  (let ( (overlays (overlays-at (point))) )
    (mapcar '(lambda (ov)
               (if (string= (overlay-get ov 'category) "linkify-imenu")
                   (delete-overlay ov)))
            overlays
            )
         ))

(defun symbol-jump-on-mouse-up-imenu ()
  (interactive )
  (symbol-jump-remove-overlay (point))
  (imenu (thing-at-point 'symbol))
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point)))
  )

(defun symbol-jump-on-mouse-up-among-files ()
  (interactive )
  (symbol-jump-remove-overlay (point))
  (let ( (symbol (thing-at-point 'symbol)) )
    ;; (message "go to symbol: %s" symbol)
    (cond
     ( (memq major-mode '(emacs-lisp-mode lisp-interaction-mode help-mode))
       (if (fboundp (intern symbol))
           (find-function-at-point)
         (find-variable-at-point)))
     ( (and 'semantic-mode
            (memq major-mode '(c-mode java-mode python-mode)))
       (call-interactively 'semantic-complete-jump) )
     (t
      (call-interactively 'find-tag)))))

(define-key global-map (kbd "<s-down-mouse-1>") 'symbol-jump-on-mouse-down)
(define-key global-map (kbd "<s-mouse-1>")      'symbol-jump-on-mouse-up-imenu)
(define-key global-map (kbd "<C-s-mouse-1>")    'symbol-jump-on-mouse-up-among-files)
(define-key global-map (kbd "<s-mouse-3>")      'pop-to-mark-command)
(define-key global-map (kbd "<C-s-mouse-3>")    'pop-global-mark)






