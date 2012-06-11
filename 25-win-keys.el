(require 'windmove)


(autoload 'toggle-one-window "window-extension"
  "Toggle between window layout and one window." t)
(autoload 'sticky-window-keep-window-visible "window-extension.el"
  "Insure the buffer associated with the current window stays visible." t)
(autoload 'sticky-window-delete-window "window-extension.el"
  "This is intended to be a replacement for `delete-window', but" t)

;;(unless (require 'window-extension nil t)  ;; `window-extension' already contain the functions of `sticky-windows'
;;    (require 'sticky-windows nil t))
;;(require 'dedicated) ;;A very simple minor mode for dedicated buffers

(autoload 'windresize "windresize" nil t)

;;(require 'transpose-frame nil t) ;; flip window layout within a frame
(autoload 'transpose-frame "transpose-frame"
  "Transpose windows arrangement at FRAME." t)
(autoload 'flip-frame "transpose-frame"
  "Flip windows arrangement vertically at FRAME." t)
(autoload 'flop-frame "transpose-frame"
  "Flop windows arrangement horizontally at FRAME." t)
(autoload 'rotate-frame "transpose-frame"
  "Rotate windows arrangement 180 degrees at FRAME." t)

;;(require 'window-numbering nil t) ;; each window has a number in mode-line
;;(autoload 'window-numbering-mode "window-numbering"
;; "A minor mode that assigns a number to each window" t)

;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.

;;(require 'split-root) ;; roote window splitter
(autoload 'split-root-window "split-root"
  "Split a window of SIZE lines/columns from the root window." t)

(autoload 'tabbar-forward-tab     "tabbar" nil t)
(autoload 'tabbar-backward-tab    "tabbar" nil t)


;; for some special buffer
;;(require 'imenu-tree nil t)
(autoload 'imenu-tree "imenu-tree" "Display tree view of imenu." t)
(autoload 'tags-tree "tags-tree" "Display tree view of tags." t)

;;(require 'sr-speedbar nil t)
(autoload 'sr-speedbar-toggle "sr-speedbar" "Toggle sr-speedbar window" t)
(autoload 'sr-speedbar-open   "sr-speedbar" "Open the sr-speedbar window" t)

;;(require 'nav)  ;; emacs-nav
(autoload 'nav "nav" "Runs nav-mode in a narrow window on the left side" t)



;;{{{ window resizing

(defcustom win-resize-big-steps-horizontal 5
  "size by columns each time we call enlarge/shrink-window-horizontally-more.")

(defcustom win-resize-big-steps-vertical 3
  "size by rows each time we call enlarge/shrink-window-more.")

(defun enlarge-window-2d (arg)
  (interactive "p")
  (enlarge-window arg)
  (enlarge-window-horizontally arg))
  
(defun shrink-window-2d (arg)
  (interactive "p")
  (shrink-window arg)
  (shrink-window-horizontally arg))

;;(global-set-key (kbd "<f11> +") 'enlarge-window-2d)
;;(global-set-key (kbd "<f11> -") 'shrink-window-2d)

(defun enlarge-window-vertically-more (arg)
  (interactive "p")
  (enlarge-window (* arg win-resize-big-steps-vertical)))

(defun shrink-window-vertically-more (arg)
  (interactive "p")
  (shrink-window (* arg win-resize-big-steps-vertical)))

(defun enlarge-window-horizontally-more ()
  (interactive)
  (enlarge-window win-resize-big-steps-horizontal))

(defun shrink-window-horizontally-more ()
  (interactive)
  (shrink-window win-resize-big-steps-horizontal))

(defun enlarge-window-2d-more ()
  "enlarge window in both 2 dimensions"
  (interactive)
  (enlarge-window win-resize-big-steps-vertical)
  (enlarge-window-horizontally win-resize-big-steps-horizontal))

(defun shrink-window-2d-more ()
  "shrink window in both 2 dimensions"
  (interactive)
  (shrink-window win-resize-big-steps-vertical)
  (shrink-window-horizontally win-resize-big-steps-horizontal))

(autoload 'widen-current-window "widen-window"
  "The very function which resizes the current window." t)

(autoload 'maximize-frame "maxframe" "Maximizes the frame to fit the display." t)

;;}}}


;;--- misc

(defun split-root-window-vertially ()
  (interactive)
  (split-root-window nil nil))

(defun split-root-window-horizontally ()
  (interactive)
  (split-root-window nil 'horizontally))

;; keys


(defun init-win-fns-keys (map)
    ;; resizing
    ;(define-key map (kbd "}")   'enlarge-window)
    (define-key map (kbd "}")    'enlarge-window-more)
    ;;(define-key map (kbd "{")  'shrink-window)
    (define-key map (kbd "{")    'shrink-window-more)

    ;;(define-key map (kbd "^")  'enlarge-window-horizontally)
    (define-key map (kbd "^")    'enlarge-window-vertically-more)
    ;;(define-key map (kbd "v")  'shrink-window-horizontally)
    (define-key map (kbd "v")    'shrink-window-vertically-more)

    (define-key map (kbd "+")    'enlarge-window-2d-more)
    (define-key map (kbd "-")    'shrink-window-2d-more)

    (define-key map (kbd "b")    'balance-windows)
    (define-key map (kbd "m")    'minimize-window) ;; FIXME: only on Emacs 24?
    (define-key map (kbd "x")    'maximize-window)
    (define-key map (kbd "w")    'widen-current-window)

    (define-key map (kbd "RET")  'windresize)


    ;; motion between windows
    ;;(windmove-default-keybindings 'super)
    (define-key map (kbd "<up>")      'windmove-up)
    (define-key map (kbd "<down>")    'windmove-down)
    (define-key map (kbd "<left>")    'windmove-left)
    (define-key map (kbd "<right>")   'windmove-right)   

    (define-key map (kbd "<tab>")     'other-window)
    (define-key map (kbd "<S-tab>")   'other-window-backward)

    (define-key map (kbd "g w")       'ido-jump-to-window)
    (define-key map (kbd "g t")       'ido-jump-to-tab)
    (define-key map (kbd "g g")       'ido-jump-to-tab-group)

    ;; (if (featurep 'window-numbering)
    ;;     ;;FIXME: customize the keymap
    ;;     (window-numbering-mode t))

   ;;; move/swap window buffers
    (define-key map (kbd "<C-up>")    'move-buffer-up)
    (define-key map (kbd "<C-down>")  'move-buffer-down)
    (define-key map (kbd "<C-left>")  'move-buffer-left)
    (define-key map (kbd "<C-right>") 'move-buffer-right)

    (define-key map (kbd "<M-up>")    'swap-buffer-up)
    (define-key map (kbd "<M-down>")  'swap-buffer-down)
    (define-key map (kbd "<M-left>")  'swap-buffer-left)
    (define-key map (kbd "<M-right>") 'swap-buffer-right)

    (define-key map (kbd "M-m") 'ido-move-window-buffer-to)
    (define-key map (kbd "M-s") 'ido-swap-window-buffer-with)

    (define-key map (kbd "<M-backspace>") 'rotate-windows)

  ;;; window layout
    (define-key map (kbd "C-z") 'winner-undo)
    (define-key map (kbd "C-y") 'winner-redo)
    ;;TODO:
    ;; transpose-frame
    ;; flip-frame
    ;; flop-frame
    ;; rotate-frame
    ;; rotate-frame-clockwise
    ;; rotate-frame-anti-clockwise

  ;;; the following need some 3rd-party library
  ;;; window-extensions.el
    (define-key map (kbd "<f11>")     'toggle-one-window)
    (define-key map (kbd "*")         'sticky-window-keep-window-visible)

;;    (define-key map (kbd "M-RET")     'maximize-frame)
;;    (define-key map (kbd "ESC M-RET") 'restore-frame)
    (define-key map (kbd "M-RET")     'toggle-full-screen)

    (define-key map (kbd "C-x 1 h")   'delete-other-windows-horizontally+)
    (define-key map (kbd "C-x 1 v")   'delete-other-windows-vertically)
    
    ;; split-root.el
    (define-key map (kbd "C-x 2")     'split-root-window-vertially)
    (define-key map (kbd "C-x 3")     'split-root-window-horizontally)

    ;;( (featurep 'tabbar)
    (define-key map (kbd "C-n")       'tabbar-forward-tab)
    (define-key map (kbd "C-p")       'tabbar-backward-tab)

    ;;
    ;; some special windows
    ;;(when (featurep 'imenu-tree)
    (define-key map (kbd "I")           'imenu-tree)
    (define-key map (kbd "T")           'tags-tree)
    (define-key map (kbd "N")           'nav)

    ;;(when (featurep 'ide-skel)
    (define-key map (kbd "B")           'ide-skel-toggle-bottom-view-window)
    (define-key map (kbd "R")           'ide-skel-toggle-right-view-window)
    (define-key map (kbd "L")           'ide-skel-toggle-left-view-window)

;;    (define-key map (kbd "s") 'speedbar)
    ;;(if (featurep 'sr-speedbar)
    (define-key map (kbd "S")           'sr-speedbar-toggle)

    (define-key map "1"    'select-window-1)
    (define-key map "2"    'select-window-2)
    (define-key map "3"    'select-window-3)
    (define-key map "4"    'select-window-4)
    (define-key map "5"    'select-window-5)
    (define-key map "6"    'select-window-6)
    (define-key map "7"    'select-window-7)
    (define-key map "8"    'select-window-8)
    (define-key map "9"    'select-window-9)
    
    map
    )

(defvar bmz/win-fns-keymap (make-sparse-keymap "Window operations"))
(init-win-fns-keys bmz/win-fns-keymap)

(global-set-key (kbd "<f11>") bmz/win-fns-keymap)
(define-key global-map (kbd "C-c w") bmz/win-fns-keymap)
(global-set-key (kbd "s-w")   bmz/win-fns-keymap)
 
