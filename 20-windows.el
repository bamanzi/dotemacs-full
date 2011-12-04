;;* window operations
;;See also (linkd-follow '(@file :file-name "25-win-fns.el" :to "init-win-fns-keys" :display "window-related keybindings"))

;;** window layout
;;TIP: C-x r w  window-configuration-to-register
;;TIP: C-x r j  jump-to-register (could be used to restore window layout)

;;*** winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;*** common used layouts
;;TODO

;;*** layout transformation
;;(require 'transpose-frame nil t) ;; flip window layout within a frame
(autoload 'transpose-frame "transpose-frame"  "Transpose windows arrangement at FRAME." t)
(autoload 'flip-frame "transpose-frame" "Flip windows arrangement vertically at FRAME." t)
(autoload 'flop-frame "transpose-framer" "Flop windows arrangement horizontally at FRAME." t)
(autoload 'rotate-frame "transpose-frame" "Rotate windows arrangement 180 degrees at FRAME." t)


;;** resizing windows
;;*** windresize.el
(autoload 'windresize "windresize" "Resize windows interactively." t)
(global-set-key (kbd "<f11> RET") 'windresize)

;;*** adjust `split-window', so that new window not 1/2 in size, but 1/3

;;; this works only on Emacs 24
;; (defadvice split-window-above-each-other (around make-new-win-one-third)
;;     (let ( (old-window (selected-window))
;;            (old-size (window-height (selected-window))) )
;;       ad-do-it
;;       (window-resize old-window (/ old-size 6) nil)))

;; (ad-activate 'split-window-above-each-other)

;; (defadvice split-window-side-by-side (around make-new-win-one-third)
;;     (let ( (old-window (selected-window))

;;            (old-size (window-width (selected-window))) )
;;       ad-do-it
;;       (window-resize old-window (/ old-size 6) 'horizontal)))

;; (ad-activate 'split-window-side-by-side)

;;NOTE: this might cause some problems
;;; This works on both emacs 23 & 24 
(defadvice split-window (around make-new-win-one-third)
    (let* ( (old-window (selected-window))
            (horizontal (ad-get-arg 2))
            (old-size (if horizontal
                          (window-width old-window)
                        (window-height old-window))) )
      ad-do-it
      (with-selected-window old-window
        (enlarge-window (/ old-size 10) horizontal))))

;;(ad-enable-advice 'split-window 'around 'make-new-win-one-third)

;;**** this one is safer
(defun split-window-vertically+ ()
  (interactive)
  (let ( (old-window (selected-window))
	 (old-size (window-height (selected-window))) )
    (split-window old-window nil nil)
    (window-resize old-window (/ old-size 6) nil)))

(defun split-window-horizontally+ ()
  (interactive)
  (let ( (old-window (selected-window))
	 (old-size (window-height (selected-window))) )
    (split-window old-window nil 'horizontal)
    (window-resize old-window (/ old-size 6) nil)))

(global-set-key [remap split-window-vertically]   'split-window-vertically+)
(global-set-key [remap split-window-horizontally] 'split-window-horizontally+)


;;*** misc
;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.


;;** window switching
;;*** windmove: move by direction
(idle-require 'windmove)
(windmove-default-keybindings 'super)


;;*** M-1, M-2 to go to different window
(autoload 'window-numbering-mode "window-numbering" "A minor mode that assigns a number to each window" t)
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix. "
  t)
(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  t)

(condition-case nil
  (window-numbering-mode t)
  (window-numer-meta-mode t))


(defun ido-jump-to-window ()
  "Jump to window by current buffer name."
  (interactive)
  (defun swap(l)
    (if (cdr l)
	(cons (cadr l) (cons (car l) (cddr l)))
      l))
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (swap (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
                (delq nil (mapcar '(lambda (window)
                                       (if (equal buffer-name (buffer-name (window-buffer window)))
                                           window
                                         nil))
                                  (window-list))))
      (select-window (car window-of-buffer)))))



;;** window buffer swapping
(defun swap-or-move-buffer-between-windows (other-window swap &optional this-window)
  (let ( (window (or this-window
                     (selected-window))) )
    (cond ( (null other-window)
            (error "No window %s from selected window" dir))
          ( (and (window-minibuffer-p other-window)
                 (not (minibuffer-window-active-p other-window)))
            (error "Minibuffer is inactive"))
          ( (window-dedicated-p window)
            (error "Current window is dedicated, can't be moved") )
          ( (window-dedicated-p other-window)
            (error "Target window is dedicated, can't be swapped") )
          (t
           (let ( (this-buffer (window-buffer window))
                  (other-buffer (window-buffer other-window)))
             (if (eq this-buffer other-buffer)
                 (let ( (this-point (window-point window))
                        (other-point (window-point other-window)) )
                   (progn
                     (set-window-point window other-point)
                     (set-window-point other-window this-point)))
               (progn
                 (if swap
                     (set-window-buffer window (window-buffer other-window)))
                 (set-window-buffer other-window this-buffer)))
             (select-window other-window))))))


;;*** move/swap by direction (two windows)
;; modified from windmove-do-window-select
(defun windmove-do-swap-window (dir swap &optional arg window)
  "Move the buffer to the window at direction DIR.

If SWAP is non-nil, the buffers in the source window and target
window would be swapped, otherwise only the source buffer be
moved to target window.  DIR, ARG, and WINDOW are handled as by
`windmove-other-window-loc'.  If no window is at direction DIR,
an error is signaled."
  (let ((other-window (windmove-find-other-window dir arg window)))
    (swap-or-move-buffer-between-windows other-window swap window)))


(defun swap-buffer-up (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'up t arg))

(defun swap-buffer-down (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'down t arg))

(defun swap-buffer-left (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'left t arg))

(defun swap-buffer-right (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'right t arg))

(defun move-buffer-up (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'up nil arg))

(defun move-buffer-down (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'down nil arg))

(defun move-buffer-left (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'left nil arg))

(defun move-buffer-right (&optional arg)
  (interactive "P")
  (windmove-do-swap-window 'right nil arg))

;;*** swap/move by window name
(defun ido-move-or-swap-window-buffer (justmove)
  (let (windows)
    (mapc '(lambda (window)
             (if (and (not (window-dedicated-p window))
                      (not (eq window (selected-window))))
               (add-to-list 'windows (format "%s" window))))
          (window-list))
;;    (message "%s" (car (window-list)))
    (let ( (target-window (ido-completing-read
                           (if justmove
                               "Move window to: "
                             "Swap window with: ")
                           windows)) )
      (mapc '(lambda (window)
               (if (string= target-window (format "%s" window))
                   (swap-or-move-buffer-between-windows window (not justmove))))
            (window-list)))
    ))

(defun ido-swap-window-buffer-with ()
  "Swap the window's buffer with another window."
  (interactive)
  (ido-move-or-swap-window-buffer nil))

(defun ido-move-window-buffer-to ()
  (interactive)
  (ido-move-or-swap-window-buffer 'justmove))

(global-set-key (kbd "<f11> M-m") 'ido-move-window-buffer-to)
(global-set-key (kbd "<f11> M-s") 'ido-swap-window-buffer-with)

;;*** move/swap by window number
;;Seems not very useful?
(eval-after-load "window-numbering"
  `(progn
     (defun move-or-swap-buffer-to-numbered-window (arg swap)
       (let ((window (selected-window))
             (windows (car (gethash (selected-frame) window-numbering-table)))
             other-window)
         (if (and (>= arg 0) (< arg 10)
                  (setq other-window (aref windows arg)))
             (swap-or-move-buffer-between-windows other-window swap window)
           (error "No window numbered %s" i))))

     (defun move-buffer-to-numbered-window (arg)
       (interactive
        (list (- (read-char "move window buffer to (1-9): ") 48)))
       (message "Move %s to window %s." (buffer-name) arg)
       (move-or-swap-buffer-to-numbered-window arg nil))

     (defun swap-buffer-with-numbered-window (arg)
       (interactive
        (list (- (read-char "swap window buffer with(1-9): ") 48)))
       (message "Swap current window with window %s." arg)
       (move-or-swap-buffer-to-numbered-window arg 'swap))
     
     (global-set-key (kbd "<f11> M-m") 'move-buffer-to-numbered-window)
     (global-set-key (kbd "<f11> M-s") 'swap-buffer-with-numbered-window)
      
     ))

;;*** rotate
;; https://github.com/banister/window-rotate-for-emacs
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

;;*** other transformations




;;** temporary windows
;;*** framepop
;;;TODO: ?
;;*** popwin
;;;...

;;** frame

;;*** maximize frame
(when (and window-system
           (or (require 'maxframe nil t)
               (require 'fit-frame nil t)))
           
  ;; (setq mf-max-width 1600)  ;; Pixel width of main monitor.
  (maximize-frame)
  ;; maximize any new frame
  (add-hook 'window-setup-hook 'maximize-frame t))

;;*** full-screen (or maximize)
;;NOTE: this only works in X & Emacs > 23
(defun x-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;;    (global-set-key [f11] 'toggle-fullscreen)

(defun toggle-full-screen (arg)
  "Toggle frame full-screen or maximization."
  (interactive "P")
  (cond
   ( (and (eq window-system 'w32)             
          (locate-file "emacs_fullscreen.exe" exec-path nil 'file-executable-p))
     (shell-command "emacs_fullscreen.exe") )
   ( (and (display-graphic-p) (>= emacs-major-version 23))
     (x-toggle-fullscreen arg))
   (t
    (or (require 'maxframe nil t)
        (require 'fit-frame nil t))
    (if (fboundp 'maximize-frame)
        (if menu-bar-mode
            (call-interactively 'restore-frame)
          (call-interactively 'maximize-frame))
      (message "Failed to find a way to toggle full screen."))))
  
   (when arg
     (tool-bar-mode (not menu-bar-mode))
     (menu-bar-mode (not menu-bar-mode))
     (scroll-bar-mode (not menu-bar-mode)))
   )

(global-set-key (kbd "<f11> M-RET") 'toggle-full-screen)

;;*** opening server files always in a new frame
;;http://www.emacswiki.org/emacs/EmacsClient#toc21

(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

;;** multiple layouts management
;;*** elscreen
;;elscreen uses header-line, thus conflicting with tabbar

;;*** el-screen
;; https://github.com/medikoo/el-screen

;;*** escreen
;; http://www.emacswiki.org/emacs/EmacsScreen

;;*** perspective
;;

;;*** workgroups
;; 

;;** misc

;;*** some useful extensions
;;- toggle-one-window
;;- sticky window
;;- delete-other-window-horizontal+
(idle-require 'window-extension)


;;*** window dedicated to a buffer
;;TIP: 'dedicated' means this window won't be selected for displaying other buffer,
;;     (but switching manually is allowed)
;;(require 'dedicated) ;;A very simple minor mode for dedicated buffers
(autoload 'dedicated-mode "dedicated.el" "Toggle dedicated minor mode." t)

;; another simple way
;; `window-extension' already contain the functions of `sticky-windows'
;; (require 'sticky-windows nil t))

;; override C-x 0 and C-x 1, to regard window-dedicated-p
;;(when (or (featurep 'window-extensions)
;;          (featurep 'sticky-windows)))
(global-set-key (kbd "<f11> *") 'sticky-window-keep-window-visible)
(eval-after-load "window-extension"
  `(progn
        (global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
        (global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)))


