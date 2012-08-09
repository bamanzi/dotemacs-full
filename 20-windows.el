;;* window operations
;;See also (linkd-follow '(@file :file-name "25-win-fns.el" :to "init-win-fns-keys" :display "window-related keybindings"))

(defvar bmz/win-fns-keymap (make-sparse-keymap "Window operations"))

;;** window layout

;; if a window is narrower/lower than this number,
;; don't split horizontally/vertically automatically (`split-window-sensibly')
(setq split-width-threshold 140
      split-height-threshold 60)

;;*** common used layouts
;;DOC: http://www.emacswiki.org/emacs/ThreeWindows

;;*** my default layout


(defun bmz/default-frame-layout-3 (&optional frame)
   "
+---------+------------+ 
|         |            | 
|         |            | 
|         |            |
|         |------------+
|         |            |
|         |            | 
+---------+------------+  "
   (interactive (list (selected-frame)))
   (unless frame (setq frame (selected-frame)))
   (with-selected-frame frame
     (delete-other-windows)
     (split-window-horizontally)   (next-buffer)
     ;;(enlarge-window (/ (window-width) 5) 'horizontal)
     (other-window 1)
     (split-window-vertically)
     (enlarge-window (/ (window-height) 3) nil)
     (switch-to-buffer "*scratch*")
     ))

(defun bmz/default-frame-layout-4 (&optional frame)
   "
+------------+--------+ 
|            |        | 
|            |        | 
|            |        |
|----------+----------+
|          |          |
|          |          | 
+----------+----------+  "
   (interactive (list (selected-frame)))
   (unless frame (setq frame (selected-frame)))
   (with-selected-frame frame
     (delete-other-windows)
     (split-window-vertically)
     (enlarge-window (/ (window-height) 3))
     (save-selected-window
       (split-window-horizontally) (enlarge-window 20 t)
       (other-window 1) (next-buffer))
     (other-window 2)
     (progn
       (split-window-horizontally)
       (enlarge-window (/ (window-height) 3) t) (next-buffer)
       (other-window 1) (switch-to-buffer "*scratch*"))
     ))

(add-hook 'window-setup-hook 'bmz/default-frame-layout-3)
;;(add-hook 'after-make-frame-functions 'bmz/default-frame-layout)

;;*** layout save & restore
;;TIP: C-x r w  window-configuration-to-register
;;TIP: C-x r j  jump-to-register (could be used to restore window layout)

;;*** winner-mode: history of your window laytous
(setq winner-dont-bind-my-keys t)
(setq winner-ring-size 20)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)


;;** resizing windows
;;*** windresize.el: manual resizing
(autoload 'windresize "windresize" "Resize windows interactively." t)
(setq windresize-default-increment 4)
(define-key bmz/win-fns-keymap (kbd "RET") 'windresize)

;;*** widen-window: automatically widen
(autoload 'widen-current-window "widen-window"
  "The very function which resizes the current window." t)
(autoload 'widen-window-mode "widen-window"
  "Widen Window mode" t)
(autoload 'global-widen-window-mode  "widen-window"
  "Toggle Widen-Window mode in every possible buffer." t)

;;(idle-require 'widen-window)
;; (eval-after-load "widen-window"
(setq ww-ratio 0.75)
;;   `(global-widen-window-mode t)
;;   )

(defun widen-current-window-by-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (call-interactively 'widen-current-window))
(define-key bmz/win-fns-keymap (kbd "<mouse-1>") 'widen-current-window-by-mouse)

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
(defun bmz/split-window-vertically ()
  (interactive)
  (let ( (old-window (selected-window))
	 (old-size (window-height (selected-window))) )
    (split-window old-window nil nil)
    (with-selected-window old-window
      (enlarge-window (/ old-size 6) nil))))

(defun bmz/split-window-horizontally ()
  (interactive)
  (let ( (old-window (selected-window))
	 (old-size (window-height (selected-window))) )
    (split-window old-window nil 'horizontal)
    (with-selected-window old-window
      (enlarge-window (/ old-size 6) 'horizontal))))

(global-set-key [remap split-window-vertically]   'bmz/split-window-vertically)
(global-set-key [remap split-window-horizontally] 'bmz/split-window-horizontally)


;;*** misc
;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.


;;** window switching
(defun other-window+ ()
  "similar to `other-window', but ignore special buffer."
  (interactive)
  (let* ( (this-window (selected-window))
          (that-window (next-window this-window nil)) )
    (while (and that-window
                (not (eq this-window that-window)))
      (if (or (window-dedicated-p that-window)
              (let ((bufname (buffer-name (window-buffer that-window))))
                (or (member bufname  special-display-buffer-names)
                    (eq (aref bufname 0) ?*))))
          (setq that-window (next-window that-window nil))
        (progn
          (message "%s" that-window)
          (select-window that-window)
          (setq that-window nil))))))

(global-set-key (kbd "C-x O") 'other-window+)

;;*** windmove: move by direction
(idle-require 'windmove)
(windmove-default-keybindings 'super)


;;*** jump by number: M-1, M-2 to go to different window
(autoload 'window-numbering-mode "window-numbering"
  "A minor mode that assigns a number to each window" t)

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

(eval-after-load "window-numbering"
  `(progn
     ;; make window number be compatible with ECB
     (defun window-numbering-get-number-string (&optional window)
       (concat "W" (int-to-string (window-numbering-get-number window)) " "))
     
    (define-key window-numbering-keymap "\M-0" nil)
    (define-key window-numbering-keymap "\M-1" nil)
    (define-key window-numbering-keymap "\M-2" nil)
    (define-key window-numbering-keymap "\M-3" nil)
    (define-key window-numbering-keymap "\M-4" nil)
    (define-key window-numbering-keymap "\M-5" nil)
    (define-key window-numbering-keymap "\M-6" nil)
    (define-key window-numbering-keymap "\M-7" nil)
    (define-key window-numbering-keymap "\M-8" nil)
    (define-key window-numbering-keymap "\M-9" nil)

    (define-key bmz/win-fns-keymap "\M-0" 'select-window-0)
    (define-key bmz/win-fns-keymap "\M-1" 'select-window-1)
    (define-key bmz/win-fns-keymap "\M-2" 'select-window-2)
    (define-key bmz/win-fns-keymap "\M-3" 'select-window-3)
    (define-key bmz/win-fns-keymap "\M-4" 'select-window-4)
    (define-key bmz/win-fns-keymap "\M-5" 'select-window-5)
    (define-key bmz/win-fns-keymap "\M-6" 'select-window-6)
    (define-key bmz/win-fns-keymap "\M-7" 'select-window-7)
    (define-key bmz/win-fns-keymap "\M-8" 'select-window-8)
    (define-key bmz/win-fns-keymap "\M-9" 'select-window-9)

    (defun bmz/select-window-by-number (n)
      (interactive "NWindow Number: ")
      (select-window-by-number n))
    
    (define-key bmz/win-fns-keymap "G" 'bmz/select-window-by-number)
    ))

(eval-after-load "window-number"
  `(progn
     ;; <f11> 1 : go to first window
     (window-number-define-keys bmz/win-fns-keymap "")
     ))    

;;*** jump by buffer name
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

(define-key global-map (kbd "M-g w") 'ido-jump-to-window)

;;** window buffer swapping
(defun move-buffer-to-largest-window()
  "Open current buffer in largest window"
  (interactive)
  (let ((oldbuf (current-buffer)))
  (select-window (get-largest-window))
  (switch-to-buffer oldbuf))
)
(define-key bmz/win-fns-keymap (kbd "C-l") 'move-buffer-to-largest-window)


(defun swap-or-move-buffer-between-windows (other-window swap &optional this-window)
  "If SWAP, swap WINDOW's buffer with OTHER-WINDOW's.
Otherwise, move WINDOW's buffer to OTHER-WINDOW."
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

;;*** move/swap by window name
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

(define-key bmz/win-fns-keymap (kbd "M-m") 'ido-move-window-buffer-to)
(define-key bmz/win-fns-keymap (kbd "M-s") 'ido-swap-window-buffer-with)

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
     
     (define-key bmz/win-fns-keymap (kbd "M-m") 'move-buffer-to-numbered-window)
     (define-key bmz/win-fns-keymap (kbd "M-s") 'swap-buffer-with-numbered-window)
      
     ))

;;*** layout transformation (including window buffer)
;;(require 'transpose-frame nil t) ;; flip window layout within a frame
(autoload 'transpose-frame "transpose-frame"
  "Transpose windows arrangement at FRAME." t)
(autoload 'flip-frame      "transpose-frame"
  "Flip windows arrangement vertically at FRAME." t)
(autoload 'flop-frame      "transpose-framer"
  "Flop windows arrangement horizontally at FRAME." t)
(autoload 'rotate-frame    "transpose-frame"
  "Rotate windows arrangement 180 degrees at FRAME." t)
(autoload 'rotate-frame-clockwise      "transpose-frame"
  "Rotate windows arrangement 90 degrees clockwise at FRAME." t)
(autoload 'rotate-frame-anticlockwise  "transpose-frame"
  "Rotate windows arrangement 90 degrees anti-clockwise at FRAME." t)

;;*** rotate
;; https://github.com/banister/window-rotate-for-emacs
(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))


;;** managing special/temporary windows
;;*** framepop
;;(idle-require 'framepop)
(eval-after-load "framepop"
  `(progn
     (framepop-enable)
     (setq special-display-regexps '("*Shell Command Output*"
                                     "*anything .\\*"
                                     "*grep*"
                                     "*compilation*"))
     ))
;;*** popwin
(autoload 'popwin:popup-buffer "popwin"
  "Show BUFFER in a popup window and return the popup window." t)
(autoload 'popwin:display-buffer  "popwin"
  "Display BUFFER-OR-NAME, if possible, in a popup window, or as" t)
(autoload 'popwin:messages  "popwin"
  "Display *Messages* buffer in a popup window." t)

;;(idle-require 'popwin)
(eval-after-load "popwin"
  `(progn
;;     (setq display-buffer-function 'popwin:display-buffer)
     
;;   (push '("*anything recentf*" :width 30 :position right) popwin:special-display-config)
     (let ((width (round (* (frame-parameter nil 'width) 0.38))))
       (push `("\*anything .*" :regexp t :width ,width :position right) popwin:special-display-config))
     
     (define-key bmz/win-fns-keymap (kbd "~") popwin:keymap)

     (define-key popwin:keymap "g" 'popwin:select-popup-window) ;;go to
     (define-key popwin:keymap "*" 'popwin:stick-popup-window)
     (define-key popwin:keymap "~" 'popwin:display-last-buffer)
     ))

(define-minor-mode popwin-mode
  "Use `popwin:display-buffer' to manage buffer window creation.

When turned on, special buffers (specified in `popwin:special-display-config')
would be displayed in a popup window."
  :init-value nil
  :lighter " POP"
  :global t
  :require 'popwin
  (if popwin-mode
      (setq display-buffer-function 'popwin:display-buffer)
    (setq display-buffer-function nil))) ;;FIXME: restore old value (rather than 'nil')

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
(define-key bmz/win-fns-keymap (kbd "*") 'sticky-window-keep-window-visible)

(eval-after-load "window-extension"
  `(progn
        (global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
        (global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)))


;;** force `info' showing in another frame
;;stolen from http://dotat.at/prog/rcfiles/emacs
(defun Info-new-frame (&optional file)
  "Start *info* mode in another frame."
  (interactive (if current-prefix-arg
                   (list (read-file-name "Info file name: " nil nil t))))
  (let ((pop-up-frames t)
        (buffer (current-buffer)))
    (pop-to-buffer buffer t)
    (raise-frame (window-frame (selected-window)))
    (info file)))

;;*** another implementation
(defvar special-display-buffer-other-frame-regexps
  '("*info*")
  "The buffer names that would be forced to display in another frame.")
 
(defvar display-buffer-function-orig nil
  "Old value of `display-buffer-function'.")
   
(defun display-buffer-use-other-frame-first  (buffer &optional other-window frame)
  "A `display-buffer-function' implementation.
This one would force using other frame (if none, this would create a new one)
to display some special buffers specified in `. For non-special"
  (let* ((buffer-name (buffer-name buffer))
         (use-other-frame   (catch 'found
                              (dolist (regexp special-display-buffer-other-frame-regexps)
                                (cond
                                 ((stringp regexp)
                                  (when (string-match-p regexp buffer-name)
                                    (throw 'found t)))
                                 ((and (consp regexp) (stringp (car regexp))
                                       (string-match-p (car regexp) buffer-name))
                                  (throw 'found (cdr regexp)))))))
         frame
         window)
    ;;(message "use-other-frame=%s" use-other-frame)
    (if (and use-other-frame (display-graphic-p))
        (progn
          (setq frame (if (eq (selected-frame) (next-frame))
                          (make-frame)
                        (next-frame)))
          (setq window (car (window-list frame)))
          (set-window-buffer window buffer)
          window)
      (let ((display-buffer-function display-buffer-function-orig)) ;;Emacs default
        (display-buffer buffer other-window frame)))))

(defvar info-other-frame nil
  "Whether to show info in another frame.

        Dont' change it, it is used internally in `info' advice
        & `info-other-frame'.")

(defadvice info (around ad-info-other-frame)
  ;;In current Emacs's implementation of `display-buffer',
  ;;`special-display-function' is too late for special buffers.
  ;;I have to override `display-buffer' temporarily.
  (setq display-buffer-function-orig  display-buffer-function)
  (let ((display-buffer-function
         (if info-other-frame  ;;FIXME: remove this (but now sometimes it won't work properly)
             'display-buffer-use-other-frame-first
           display-buffer-function))
        (after-make-frame-functions   '()))
    ad-do-it
    ))

(defun info-other-frame ()
  (interactive)
  (let ((info-other-frame t)
        (after-make-frame-functions   '()))
    (call-interactively 'info)))

(defun info-view-file-other-frame (file)
  (interactive (list (read-file-name "Info file name: " nil nil t)))
  (info-other-frame file))

;;for testing
(ad-enable-advice 'info 'around 'ad-info-other-frame)
;;(ad-deactivate 'info)
;;(ad-activate 'info)
;; (info "(emacs)Top")

;;** frame

;;*** maximize frame
(when (and window-system
           (or (require 'maxframe nil t)
               (require 'fit-frame nil t)))
           
  ;; (setq mf-max-width 1600)  ;; Pixel width of main monitor.
  (maximize-frame)
  ;; maximize any new frame
  (add-hook 'window-setup-hook 'maximize-frame t)

  (defun maximize-frame-if-splittable (frame)
    (interactive (list (selected-frame)))
    (unless (frame-parameter frame 'unsplittable)
      (with-selected-frame frame
        (if (fboundp 'maximize-frame)
            (maximize-frame)))))

  (add-hook 'after-make-frame-functions 'maximize-frame-if-splittable)
  )

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
          ;; https://bitbucket.org/alexander_manenko/emacs-fullscreen-win32
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

(define-key bmz/win-fns-keymap (kbd "M-RET") 'toggle-full-screen)

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

(defun focus-or-switch-to-buffer (buffer)
  "If buffer currently shows in a window of current frame, focus it.
Otherwise call `switch-to-buffer'."
  (interactive "bBuffer: ")
  (let ((win (find-if (lambda (win)
                        (string= buffer
                                 (buffer-name (window-buffer win))))
                      (window-list))))
    (if win
        (select-window win)
      (switch-to-buffer buffer))))

(define-key global-map (kbd "M-g b") 'focus-or-switch-to-buffer)

;;*** some useful extensions
;;- toggle-one-window
;;- sticky window
;;- delete-other-window-horizontal+
(idle-require 'window-extension)


