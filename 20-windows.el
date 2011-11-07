;;* window operations
;;See also (linkd-follow '(@file :file-name "25-win-fns.el" :to "init-win-fns-keys" :display "window-related keybindings"))

;;** window layout
;;*** winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;*** common used layouts
;;TODO


;;** resizing windows
;;*** windresize.el
(autoload 'windresize "windresize" "Resize windows interactively." t)
;;(global-set-key (kbd "<f11> RET") 'windresize)

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

(ad-activate 'split-window)



;;** window switching
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

;;*** misc
;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.


;;** window buffer swapping

;;*** two windows
;;{{{ swap buffer window
;; modified from windmove-do-window-select
(defun windmove-do-swap-window (dir swap &optional arg window)
  "Move the buffer to the window at direction DIR.

If SWAP is non-nil, the buffers in the source window and target
window would be swapped, otherwise only the source buffer be
moved to target window.  DIR, ARG, and WINDOW are handled as by
`windmove-other-window-loc'.  If no window is at direction DIR,
an error is signaled."
  (let ((other-window (windmove-find-other-window dir arg window)))
    (cond ((null other-window)
           (error "No window %s from selected window" dir))
          ((and (window-minibuffer-p other-window)
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
;;(require 'transpose-frame nil t) ;; flip window layout within a frame
(autoload 'transpose-frame "transpose-frame"  "Transpose windows arrangement at FRAME." t)
(autoload 'flip-frame "transpose-frame" "Flip windows arrangement vertically at FRAME." t)
(autoload 'flop-frame "transpose-framer" "Flop windows arrangement horizontally at FRAME." t)
(autoload 'rotate-frame "transpose-frame" "Rotate windows arrangement 180 degrees at FRAME." t)


;;** tabbar
;; (find-library "tabbar")

;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-ske'
;; as this would mess up the tab group definition of `ide-skel'
(when (require 'tabbar nil t)
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward-tab)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward-tab)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )

;;(when (featurep 'tabbar)
(defun ido-jump-to-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (require 'tabbar)
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (mapcar '(lambda (tab) (buffer-name (tabbar-tab-value tab)))
					  (tabbar-tabs (tabbar-current-tabset t))))
         (buffer-name (ido-completing-read "Buffer: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (switch-to-buffer buffer-name))))

(defun ido-jump-to-tab-group ()
  "Jump to a tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) 
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs tabbar-tabsets-tabset)))
          (group-name (ido-completing-read "Groups: " groups)) )
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group))))
          (tabbar-tabs tabbar-tabsets-tabset))))

;;*** Add a buffer modification state indicator in the label
;;FROM: http://www.emacswiki.org/emacs/TabBarMode#toc11

(eval-after-load "tabbar"
  `(progn
     ;; add a buffer modification state indicator in the tab label,
     ;; and place a space around the label to make it looks less crowd
     (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
       (setq ad-return-value
             (if (and (buffer-modified-p (tabbar-tab-value tab))
                      (buffer-file-name (tabbar-tab-value tab)))
                 (concat " + " (concat ad-return-value " "))
               (concat " " (concat ad-return-value " ")))))

     ;; called each time the modification state of the buffer changed
     (defun ztl-modification-state-change ()
       (tabbar-set-template tabbar-current-tabset nil)
       (tabbar-display-update))
     ;; first-change-hook is called BEFORE the change is made
     (defun ztl-on-buffer-modification ()
       (set-buffer-modified-p t)
       (ztl-modification-state-change))

     (add-hook 'after-save-hook 'ztl-modification-state-change)
     ;; this doesn't work for revert, I don't know
     ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
     (add-hook 'first-change-hook 'ztl-on-buffer-modification)
     ))

;;*** tabbar-rules
;;;.....


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

;;*** opening server files always in a new frame
;;http://www.emacswiki.org/emacs/EmacsClient#toc21

(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))

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
(global-set-keyy (kbd "<f11> *") 'sticky-window-keep-window-visible)
(eval-after-load "window-extension"
  `(progn
        (global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
        (global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)))


