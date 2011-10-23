;;* window operations
;;** winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;** adjust `split-window', so that new window not 1/2 in size, but 1/3

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


;;** tabbar
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-ske'
;; as this would mess up the tab group definition of `ide-skel'
(when (or (featurep 'tabbar)
          (load "tabbar" t))
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )


;;** window switching
;;*** M-1, M-2 to go to different window
(autoload 'window-numbering-mode "window-numbering" "A minor mode that assigns a number to each window" t)
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
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

;;** tabbar-mode
;;(unless (require 'ide-skel nil t)  ;; `ide-skel' would load `tabbar' and  make its own tab group settings
;;        (require 'tabbar nil t))
(when (require 'tabbar nil t)
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )

;;(when (featurep 'tabbar)
(defun ido-jump-to-tab ()
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




;;*** tabbar-rules
;;;.....

;;** popwin
;;;TODO: ?


;;** misc
;;(require 'pack-windows) ;; Resize all windows to display as much info as possible.


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

