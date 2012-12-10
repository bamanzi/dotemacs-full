;;* tabbar & perspetive: grouping buffers & windows

;; (find-library "tabbar")

;;** ide-skel
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-skel'
;; as this would mess up the tab group definition of `ide-skel'

;;** tabbar
;;*** tabbar-mode basic
(unless (featurep 'tabbar)
  (require 'tabbar nil t))

(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward-tab)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-backward-tab)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-forward-group)
     (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
     ))

;;*** jump to tab/group, ido style
;;(when (featurep 'tabbar)
(defun ido-jump-to-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
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
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))  
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh groups
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs tabbar-tabsets-tabset)))
          (group-name (ido-completing-read "Groups: " groups)) )
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group))))
          (tabbar-tabs tabbar-tabsets-tabset))))


;;*** add a buffer modification state indicator in the label
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

;;*** buffer grouping functions

;; from Xah Lee
 (defun tabbar-buffer-groups-simple ()
   "Return the list of group names the current buffer belongs to.
 This function is a custom function for tabbar-mode's tabbar-buffer-groups.
 This function group all buffers into 3 groups:
 Those Dired, those user buffer, and those emacs buffer.
 Emacs buffer are those starting with “*”."
   (list
    (cond
     ((memq major-mode '(dired-mode shell-mode eshell-mode ibuffer-mode))
      "Utils"
      )
     ((string-equal "*" (substring (buffer-name) 0 1))
      "Emacs Buffer"
      )
     (t
      "User Buffer"
      )
     )))

(defun tabbar-group-by-simple ()
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-simple)
  )
;(fset 'bmz/tabbar-buffer-groups-function 'tabbar-buffer-groups-simple)
(tabbar-group-by-simple)

;;make some *useful* buffers group together
(defun tabbar-buffer-groups-by-folder ()
   (list
    (cond
     ( buffer-file-name
       (let ((list (split-string buffer-file-name "/")))
         (concat (nth (- (length list) 3) list)
                 "/"
                 (nth (- (length list) 2) list))))
         
     (t
        "Emacs Buffer"
      )
     )))

;;(setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-folder)


;;*** user groups
(defcustom tabbar-user-groups '("user1" "user2" "user3" "user4")
  "Group name for buffer grouping.")

(defvar tabbar-buffer-user-groups-alist '()
  "A list storing group names for each buffer.  (buffer-name . (group1 group2 ...)).")

(defun tabbar-add-user-group (name)
  (interactive "sGroup name: ")
  (if (member name tabbar-user-groups)
      (message "Group `%s' already exists.")
    (add-to-list 'tabbar-user-groups name)))

(defun tabbar-add-buffer-to-user-group (buffer group)
;;  (interactive "bBuffer: \nsGroup: ")
  (interactive (list
                (ido-read-buffer "Buffer: " (buffer-name))
                (ido-completing-read "Group: " tabbar-user-groups)))
  (let ((buffer-obj  (get-buffer buffer))
        (groups      (assoc buffer tabbar-buffer-user-groups-alist)))
    (when buffer-obj
      (add-to-list 'tabbar-user-groups group) ;;add group name
      (if groups
          (setcdr groups (cons group (cdr groups)))
        (add-to-list 'tabbar-buffer-user-groups-alist (list buffer group))))))        

(defun tabbar-buffer-groups-by-user-groups ()
  (let ((groups (assoc (buffer-name (current-buffer)) tabbar-buffer-user-groups-alist)))
    (append (cdr groups) (tabbar-buffer-groups-by-modes)))) ;;FIXME: tabbar.el supports one buffer in multi groups?

;;TODO: enable this
;;(setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-user-groups)

;;*** tabbar-ruler: add context menu to tabs
(if (and (not (featurep 'ide-skel))   ;;conflicting with idle-
         (display-graphic-p))   ;;won't work well in terminal(?)
    (idle-require 'tabbar-ruler))

(eval-after-load "tabbar-ruler"
  `(progn
     ;; restore my choice (tabbar-ruler.el would change this)
;;     (setq tabbar-buffer-groups-function 'bmz/tabbar-buffer-groups-function)
     (tabbar-group-by-simple)
     ))
     
;;*** show tabbar group on modeline
(setq mode-line-tabbar-group
      '(:eval (when (tabbar-mode-on-p)  ;;FIXME: not work?
                (concat (propertize (car (funcall tabbar-buffer-groups-function)) 
                                    'face 'tabbar-selected
                                    'help-echo "tabbar group")
                        " > "))))
;;FIXME: not work?
(eval-after-load "tabbar"
  `(progn
     (if (require 'bmz-misc nil t)
         (mode-line-install-element 'mode-line-tabbar-group 'mode-line-frame-identification))

     
     ))

;;*** some hacks on header-line
(setq bmz/tabbar-header-line-format
              '((:propertize "[O]"
                             local-map (keymap (header-line keymap
                                                            (mouse-1   . widen-current-window-by-mouse)
                                                            (C-mouse-1 . delete-other-windows)
                                                            (mouse-3   . tabbar-tabset-menu)
                                                            ))
                             help-echo "  mouse-1: widen current window
C-mouse-1: delete other windows
C-mouse-3: list buffers in current tabset")
                (:propertize "[X]"
                             local-map (keymap (header-line keymap
                                                            (mouse-1   . mouse-delete-window)
                                                            (C-mouse-1 . kill-buffer)
                                                            (S-mouse-1 . mouse-delete-other-windows)
                                                            (M-mouse-1 . mouse-tear-off-window)
                                                            (mouse-3 . tabbar-tabset-menu)))
                             help-echo "  mouse-1: delete current window
C-mouse-1: kill current buffer
C-mouse-1: delete other windows
M-mouse-1: tear of current window into a new frame
  mouse-3: list buffers of current tabset in menu")
;;                which-func-format
                (:eval (tabbar-line))))

(eval-after-load "tabbar"
  `(progn
     (setq tabbar-header-line-format bmz/tabbar-header-line-format)
     (setq-default header-line-format tabbar-header-line-format)
     ))

(defun widen-current-window-by-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (if (fboundp 'maximize-window) ;;Emacs 24 has 'maximize-window
      (call-interactively 'maximize-window)
    (if (require 'widen-window nil t)
        (call-interactively 'widen-current-window)
      (delete-other-windows))))
      
(defun tabbar-tabset-menu (event)
  "List all buffers of current tabset in a popup menu, let use choose to switch."
  (interactive "e")
  (let* ((buffers (mapcar '(lambda (tab) (tabbar-tab-value tab))
                          (tabbar-tabs (tabbar-current-tabset t))))
         (alist   (mouse-buffer-menu-alist buffers))
         (menu    (cons "Buffers in current tabset"
                        (mouse-buffer-menu-split "Select Buffer" alist)))
         (sel     (x-popup-menu event menu)))
    (if sel
        (switch-to-buffer sel))))

(eval-after-load "tabbar"
  `(progn
     ;;re-implement `tabbar-buffer-click-on-home'
     (defun tabbar-buffer-click-on-home (event)
       "Handle a mouse click EVENT on the tab bar home button.
mouse-1, toggle the display of tabs for groups of buffers.
mouse-3, list buffers in current tabset."
       (let ((mouse-button (event-basic-type event)))
         (cond
          ((eq mouse-button 'mouse-1)
           (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
          ((eq mouse-button 'mouse-3)
           (call-interactively 'tabbar-tabset-menu))
          )))
     ))


;;** perspective
;;`perspective' is more than window layout manager such as elscreen/escreen/workgroups.
;;it also manages buffers: you can add buffers to a perpective, `C-x b' switches among them.

;;DOC: Putting it in Perspective - http://nex-3.com/posts/72-putting-it-in-perspective
;;DOC: Workspaces EmacsRookie (about perspective.el) - http://emacsrookie.com/2011/09/25/workspaces/

(autoload 'persp-mode "perspective"
  "Toggle perspective mode." t)

;;(idle-require 'perspective)

(defun bmz/persp-mode-init ()
  (setq frame-title-format
        '((:eval (format "[%s]" (persp-name persp-curr)))
          " - Emacs "
          (:eval emacs-version)))

  ;; modify tabbar-mode settings
  (when (and (featurep 'tabbar) tabbar-mode)
    ;;hide buffers not in current perspecitve
    (defun tabbar-buffer-groups-only-current-persp ()
      (if (or (string= (persp-name persp-curr) "main")  ;;perspective `main' unhide all buffers
              (memq (current-buffer) (persp-buffers persp-curr)))
          (if (eq tabbar-buffer-groups-function-before-persp
                  'tabbar-buffer-groups-only-current-persp)
              (tabbar-buffer-groups) ;;default one in tabbar.el
            (funcall tabbar-buffer-groups-function-orig))
        nil))

    ;;change `tabbar-buffer-groups
    (defvar tabbar-buffer-groups-function-before-persp nil)
    (when (or (not tabbar-buffer-groups-function-before-persp)
              (not (eq tabbar-buffer-groups-function-before-persp
                        tabbar-buffer-groups-function)))
        (setq tabbar-buffer-groups-function-before-persp tabbar-buffer-groups-function)
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-only-current-persp))
    ))

(eval-after-load "perspective"
  `(progn
     (add-hook 'persp-mode-hook 'bmz/persp-mode-init)
     ;;(add-hook 'persp-activated-hook 'persp-set-icon)
     ))


;;*** frame-bufs: a lightweight perspective
;; with `frame-bufs.el', we can associate buffers to frames,
;; thus frame could be used as a perspective

(autoload 'frame-bufs-mode  "frame-bufs"
  "Toggle frame-bufs-mode on and off." t)

;(idle-require 'frame-bufs)

(defun frame-bufs-switch-buffer ()
  "Switch buffer, within buffers associated with current frame (`frame-bufs-buffer-list')

Other buffers are excluded."
  (interactive)
  (if (and (featurep 'frame-bufs)
           frame-bufs-mode)
      (let* ( (buffers (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame))))
              (buffers-rotated (append (cdr buffers) (cons (car buffers) nil)))
              (target (ido-completing-read "Buffer: " buffers-rotated)) )
        (switch-to-buffer target))
    (call-interactively 'ido-switch-buffer)))

(eval-after-load "frame-bufs"
  `(progn
     (add-hook 'frame-bufs-mode-on-hook
               #'(lambda ()
                   (global-set-key (kbd "C-x b") 'frame-bufs-switch-buffer)
                   (global-set-key (kbd "C-x B") 'ido-switch-buffer)))
     (add-hook 'frame-bufs-mode-off-hook
               #'(lambda ()
                   (global-set-key (kbd "C-x b") 'ido-switch-buffer)))
     ))


;;and you need to modify your 'tabbar-buffer-groups-function'
(defun tabbar-buffer-grouping-simple-with-frame-bufs ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (setq last-tabbar-ruler-tabbar-buffer-groups
        (list
         (cond
          ((= (aref (buffer-name) 0) ?*)
           "Emacs")
          ((or (memq major-mode '(dired-mode
                                  eshell-mode
                                  shell-mode
                                  occur-mode
                                  grep-mode
                                  compilation-mode)))
           "Utils")
          (t
           "Files"
           ))))
  (if (and (featurep 'frame-bufs)
           frame-bufs-mode
           (memq (current-buffer) (frame-bufs-buffer-list (selected-frame))))
      (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups)))

(eval-after-load "frame-bufs"
  `(progn
     ;;(fset 'bmz/tabbar-buffer-groups-function 'tabbar-buffer-grouping-simple-with-frame-bufs)
     (setq tabbar-buffer-groups-function 'tabbar-buffer-grouping-simple-with-frame-bufs)
     ))


