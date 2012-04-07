;;** tabbar & perspetive: grouping buffers & windows

;; (find-library "tabbar")

;;*** ide-skel
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-skel'
;; as this would mess up the tab group definition of `ide-skel'

;;*** tabbar-mode basic
(unless (featurep 'tabbar)
  (require 'tabbar nil t))

(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward-tab)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward-tab)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
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

;;*** buffer grouping functions

(defun tabbar-buffer-grouping-simple ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (setq last-tabbar-ruler-tabbar-buffer-groups
        (list
         (cond
           ((= (aref (buffer-name) 0) ?*)
           "*Emacs*")
          ((or (memq major-mode '(dired-mode eshell-mode shell-mode
                                             occur-mode grep-mode compilation-mode)))              
           "Utils")
          (t
           "Files"
           ))))
  (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups))

(setq bmz/tabbar-buffer-groups-function 'tabbar-buffer-grouping-simple)

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

;;*** tabbar-ruler: add context menu to tabs
(if (and (not (featurep 'ide-skel))   ;;conflicting with idle-
         (display-graphic-p))   ;;won't work well in terminal(?)
    (idle-require 'tabbar-ruler))

(eval-after-load "tabbar-ruler"
  `(progn
     ;; restore my choice (tabbar-ruler.el would change this)
     (setq tabbar-buffer-groups-function bmz/tabbar-buffer-groups-function)
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

(setq tabbar-header-line-format
              '((:propertize "[X]"
                             local-map
                             (keymap
                              (header-line keymap
                                           (mouse-1   . mouse-delete-window)
                                           (C-mouse-1 . mouse-tear-off-window)
                                           (mouse-2   . tabbar-tabset-menu)
                                           (mouse-3   . widen-current-window-by-mouse)
                                           (C-mouse-3 . mouse-delete-other-windows)))
                             help-echo "  mouse-1: delete current window
C-mouse-1: tear of current window into a new frame
  mouse-2: list buffers of current tabset in menu
  mouse-3: widen current window
C-mouse-3: delete other windows")
                which-func-format
                (:eval (tabbar-line))))

(eval-after-load "tabbar"
  `(progn
     (setq-default header-line tabbar-header-line-format)
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
mouse-3, close the current buffer."
       (let ((mouse-button (event-basic-type event)))
         (cond
          ((eq mouse-button 'mouse-1)
           (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
          ((eq mouse-button 'mouse-2)
           (call-interactively 'tabbar-tabset-menu))
          ((eq mouse-button 'mouse-3)
           (kill-buffer nil))
          )))
     ))


;;*** perspective
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
