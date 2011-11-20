;;** tabbar
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
         (not (display-graphic-p)))   ;;won't work well in GUI
    (idle-require 'tabbar-ruler))

;;*** show tabbar group on modeline
(setq mode-line-tabbar-group  '(:eval (when (tabbar-mode-on-p)
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
