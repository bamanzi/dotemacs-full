
;;** svn
(unless (fboundp 'vc-svn-root)  ;;fix emacs-23's svn support
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

;;*** tortoise	
(if (eq system-type 'windows-nt)
	(idle-require 'tortoise-svn))

;;*** svn 1.7
;;you need dsvn.el or vc-svn17.el


;;** git
(autoload 'magit-status  "magit"
  "Open a Magit status buffer for the Git repository containing" t)
(autoload 'magit-log "magit"
  "Command for `log'." t)

;;*** git-gutter
;; https://github.com/syohex/emacs-git-gutter
(autoload 'git-gutter:toggle "git-gutter"
  "toggle to show diff information" t)

(defun frame-reinit-git-gutter (&optional frame)
  "Choose `git-gutter' implementation from `git-gutter.el' or `git-gutter-fringe.el'."
  (interactive)
  (if (require 'git-gutter-fringe nil t)
      (if (display-graphic-p)
          (setq git-gutter:init-function 'git-gutter-fr:init
                git-gutter:view-diff-function 'git-gutter-fr:view-diff-infos
                git-gutter:clear-function 'git-gutter-fr:clear)
        (setq git-gutter:init-function nil
                git-gutter:view-diff-function 'git-gutter:view-diff-infos
                git-gutter:clear-function 'git-gutter:clear-diff-infos))))

(eval-after-load "magit"
  `(progn
     (add-hook 'after-make-frame-functions 'frame-reinit-git-gutter)
     ))


;;** hg
(autoload 'monky-status "monky"
  "Show the status of Hg repository." t)
(autoload 'monky-log  "monky"
  "Undocumented." t)


;;** misc
;;*** diff-hl
;;Highlight uncommitted changes when editing
;;similar to `git-gutter', but works
(autoload 'diff-hl-mode "diff-hl"
  "Toggle VC diff fringe highlighting." t)

(eval-after-load "diff-hl"
  `(progn
     (if (boundp 'prog-mode-hook)
         (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))
     (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
     ))


;;*** vc-jump (similar to dired-jump)
(autoload 'vc-jump "vc-jump"
  "jump to status buffer for the current VC." t)

;;*** ibuffer-vc: group ibuffer list by vc	
(idle-require 'ibuffer-vc)
(eval-after-load "ibuffer-vc"
  `(progn
     (add-hook 'find-file-hooks 'vc-find-file-hook)

     (add-hook 'ibuffer-hook
               (lambda ()
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 (ibuffer-do-sort-by-alphabetic)))

     ;;include vc status info in ibuffer list
     (setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

     ))

;;*** show vc status as icons (use tortoisesvn's icon set)
(defconst vc-icon-dir "d:/Program Files/TortoiseSVN/Icons/XPStyle/")
(add-to-list 'image-load-path vc-icon-dir)

(defun vc-icon ()
  (let ((icon (format "%s.png" (vc-state (buffer-file-name))))
        (bg-colour (face-attribute 'mode-line :background)))
    (propertize
     "  "
     'display (find-image `((:type png
                             :file ,icon
                             :ascent center
                             :background ,bg-colour))))))

;;(add-to-list 'mode-line-format '(:eval (vc-icon)))
