;;* code folding & section-based navigation
;; (for minibuffer completion, see 25-minibuffer.el)

;;** hideshow
;;(global-set-key (kbd "M-+")  'hs-toggle-hiding)

(eval-after-load "hideshow"
  `(progn
     (define-key hs-minor-mode-map (kbd "M-+")  'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "<C-down-mouse-1>")  'hs-mouse-toggle-hiding)
     ))

(defun bmz/turn-on-hideshow ()
  (interactive)
  (if (display-graphic-p)
      (when (or (require 'hideshowvis nil t)
                (require 'hideshow-fringe nil t))
          (hideshowvis-enable))
    (hs-minor-mode t)))

(add-hook 'find-file-hook 'bmz/turn-on-hideshow)

(defadvice hideshowvis-minor-mode (before disable-hideshowvis-for-term activate)
  (if (not (display-graphic-p))
      (error "`hideshowvis' would cause emacs hanging on term. cancelled")))

;;*** hideshowvis add +/- symbol in left fringe
(autoload 'hideshowvis-enable "hideshowvis"
  "Add markers to the fringe for regions foldable by `hideshow-mode'." t)
(autoload 'hideshowvis-minor-mode "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe." 'interactive)

(eval-after-load "hideshowvis"
  `(progn
     (load "hideshow-fringe" t)

     ;;(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)

     ;;(eval-after-load 'python
     ;;  (add-hook 'python-mode-hook 'hideshowvis-enable))
     ))

;;**** fix compatibility between hideshowvis & transpose-words
;; The problem: if hideshowvis-minor-mode enabled,  `transpose-words' on "this-is',
;; you'll get 'isthis-',  rather than 'is-this'.

(if nil
;; Because confliction with `transpose-words', we'll removed
;; `hideshowvis-highlight-hs-regions-in-fringe' `after-change-functions'.
(eval-after-load "hideshowvis"
  `(progn
     (add-hook 'hs-minor-mode-hook
               #'(lambda ()
                   (setq after-change-functions (remove 'hideshowvis-highlight-hs-regions-in-fringe
                                                        after-change-functions))))
  
     ;; but +/- signs on left fringe won't update automatically upon changes
     ;; thus we need to refresh it manually
     (defun hideshowvis-refresh-fringe ()
       (interactive)
       (hideshowvis-highlight-hs-regions-in-fringe))

     (define-key hs-minor-mode-map (kbd "C-c @ C-r") 'hideshowvis-refresh-fringe)
     (define-key hs-minor-mode-map (kbd "<left-fringe> <mouse-3>")  'hideshowvis-refresh-fringe)
     ))
)

;;this solution would be better (?)
(eval-after-load "hideshowvis"
  `(progn
     (defadvice transpose-words (around disable-hideshowvis-change-for-transpose-words activate)
       (let ( (old (copy-sequence after-change-functions)) )
         (setq after-change-functions
               (remove 'hideshowvis-highlight-hs-regions-in-fringe after-change-functions))
         ad-do-it
         (setq after-change-functions old)
         )))
  )


;;** outline
(if (global-key-binding (kbd "C-z"))
    (global-unset-key (kbd "C-z")))
(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")        outline-mode-prefix-map)
;;     (global-set-key (kbd "s-z")        outline-mode-prefix-map)
;;     (global-set-key (kbd "<f2> z")        outline-mode-prefix-map)

     (define-key outline-mode-prefix-map (kbd "C-s") 'show-subtree)
     (define-key outline-mode-prefix-map (kbd "<up>")     'outline-previous-visible-heading)
     (define-key outline-mode-prefix-map (kbd "<down>")   'outline-next-visible-heading)
     (define-key outline-mode-prefix-map (kbd "<C-up>")   'outline-backward-same-level)
     (define-key outline-mode-prefix-map (kbd "<C-down>") 'outline-next-visible-heading)
     (define-key outline-mode-prefix-map (kbd "<tab>")    'outline-toggle-children)
     ))

(progn
;;  (global-set-key (kbd "<C-up>")     'outline-previous-visible-heading)
;;  (global-set-key (kbd "<C-down>")   'outline-next-visible-heading)

  (global-set-key (kbd "<C-wheel-up>") 'outline-previous-visible-heading)
  (global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
  (global-set-key (kbd "<C-mouse-1>")  'outline-toggle-children)
  (global-set-key (kbd "<C-mouse-3>")  'hide-sublevels)
  (global-set-key (kbd "<C-mouse-2>")  'show-all)

  (global-set-key (kbd "<f2> sa")      'show-all)
  (global-set-key (kbd "<f2> sb")      'show-branches)
  (global-set-key (kbd "<f2> sc")      'show-children)
  (global-set-key (kbd "<f2> se")      'show-entry)
  (global-set-key (kbd "<f2> ss")      'show-subtree)

  (global-set-key (kbd "<f2> ha")      'hide-sublevels)
  (global-set-key (kbd "<f2> hb")      'hide-body)
  (global-set-key (kbd "<f2> he")      'hide-entry)
  (global-set-key (kbd "<f2> hl")      'hide-leaves)
  (global-set-key (kbd "<f2> hs")      'hide-subtree)

  (global-set-key (kbd "<f2> S")       'hide-sublevels)

  )

;;*** foldout: narrow to outline subtree
(autoload 'foldout-zoom-subtree "foldout" "Open the subtree under the current heading and narrow to it." t)
(autoload 'foldout-exit-fold    "foldout" "Return to the ARG'th enclosing fold view.  With ARG = 0 exit all folds." t)
;; default keybinding (from foldout.el)
;; foldout-zoom-subtree:  C-c C-z,  (outline-prefix) C-z
;; foldout-exit-fold:     C-c C-x,  (outline-prefix) C-x

(defun foldout-zoom-or-exit ()
  (interactive)
  (if (null foldout-fold-list)
      (foldout-zoom-subtree 1)
    (foldout-exit-fold 1)))

(eval-after-load "outline"
  `(progn
     (define-key outline-mode-prefix-map [C-return] 'foldout-zoom-or-exit)
     (define-key outline-mode-prefix-map [C-z]      nil)
     (define-key outline-mode-prefix-map ">"        'foldout-zoom-subtree)
     (define-key outline-mode-prefix-map "<"        'foldout-exit-fold)
     ))



;;*** outline-cycle: org-mode like operations
;;  cycling between FOLDED, SUBTREE, CHILDREN
(autoload 'outline-cycle   "outline-magic" "Visibility cycling for outline(-minor)-mode." t)
(autoload 'outline-promote "outline-magic" "Decrease the level of an outline-structure by ARG levels." t)
(autoload 'outline-demote  "outline-magic" "Increase the level of an outline-structure by ARG levels." t)
(autoload 'outline-move-subtree-up   "outline-magic" "Move the currrent subtree up past ARG headlines of the same level." t)
(autoload 'outline-move-subtree-down "outline-magic" "Move the currrent subtree down past ARG headlines of the same level. " t)

(defun outline-cycle-whole-buffer ()
  (interactive)
  (outline-cycle 4))

(eval-after-load "outline"
  `(progn
     (define-key outline-mode-prefix-map (kbd "<tab>") 'outline-cycle)
     (define-key outline-mode-prefix-map (kbd "<backtab>") 'outline-cycle-whole-buffer)   ;;on Linux, it's <backtab> but not <S-tab>
     (define-key outline-mode-prefix-map (kbd "<S-tab>") 'outline-cycle-whole-buffer)
     
     (define-key outline-mode-prefix-map (kbd "<M-up>"   ) 'outline-move-subtree-up)
     (define-key outline-mode-prefix-map (kbd "<M-down>" ) 'outline-move-subtree-down)
     (define-key outline-mode-prefix-map (kbd "<M-left>" ) 'outline-promote)
     (define-key outline-mode-prefix-map (kbd "<M-right>") 'outline-demote)))


  
;;*** org-mode style headings in comment
(autoload 'outline-org/outline-cycle               "outline-org-like" nil t)
(autoload 'outline-org/outline-command-dispatcher  "outline-org-like" nil t)

(global-set-key (kbd "<H-tab>") 'outline-org/outline-cycle)
(global-set-key (kbd "C-z C-z") 'outline-org/outline-command-dispatcher)

(autoload 'outline-org-heading-mode               "outline-org-like" nil t)
(autoload 'outline-org-mode                       "outline-org-like" nil t)


;;*** qtmstr-outline
;; with this package, we have another set of left-fringe icons to fold/unfold sections
;; and it has overlays for clicking (and RET works on it too)
(idle-require 'qtmstr-outline)
(eval-after-load "qtmstr-outline"
  `(progn
     ;; supress auto turn on
     (remove-hook 'outline-minor-mode-hook #'qtmstr-outline-mode-hook)

     (define-minor-mode qtmstr-outline-mode
       "Add left-fringe +/- icons and line overlays for outline-sections."
       nil
       :group 'outline
       (if qtmstr-outline-mode
           ;; turn on
           (progn
             (unless outline-minor-mode
               (outline-minor-mode t))
             (qtmstr-outline-add-overlays)
             (unless hs-minor-mode
                 ;; avoid key conflictingve with hideshowvis
                 (local-set-key [left-fringe mouse-1] 'qtmstr-outline-fringe-click)))                 
         (qtmstr-outline-remove-overlays)))

     ;; leave mouse-1 for hideshowvis
     (define-key outline-minor-mode-map [left-fringe mouse-1] nil)
     (define-key outline-minor-mode-map [left-fringe C-mouse-1] 'qtmstr-outline-fringe-click)

     (define-key qtmstr-outline-header-map [mouse-1]  'outline-cycle)
;;     (define-key qtmstr-outline-header-map [mouse-2]  'hide-leaves)
     (define-key qtmstr-outline-header-map [mouse-3]  'show-all)

     (define-key qtmstr-outline-header-map (kbd "TAB") 'outline-toggle-children)
     (define-key qtmstr-outline-header-map (kbd "RET") nil)

     (set-face-underline 'qtmstr-outline-header-face t)
     (set-face-bold-p    'qtmstr-outline-header-face t)
;;     (set-face-background 'qtmstr-outline-header-face 
     ))

;;** allout
(eval-after-load "allout"
  `(progn
     (define-key allout-mode-map (kbd "<C-M-up>")     'allout-previous-visible-heading)
     (define-key allout-mode-map (kbd "<C-M-down>")   'allout-next-visible-heading)

     (define-key allout-mode-map (kbd "<C-wheel-up>")   'allout-previous-visible-heading)
     (define-key allout-mode-map (kbd "<C-wheel-down>") 'allout-next-visible-heading)
     (define-key allout-mode-map (kbd "<C-mouse-1>")    'allout-hide-current-subtree)
     (define-key allout-mode-map (kbd "<C-mouse-3>")    'allout-show-current-subtree)
     (define-key allout-mode-map (kbd "<C-mouse-2>")    'allout-show-all)
     ))

;;** fold-dwim: one front-end for hideshow/outline/folding
(autoload 'fold-dwim-toggle "fold-dwim" "Toggle folding at point." t)
(autoload 'fold-dwim-show-all "fold-dwim")
(autoload 'fold-dwm-hide-all   "fold-dwim")

(global-set-key (kbd "C-c +")   'fold-dwim-toggle)
(global-set-key (kbd "C-c C-+") 'fold-dwim-show-all)
(global-set-key (kbd "C-c C--") 'fold-dwim-hide-all)

;;(when (locate-library "fold-dwim")
;;      ;; FIXME: fold-dwim-toggle would fold/unfold on cursor, not the mouse point
;;      (global-set-key (kbd "<left-fringe><mouse-1>") 'fold-dwim-toggle)
;;      )


;;** selective display (quick & dirty code folding)
;; http://www.emacswiki.org/emacs/HideShow#toc5
;; hide lines whose indentation is bigger than x column
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(global-set-key (kbd "C-c \\") 'toggle-selective-display)
;;(global-set-key (kbd "C-+") 'toggle-hiding)


;;** hide-region: manually hide
(autoload 'hide-region-hide  "hide-region"
  "Hides a region by making an invisible overlay over it and save the" t)
(autoload 'hide-region-unhide  "hide-region"
  "Unhide a region at a time, starting with the last one hidden and" t)
;;TODO: sfafa


;;** block movement
;;stolen from http://xahlee.org/emacs/xah_emacs_cursor_movement.el
;;(modified: now it move to next occurrence of 3rd newline char)
(defun forward-block ()
  "Move cursor forward to next occurrence of double newline char.
In most major modes, this is the same as `forward-paragraph', however,
this function behaves the same in any mode.
forward-paragraph is mode dependent, because it depends on
syntax table that has different meaning for “paragraph”."
  (interactive)
  (skip-chars-forward "\n")
  (when (not (search-forward-regexp "\n[[:blank:]]*\n[[:blank:]]*\n" nil t))
    (goto-char (point-max)) ) )

(defun backward-block ()
  "Move cursor backward to previous occurrence of double newline char.
See: `forward-block'"
  (interactive)
  (skip-chars-backward "\n")
  (when (not (search-backward-regexp "\n[[:blank:]]*\n[[:blank:]]*\n" nil t))
    (goto-char (point-min))
    )
  )

;;(global-set-key (kbd "C-c n") 'forward-block)
;;(global-set-key (kbd "C-c p") 'backward-block)


;;** outline settings for my init files

(defun bmz/turn-on-outline-settings ()
  (if (string-match "/.emacs.d/init.d/.*.el$" buffer-file-name)
      (ignore-errors
        (if (require 'outline-org-like nil t)
            (outline-org-mode t))
        ;; (if (require 'qtmstr-outline nil t)
        ;;     (qtmstr-outline-mode-hook))
        )))


(add-hook 'find-file-hook 'bmz/turn-on-outline-settings)

(global-set-key (kbd "<M-f2>") 'outline-cycle)

