;;* code folding & section-based navigation
;; (for minibuffer completion, see 25-minibuffer.el)

;;** hideshow

;;*** hideshowvis add +/- symbol in left fringe
(autoload 'hideshowvis-enable "hideshowvis" "Add markers to the fringe for regions foldable by `hideshow-mode'." t)
(autoload 'hideshowvis-minor-mode "hideshowvis" "Will indicate regions foldable with hideshow in the fringe." 'interactive)

(eval-after-load "hideshowvis" '(load "hideshow-fringe" t))

;;(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)

;;(eval-after-load 'python
;;  (add-hook 'python-mode-hook 'hideshowvis-enable))

;(eval-after-load "hideshow"
;  (define-key hs-minor-mode-map (kbd "C-+")  'hs-toggle-hiding))

;;**** fix compatibility between hideshowvis & transpose-words
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
     (define-key hs-minor-mode-map (kbd "<left-fringe> <mouse-2>")  'hideshowvis-refresh-fringe)
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
(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")        outline-mode-prefix-map)
;;     (global-set-key (kbd "s-z")        outline-mode-prefix-map)

     (define-key outline-mode-prefix-map (kbd "C-s") 'show-subtree)

     (global-set-key (kbd "<f2> z")        outline-mode-prefix-map)

     ))

(progn
  (global-set-key (kbd "<H-up>")     'outline-previous-visible-heading)
  (global-set-key (kbd "<H-down>")   'outline-next-visible-heading)

  (global-set-key (kbd "<C-M-up>")     'outline-previous-visible-heading)
  (global-set-key (kbd "<C-M-down>")   'outline-next-visible-heading)

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
(define-key outline-mode-prefix-map [C-return] 'foldout-zoom-or-exit)


;;*** outline-cycle: org-mode like operations
;;  cycling between FOLDED, SUBTREE, CHILDREN
(autoload 'outline-cycle   "outline-magic" "Visibility cycling for outline(-minor)-mode." t)
(autoload 'outline-promote "outline-magic" "Decrease the level of an outline-structure by ARG levels." t)
(autoload 'outline-demote  "outline-magic" "Increase the level of an outline-structure by ARG levels." t)
(autoload 'outline-move-subtree-up   "outline-magic" "Move the currrent subtree up past ARG headlines of the same level." t)
(autoload 'outline-move-subtree-down "outline-magic" "Move the currrent subtree down past ARG headlines of the same level. " t)

(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "<f2> <tab>")   'outline-cycle)
     
     (define-key outline-mode-prefix-map (kbd "<backtab>") 'outline-cycle) ;;on Linux, it's <backtab> but not <S-tab>
     (define-key outline-mode-prefix-map (kbd "<M-up>"   ) 'outline-move-subtree-up)
     (define-key outline-mode-prefix-map (kbd "<M-down>" ) 'outline-move-subtree-down)
     (define-key outline-mode-prefix-map (kbd "<M-left>" ) 'outline-promote)
     (define-key outline-mode-prefix-map (kbd "<M-right>") 'outline-demote)))

;;*** org-mode style headings in comment
(autoload 'outline-org/outline-cycle               "outline-org" nil t)
(autoload 'outline-org/outline-command-dispatcher  "outline-org" nil t)

(global-set-key (kbd "<H-tab>") 'outline-org/outline-cycle)
(global-set-key (kbd "C-z C-z") 'outline-org/outline-command-dispatcher)

(autoload 'outline-org-heading-mode               "outline-org" nil t)
(autoload 'outline-org-mode                       "outline-org" nil t)

(add-hook 'find-file-hook #'(lambda ()
                              (if (string-match "/.emacs.d/init.d/.*.el$" buffer-file-name)
                                  (ignore-errors
                                    (outline-org-mode t)))))

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

     (define-key qtmstr-outline-header-map [mouse-2]  'hide-leaves)
     (define-key qtmstr-outline-header-map [mouse-3]  'show-all)
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


;;** bm: buffer-local bookmarks
(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)
(global-set-key (kbd "<f2> t") 'bm-toggle)
(global-set-key (kbd "<C-f2>") 'bm-toggle)

(idle-require 'bm)
(eval-after-load "bm"
  `(progn
      (global-set-key (kbd "<f2> t") 'bm-toggle)
      (global-set-key (kbd "<f2> n") 'bm-next)
      (global-set-key (kbd "<f2> p") 'bm-previous)
      (global-set-key (kbd "<f2> l") 'bm-show)
      (global-set-key (kbd "<f2> r") 'bm-bookmark-regexp)
      (global-set-key (kbd "<f2> c") 'bm-remove-all-current-buffer)

      (global-set-key (kbd "<f2> <f2>") 'bm-next)

      (if (fboundp 'anything-bm-list)
          (global-set-key (kbd "<f2> l") 'anything-bm-list))
      ))

;;(idle-require 'linemark) ;; linemark.el from CEDET
(eval-after-load "linemark"
  `(progn        
     (define-key global-map (kbd "<f2> t") 'viss-bookmark-toggle)
     (define-key global-map (kbd "<f2> n") 'viss-bookmark-prev-buffer)
     (define-key global-map (kbd "<f2> p") 'viss-bookmark-next-buffer)
     (define-key global-map (kbd "<f2> c") 'viss-bookmark-clear-all-buffer)

     (define-key global-map (kbd "<f2> <f2>") 'viss-bookmark-next-buffer)
      )

;;**  linkd: visualize section header & links (to file/man/info/url)
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)
(autoload 'linkd-follow "linkd" "Follow the link represented by SEXP." nil)
(eval-after-load "linkd"
    `(progn
      ;;;_.. restore [mouse-4] (for mwheel-scroll, linkd bind it to `linkd-back')
      (when (eq window-system 'x)
        (define-key linkd-map [mouse-4] nil)
        (define-key linkd-overlay-map [mouse-4] nil))
        
;;      (add-hook 'emacs-lisp-mode-hook 'linkd-enable)
;;      (add-hook 'python-mode-hook 'linkd-enable)
;;      (add-hook 'espresso-mode-hook 'linkd-enable)
      
      ))


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
