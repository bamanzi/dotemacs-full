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

;;** outline
(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")        outline-mode-prefix-map)))

(global-set-key (kbd "<H-up>")     'outline-previous-visible-heading)
(global-set-key (kbd "<H-down>")   'outline-next-visible-heading)

(global-set-key (kbd "<C-M-up>")     'outline-previous-visible-heading)
(global-set-key (kbd "<C-M-down>")   'outline-next-visible-heading)

(global-set-key (kbd "<C-wheel-up>") 'outline-previous-visible-heading)
(global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
(global-set-key (kbd "<C-mouse-1>")  'outline-toggle-children)
(global-set-key (kbd "<C-mouse-3>")  'hide-sublevels)
(global-set-key (kbd "<C-mouse-2>")  'show-all)

;;*** outline-cycle: org-mode like operation
(autoload 'outline-cycle   "outline-magic" "Visibility cycling for outline(-minor)-mode." t)
(autoload 'outline-promote "outline-magic" "Decrease the level of an outline-structure by ARG levels." t)
(autoload 'outline-demote  "outline-magic" "Increase the level of an outline-structure by ARG levels." t)
(autoload 'outline-move-subtree-up   "outline-magic" "Move the currrent subtree up past ARG headlines of the same level." t)
(autoload 'outline-move-subtree-down "outline-magic" "Move the currrent subtree down past ARG headlines of the same level. " t)

(eval-after-load "outline"
  `(progn
    (define-key outline-mode-prefix-map (kbd "<backtab>") 'outline-cycle) ;;on Linux, it's <backtab> but not <S-tab>
    (define-key outline-mode-prefix-map (kbd "<M-up>"   ) 'outline-move-subtree-up)
    (define-key outline-mode-prefix-map (kbd "<M-down>" ) 'outline-move-subtree-down)
    (define-key outline-mode-prefix-map (kbd "<M-left>" ) 'outline-promote)
    (define-key outline-mode-prefix-map (kbd "<M-right>") 'outline-demote)))

;;*** org-mode style headings in 
(autoload 'outline-org/outline-cycle               "outline-org" nil t)
(autoload 'outline-org/outline-command-dispatcher  "outline-org" nil t)

(global-set-key (kbd "<H-tab>") 'outline-org/outline-cycle)
(global-set-key (kbd "C-z C-z") 'outline-org/outline-command-dispatcher)

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
(ignore-errors
  (or (require 'bm nil t)
      (require 'linkmark nil t))
  )
(if (featurep 'bm)
    (progn
      (global-set-key (kbd "<f2> t") 'bm-toggle)
      (global-set-key (kbd "<f2> n") 'bm-next)
      (global-set-key (kbd "<f2> p") 'bm-previous)
      (global-set-key (kbd "<f2> l") 'bm-show)
      (global-set-key (kbd "<f2> r") 'bm-bookmark-regexp)

      (global-set-key (kbd "<f2> <f2>") 'bm-next)

      (if (fboundp 'anything-bm-list)
          (global-set-key (kbd "<f2> l") 'anything-bm-list))
      )
  (if (featurep 'linemark)              ;; linemark.el from CEDET
      (progn        
        (define-key global-map (kbd "<f2> t") 'viss-bookmark-toggle)
        (define-key global-map (kbd "<f2> n") 'viss-bookmark-prev-buffer)
        (define-key global-map (kbd "<f2> p") 'viss-bookmark-next-buffer)
        (define-key global-map (kbd "<f2> c") 'viss-bookmark-clear-all-buffer)

        (define-key global-map (kbd "<f2> <f2>") 'viss-bookmark-next-buffer)
        ))
      )

;;**  linkd: visualize section header & links (to file/man/info/url)
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)
(autoload 'linkd-follow "linkd.el" "Follow the link represented by SEXP." nil)
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
