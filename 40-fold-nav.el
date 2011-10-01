;; in-buffer navigation & code folding


(global-set-key (kbd "<H-up>")     'outline-previous-visible-heading)
(global-set-key (kbd "<H-down>")   'outline-next-visible-heading)


;;;_. list & choose method
(defun bmz/select-method()
  (interactive)
  (require 'idomenu "idomenu" t)  
  (require 'eassist "eassist" t)  ;; for `eassist-list-methods'
  (cond
   ( (and (fboundp 'anything-browse-code)
	  (memq major-mode '(emacs-lisp-mode lisp-interaction-mode python-mode)))
     (call-interactively 'anything-browse-code))
   ( (fboundp 'anything-imenu)
     (call-interactively 'anything-imenu) )
   ( (and (fboundp 'eassist-list-methods)
	  (memq major-mode '(emacs-lisp-mode c-mode java-mode python-mode))
	  (memq 'semantic-mode minor-mode-alist))
     (call-interactively 'eassist-list-methods) )
   ( (fboundp 'idomenu)
     (call-interactively 'idomenu) )
   (t
    (call-interactively 'imenu))))

(global-set-key (kbd "C-c C-o") 'bmz/select-method)
(global-set-key (kbd "<f5> I") 'bmz/select-method)


;;;_ S(@* "buffer-local bookmarks")
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


;;;_. fold-dwim
(autoload 'fold-dwim-toggle "fold-dwim" "Toggle folding at point." t)
(autoload 'fold-dwim-show-all "fold-dwim")
(autoload 'fold-dwm-hide-all   "fold-dwim")

(global-set-key (kbd "C-c +") 'fold-dwim-toggle)
(global-set-key (kbd "C-c C-+") 'fold-dwim-show-all)
(global-set-key (kbd "C-c C--") 'fold-dwim-hide-all)

;;(when (locate-library "fold-dwim")
;;      ;; FIXME: fold-dwim-toggle would fold/unfold on cursor, not the mouse point
;;      (global-set-key (kbd "<left-fringe><mouse-1>") 'fold-dwim-toggle)
;;      )

;;;_. linkd: visualize section header & links (to file/man/info/url)
(autoload 'linkd-mode "linkd" "Create or follow hypertext links." t)
(eval-after-load "linkd"
    '(progn
       ;;;_.. linkd icons path
      (let ( (dir (concat (file-name-directory (locate-library "linkd")) "icons")) )
        (when (file-exists-p dir)
          (setq linkd-icons-directory dir)
          (setq linkd-use-icons t)))

      ;;;_.. restore [mouse-4] (for mwheel-scroll, linkd bind it to `linkd-back')
      (when (eq window-system 'x)
        (define-key linkd-map [mouse-4] nil)
        (define-key linkd-overlay-map [mouse-4] nil))
        
;;      (add-hook 'emacs-lisp-mode-hook 'linkd-enable)
;;      (add-hook 'python-mode-hook 'linkd-enable)
;;      (add-hook 'espresso-mode-hook 'linkd-enable)
      
      ))



;;;_. block movement
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

(global-set-key (kbd "C-c n") 'forward-block)
(global-set-key (kbd "C-c p") 'backward-block)
