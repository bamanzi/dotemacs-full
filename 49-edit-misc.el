
;;** position
;;** paren/pair
;;** search

(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(global-set-key (kbd "<f10> P") 'highlight-parentheses-mode)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(global-set-key (kbd "<f10> rd") 'rainbow-delimiters)

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)
          
;;** select

;;** misc
;;*** iedit

;;;_ S(@* "minor modes")

;; (global-set-key (kbd "<f10> c") 'highlight-changes-visible-mode)
;; (global-set-key (kbd "<f10> f") 'auto-fill-mode)
;; (global-set-key (kbd "<f10> p") 'show-paren-mode)
;; (global-set-key (kbd "<f10> w") 'whitespace-mode)
;; (global-set-key (kbd "<f10> h") 'hs-minor-mode)
;; (global-set-key (kbd "<f10> o") 'outline-minor-mode)
;; (global-set-key (kbd "<f10> v") 'toggle-viper-mode)
;; (global-set-key (kbd "<f10> C-w") 'visual-line-mode)
;; (global-set-key (kbd "<f10> t") 'toggle-truncate-lines)
;; (global-set-key (kbd "<f10> l") 'linum-mode)

;;;_. 3rd-party modules

(global-set-key (kbd "<f10> M") 'visibile-mark-mode)
(global-set-key (kbd "<f10> D") 'drag-stuff-mode)
(global-set-key (kbd "<f10> N") 'setnu-mode)

     
;;;_ S(@* "keybindings")


;;;_. misc keys


;(global-set-key (kbd "<f3> C-f") 'ffap-other-window)


(global-set-key (kbd "C-c d") 'diff-buffer-with-file)




;;;_ S(@* "editing")


;;;_ S(@* "search")
;;;_. hidesearch
(autoload 'hidesearch "hidesearch" "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines" "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines" "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

;;FIXME: anything-occur is better?

;;;_ S(@* "secondary selection")
(autoload 'secondary-to-primary "second-sel" nil t)
(autoload 'primary-to-secondary "second-sel" nil t)
(autoload 'secondary-swap-region "second-sel" nil t)
(define-key search-map (kbd "2") 'primary-to-secondary)
(define-key search-map (kbd "1") 'secondary-to-primary)
(define-key search-map (kbd "`") 'secondary-swap-region)

;;(define-key search-map (kbd "~") 'transpose-selections)

;;;_ S(@* "misc")
;;;_. extend selection incrementally (ergoemacs-functions.el)
;; http://xahlee.org/emacs/syntax_tree_walk.html
(autoload 'extend-selection "ergoemacs-functions" nil t)
(global-set-key (kbd "C-.") 'extend-selection)
;; see also: mark-sexp (C-M-SPC), mark-word (M-@)



;;;_. quickly swap lines
;; Move line up/down. Stolen from org-mode's M-up/down
;; TODO: support region (move region line up/down)
;; see also:  (@file :file-name "drag-stuff.el" :to "define-minor-mode drag-stuff-mode")
(defun swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))
