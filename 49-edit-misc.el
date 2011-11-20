;;** paren/pair

(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(global-set-key (kbd "<f10> hp") 'highlight-parentheses-mode)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(global-set-key (kbd "<f10> rd") 'rainbow-delimiters)

(autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors." t)
(global-set-key (kbd "<f10> rb") 'rainbow-mode)

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)
          
;;*** iedit
(autoload 'iedit-mode "iedit" "Toggle iedit mode." t)


;;*** minor modes

;; (global-set-key (kbd "<f10> hc") 'highlight-changes-visible-mode)
;; (global-set-key (kbd "<f10> af") 'auto-fill-mode)
;; (global-set-key (kbd "<f10> sp") 'show-paren-mode)
;; (global-set-key (kbd "<f10> ws") 'whitespace-mode)
;; (global-set-key (kbd "<f10> hs") 'hs-minor-mode)
;; (global-set-key (kbd "<f10> om") 'outline-minor-mode)
;; (global-set-key (kbd "<f10> vi") 'toggle-viper-mode)
;; (global-set-key (kbd "<f10> vl") 'visual-line-mode)
;; (global-set-key (kbd "<f10> C-w") 'toggle-truncate-lines)
;; (global-set-key (kbd "<f10> ln") 'linum-mode)

;;;_. 3rd-party modules

(global-set-key (kbd "<f10> vm") 'visibile-mark-mode)
(global-set-key (kbd "<f10> ds") 'drag-stuff-mode)
(global-set-key (kbd "<f10> sn") 'setnu-mode)

     

;;** search
;;*** hidesearch
(autoload 'hidesearch "hidesearch" "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines" "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines" "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

;;FIXME: anything-occur is better?

;;** secondary selection
(autoload 'secondary-to-primary "second-sel" nil t)
(autoload 'primary-to-secondary "second-sel" nil t)
(autoload 'secondary-swap-region "second-sel" nil t)
(define-key search-map (kbd "2") 'primary-to-secondary)
(define-key search-map (kbd "1") 'secondary-to-primary)
(define-key search-map (kbd "`") 'secondary-swap-region)

;;(define-key search-map (kbd "~") 'transpose-selections)

;;** misc
;;*** extend selection incrementally (ergoemacs-functions.el)
;; http://xahlee.org/emacs/syntax_tree_walk.html
(autoload 'extend-selection "ergoemacs-functions" nil t)
(global-set-key (kbd "C-.") 'extend-selection)
;; see also: mark-sexp (C-M-SPC), mark-word (M-@)


;;** quickly swap lines
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
