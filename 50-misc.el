
;;;_ S(@* "emacs options")

(global-set-key (kbd "<C-10> F") 'menu-set-font)

(global-set-key (kbd "<mode-line> <C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<mode-line> <C-wheel-down>") 'text-scale-decrease)


(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)
          

;;;_ S(@* "minor modes")

;;(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

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
(global-set-key (kbd "<f10> C") 'auto-complete-mode)

(autoload 'highlight-indentation "highlight-indentation" "Toggle highlight indentation." t)
(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(autoload 'idle-highlight "idle-highlight" nil t)
(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(autoload 'visible-mark-mode "visible-mark" nil t)
(autoload 'drag-stuff-mode "drag-stuff" nil t)
(autoload 'setnu-mode "setnu" "vi-style line numbers" t)

(global-set-key (kbd "<f10> I") 'highlight-indentation)
(global-set-key (kbd "<f10> P") 'highlight-parentheses-mode)
(global-set-key (kbd "<f10> H") 'idle-highlight)
(global-set-key (kbd "<f10> R") 'rainbow-delimiters)
(global-set-key (kbd "<f10> M") 'visibile-mark-mode)
(global-set-key (kbd "<f10> D") 'drag-stuff-mode)
(global-set-key (kbd "<f10> N") 'setnu-mode)

     
;;;_ S(@* "keybindings")
;;;_. hyper keys
(global-set-key (kbd "H-a") 'mark-whole-buffer)
;; H-s
;; H-d
;; H-f

(global-set-key (kbd "H-z") 'undo-tree-undo)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-c") 'kill-ring-save)
(global-set-key (kbd "H-v") 'cua-paste)

(defun select-parened-expression ()
  (interactive)
  (if (re-search-backward "[({]")
      (set-mark (save-excursion
                 (goto-match-paren 1)
                 (point)
                 ))))

(global-set-key (kbd "H-b") 'select-parened-expression)

;; H-q
(global-set-key (kbd "H-w") 'toggle-truncate-lines)
(global-set-key (kbd "H-y") 'undo-tree-redo)
(global-set-key (kbd "H-e") 'kill-whole-line)
;; H-r
(global-set-key (kbd "H-t") 'transpose-selections)

(global-set-key (kbd "H-g") 'keyboard-quit)

(global-set-key (kbd "<H-up>")     'outline-previous-visible-heading)
(global-set-key (kbd "<H-down>")   'outline-next-visible-heading)

;;;_. misc keys
;(global-set-key (kbd "<f3> C-f") 'ffap-other-window)

(define-key minibuffer-local-map (kbd "ESC ESC") 'minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)

(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

(global-set-key (kbd "M-g d") 'dired-jump) ;;C-x C-j


;;;_ S(@* "editing")
;; make M-z behave more as zap-up-to-char
(defun zap-up-to-char (arg char)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (interactive "p\ncZap to char: ")
    (zap-to-char arg char)
    (insert char)
    (forward-char -1))

(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun zap-back-to-char (arg char)
  (interactive "p\ncBack-zap to char: ")
  (zap-to-char (- arg) char))

(global-set-key (kbd "ESC M-z") 'zap-back-to-char)



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


;;;_ S(@* "folding")
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

;;;_. selective display (quick & dirty code folding)
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

;;;_ S(@* "search")
;;;_. hidesearch
(autoload 'hidesearch "hidesearch" "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines" "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines" "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

;;FIXME: anything-occur is better?

     
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



;;;_ S(@* "completion")

;;;_. auto-completion
(defun ac-expand-filename ()  ;;FIXME: `ac-complete-filename'?
  (interactive)
  (let ( (ac-sources '(ac-source-filename ac-source-files-in-current-dir)) )
    (call-interactively 'ac-start)))

(if (boundp 'undo-tree-map)
    (define-key undo-tree-map (kbd "C-/") nil))
(global-set-key (kbd "C-/") 'ac-expand-filename)

;; (defun ac-expand-dabbrev ()
;;   (interactive)
;;   (when (not (featurep 'ac-dabbrev)) (require 'ac-dabbrev))
;;   (flet ( (ac-dabbrev-get-candidates (abbrev)
;;                                      '(ac-dabbrev-get-limit-candidates abbrev t)) )
;;     (let ( (ac-sources '(ac-source-abbrev ac-source-dabbrev))
;;            (ac-candidate-max 50)
;;            )
;;       (call-interactively 'ac-start))))

;;(global-set-key (kbd "C-M-/") 'ac-expand-dabbrev)
(global-set-key (kbd "C-M-/") 'ac-complete-words-in-all-buffer)

;; (defun ac-expand-english-words ()
;;   "complete english words."
;;   (interactive)
;;   (find-file-noselect "/usr/share/dict/words")
;;   (call-interactively 'ac-expand-dabbrev))

(defun ac-expand-english-words ()
  (interactive)
  (find-file-noselect "/usr/share/dict/words")
  (call-interactively 'ac-complete-words-in-all-buffer))

(global-set-key (kbd "C-, w") 'ac-expand-english-words)


;;;_. completion-ui
(idle-require 'completion-ui)
(global-set-key (kbd "C-, d") 'complete-dabbrev)
(global-set-key (kbd "C-, t") 'complete-etags)
(global-set-key (kbd "C-, f") 'complete-files)
  
;;(global-set-key (kbd "C-, s") 'complete-symbol) ;;elisp
;;(global-set-key (kbd "C-, >") 'complete-nxml)
;;(global-set-key (kbd "C-, <") 'complete-nxml)
(global-set-key (kbd "C-, $") 'complete-ispell)

(autoload 'complete-ispell-lookup "complete-ui-more-source")
(global-set-key (kbd "C-, $") 'complete-ispell-lookup)


;;;_ S(@* "misc")
;;;_. extend selection incrementally (ergoemacs-functions.el)
;; http://xahlee.org/emacs/syntax_tree_walk.html
(autoload 'extend-selection "ergoemacs-functions" nil t)
(global-set-key (kbd "C-.") 'extend-selection)
;; see also: mark-sexp (C-M-SPC), mark-word (M-@)


;;;_. opening server files always in a new frame
;;http://www.emacswiki.org/emacs/EmacsClient#toc21

(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))


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

;;;_. maximize frame
(when (and window-system
       (require 'maxframe nil t))
  ;; (setq mf-max-width 1600)  ;; Pixel width of main monitor.
  (maximize-frame)
  ;; maximize any new frame
  (add-hook 'window-setup-hook 'maximize-frame t))

;;;_. TODO: fuzzy.el

