
;;;_ S(@* "emacs options")

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)
          

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



;;;_. misc keys
;(global-set-key (kbd "<f3> C-f") 'ffap-other-window)

(defun describe-major-mode ()
  (interactive)
  (let ( (mode major-mode) )
    (with-help-window
        (format "%s" mode)
      (describe-function mode))))

(define-key minibuffer-local-map (kbd "ESC ESC") 'minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)

;;(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

;;(global-set-key (kbd "M-g d") 'dired-jump) ;;C-x C-j


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


;;;_ S(@* "search")
;;;_. hidesearch
(autoload 'hidesearch "hidesearch" "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines" "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines" "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

;;FIXME: anything-occur is better?


;;;_ S(@* "completion")

;;;_. auto-completion

;;;_.. use `pos-tip' to fix the popup window position issue
;; `auto-complete' 1.4 already use `pos-tip'
(when (require 'popup-pos-tip nil t)
  (defadvice popup-tip
    (around popup-pos-tip-wrapper (string &rest args) activate)
    (if (memq window-system '(x windows-nt))
        (apply 'popup-pos-tip string args)
      ad-do-it)))

;;;_.. complete file name
(defun ac-expand-filename ()  ;;FIXME: `ac-complete-filename'?
  (interactive)
  (let ( (ac-sources '(ac-source-filename ac-source-files-in-current-dir)) )
    (call-interactively 'ac-start)))

(if (boundp 'undo-tree-map)
    (define-key undo-tree-map (kbd "C-/") nil))
(global-set-key (kbd "C-/") 'ac-expand-filename)

;;;_.. complete english words
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
  (if (file-exists-p "/usr/share/dict/words")
      (find-file-noselect "/usr/share/dict/words")
    (if (file-exists-p "~/.emacs.d/etc/words")
        (find-file-noselect "~/.emacs.d/etc/words")))
  (call-interactively 'ac-complete-words-in-all-buffer))

(global-set-key (kbd "C-, w") 'ac-expand-english-words)


;;;_. completion-ui
(autoload 'complete-dabbrev "completion-ui" nil t)
(autoload 'complete-etags   "completion-ui" nil t)
(autoload 'complete-files   "completion-ui" nil t)
(global-set-key (kbd "C-, d") 'complete-dabbrev)
(global-set-key (kbd "C-, t") 'complete-etags)
(global-set-key (kbd "C-, f") 'complete-files)
;;(global-set-key (kbd "C-, s") 'complete-symbol) ;;elisp
;;(global-set-key (kbd "C-, >") 'complete-nxml)
;;(global-set-key (kbd "C-, <") 'complete-nxml)
(global-set-key (kbd "C-, $") 'complete-ispell)

(autoload 'complete-ispell-lookup "completion-ui-more-source")
(global-set-key (kbd "C-, $") 'complete-ispell-lookup)

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



