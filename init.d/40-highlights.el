;;* some special highlights & overlay

;;** highlight changes
(setq highlight-changes-visibility-initial-state t)
(global-highlight-changes-mode t)

(global-set-key (kbd "<f10> hc") 'highlight-changes-visible-mode)

;;*** use background to show changes rather than foreground
(defun bmz/fix-highlight-chnages-face (frame)
  (interactive (list (selected-frame)))
  ;;(copy-face 'fringe 'highlight-changes)
  ;;NOTE: you need to call this each time you change your color-theme
  (set-face-background 'highlight-changes (face-background 'fringe nil t))
  (set-face-foreground 'highlight-changes 'unspecified)
  ;;(set-face-attribute  'highlight-changes nil :inherit 'fringe)
)

(add-hook 'after-make-frame-functions 'bmz/fix-highlight-chnages-face)


;;** whitespaces
(global-set-key (kbd "<f10> ws") 'whitespace-mode)
;; (setq whitespace-style '(tabs spaces trailing lines
;;                               space-before-tab newline
;;                               indentation empty space-after-tab
;;                               space-mark tab-mark newline-mark))

;;*** develock: programmers whitespace-mode
;; Develock is a minor mode which provides the ability to make font-
;; lock highlight leading and trailing whitespace, long lines and
;; oddities in the file buffer for developers
(autoload 'develock-mode  "develock" "Toggle Develock mode." t)
(setq develock-auto-enable nil)

;;(idle-require 'develock)  ;;FIXME: conflict with outline-org-heading-mode

;;FIXME: now develock-mode would make `lisp-indent-line' behavior strangely
(eval-after-load "develock"
  `(progn
     (ad-disable-advice 'lisp-indent-line 'around 'remove-useless-whitespace)
     (ad-deactivate 'lisp-indent-line)
     ))

;;** highlight current position
;;*** mark
(autoload 'visible-mark-mode  "visible-mark"
  "A mode to make the mark visible." t)

(global-set-key (kbd "<f10> vm") 'visible-mark-mode)

;;*** current line
;;TIP: `hl-line-mode' available in GNU Emacs since 23.2

(autoload 'flash-line-highlight          "hl-line+"
  "Highlight the current line for `hl-line-flash-show-period' seconds." t)
(autoload 'toggle-hl-line-when-idle     "hl-line+"
  "Turn on or off using `global-hl-line-mode' when Emacs is idle." t)


(autoload 'highline-mode "highline"
  "minor mode to highlight current line in buffer" t)

(autoload 'hl-spotlight-mode "hl-spotlight" "spotlight current few lines." t)

;;*** current column
(autoload 'column-highlight-mode              "col-highlight"
  "Toggle highlighting the current column." t)
(autoload 'toggle-highlight-column-when-idle  "col-highlight"
  "Turn on or off highlighting the current column when Emacs is idle." t)

;;*** both
(autoload 'crosshairs        "crosshairs"
  "Highlight current position with crosshairs." t)
(autoload 'crosshairs-flash  "crosshairs"
  "Highlight the current line and column temporarily." t)
(autoload 'crosshairs-mode    "crosshairs"
  "Toggle highlighting the current line and column." t)

(define-key global-map (kbd "M-+") 'crosshairs-flash)
(global-set-key (kbd "<f10> ch")   'crosshairs-mode)


;;** highlight matching paren/pair

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)


(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(global-set-key (kbd "<f10> hp") 'highlight-parentheses-mode)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(global-set-key (kbd "<f10> rd") 'rainbow-delimiters)

;;** highlight symbol
;;*** Emacs built-in
(autoload 'highlight-regexp  "hi-lock" "Set face of each match of REGEXP to FACE." t)
(autoload 'highlight-phrase  "hi-lock" "Set face of each match of phrase REGEXP to FACE." t)
(autoload 'highlight-lines-matching-regexp  "hi-lock" "Set face of all lines containing a match of REGEXP to FACE." t)
(autoload 'unhighlight-regexp  "hi-lock" "Remove highlighting of each match to REGEXP set by hi-lock." t)

;;*** manually: `highlight-symbol'
(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" "Toggle highlighting of the symbol at point." t)

(idle-require 'highlight-symbol)

(global-set-key (kbd "C-c j")          'highlight-symbol-at-point)
(define-key search-map (kbd "j")        'highlight-symbol-at-point)
(define-key search-map (kbd "<up>")   'highlight-symbol-prev)
(define-key search-map (kbd "<down>") 'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)
(global-set-key (kbd "C-#")      'highlight-symbol-prev) ;;NOTE: key not work on terminal
(global-set-key (kbd "C-*")    'highlight-symbol-next)   ;;NOTE: key not work on terminal

;;*** automatically highlight symbol at point for a short time
(autoload 'idle-highlight "idle-highlight"
  "highlight the word the point is on" t)
(autoload 'idle-highlight-mode "idle-highlight"
  "highlight the word the point is on" t)

(global-set-key (kbd "<f10> ih") 'idle-highlight)

(idle-require 'idle-highlight)
(eval-after-load "idle-highlight"
  `(progn
     (if (fboundp 'idle-highlight)
         (add-hook 'find-file-hook 'idle-highlight)
       (add-hook 'find-file-hook 'idle-highlight-mode))
     ))

;;See also: light-symbol.el


;;** in-buffer bookmark with visual effects
(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)

(idle-require 'bm)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(progn
  (global-set-key (kbd "<M-f2>") 'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous)
  (global-set-key (kbd "<f2> r") 'bm-bookmark-regexp)
  (global-set-key (kbd "<f2> c") 'bm-remove-all-current-buffer)

  (global-set-key (kbd "<left-fringe> <S-mouse-1>")     'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-up>")    'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-down>")  'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <S-mouse-2>")     'bm-show)

  (global-set-key (kbd "<f2> <f2>") 'bm-next)

  (global-set-key (kbd "<H-f2>") 'bm-show)  
  (global-set-key (kbd "<f2> l") 'bm-show)

  (if (fboundp 'anything-bm-list)
      (global-set-key (kbd "<f2> l") 'anything-bm-list))
  )

;;*** linemark.el from CEDET
;(idle-require 'linemark) 
(eval-after-load "linemark"
  `(progn        
     (define-key global-map (kbd "<f2> t") 'viss-bookmark-toggle)
     (define-key global-map (kbd "<f2> n") 'viss-bookmark-prev-buffer)
     (define-key global-map (kbd "<f2> p") 'viss-bookmark-next-buffer)
     (define-key global-map (kbd "<f2> c") 'viss-bookmark-clear-all-buffer)

     (define-key global-map (kbd "<f2> <f2>") 'viss-bookmark-next-buffer)
     ))



;;** highlight columns
;;*** border (fill-column)
(autoload 'highlight-beyond-fill-column "highlight-beyond-fill-column"
  "Setup this buffer to highlight beyond the `fill-column'." t)

(autoload 'fci-mode "fill-column-indicator" "Undocumented." t)

;;*** manual highlight a column
;;highlight current column or specific column (C-u 70 M-x column-marker-1)
;;to turn of, C-u M-x column-marker-1
(autoload 'column-marker-1  "column-marker"
  "Highlight a column." t)
(autoload 'column-marker-2  "column-marker"
  "Highlight a column." t)
(autoload 'column-marker-3  "column-marker"
  "Highlight a column." t)

;;** linkd : links to file/man/info/url
;; e.g. (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode" :display "LinkdMode - EmacsWiki")
;; (linkd-follow (@file :file-name "linkd.el" :to "linkd-mode"))
(autoload 'linkd-mode        "linkd" "Create or follow hypertext links." t)
(autoload 'linkd-insert-link "linkd" "Insert a link." t)
(autoload 'linkd-follow      "linkd" "Follow the link represented by SEXP." nil)

(eval-after-load "linkd"
  `(progn
     ;;_.. linkd icons path
     (let ( (dir (concat (file-name-directory (locate-library "linkd")) "icons")) )
        (when (file-exists-p dir)
          (setq linkd-icons-directory dir)
          (setq linkd-use-icons t)))

      ;;_.. restore [mouse-4] (for mwheel-scroll, linkd bind it to `linkd-back')
      (when (eq window-system 'x)
        (define-key linkd-map [mouse-4] nil)
        (define-key linkd-overlay-map [mouse-4] nil))
        
;;      (add-hook 'emacs-lisp-mode-hook 'linkd-enable)
;;      (add-hook 'python-mode-hook 'linkd-enable)
;;      (add-hook 'espresso-mode-hook 'linkd-enable)

      (progn  ;; change linkd faces according to current theme
        ;; (copy-face 'org-link 'linkd-generic-link )
        ;; (copy-face 'org-link 'linkd-generic-link-name)
        
        (set-face-foreground 'linkd-generic-link (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link t)
        
        (set-face-foreground 'linkd-generic-link-name (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link-name t)
        
        (set-face-foreground 'linkd-generic-link-name (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link-name t)
        
        (set-face-foreground 'linkd-tag-name (face-foreground 'org-done))
        )
      ))

;; turn
;;    http://www.emacswiki.org/emacs/LinkdMode
;; into:
;;    (linkd-follow (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode"))
(defun linkd-wrap-url (begin end)
  (interactive "r")
  (goto-char end)
  (insert-string "\"))")
  (goto-char begin)
  (insert-string "(linkd-follow (@url :file-name \""))



;;** pulse: temperarily highlight a line/region, to draw user's attension
(idle-require 'pulse)
(unless (fboundp 'cedet-called-interactively-p)
  ;;(defalias 'cedet-called-interactively-p 'called-interactively-p)
  (defmacro cedet-called-interactively-p (&optional arg)
    '(interactive-p))
        
  ;; (defun cedet-called-interactively-p ()
  ;;   (if (string< emacs-version "23.2")
  ;;       (called-interactively-p)
  ;;     (called-interactively-p 'any)))
    )

;;(setq pulse-command-advice-flag (if window-system 1 nil))
(setq pulse-command-advice-flag t)

(eval-after-load "pulse"
  `(progn
     (if (fboundp 'pulse-toggle-integration-advice)
         (pulse-toggle-integration-advice t))
     
     (defadvice imenu (after pulse-advice activate)
       "Cause the line that is `imenu'd to pulse when the cursor gets there."
       (when (and pulse-command-advice-flag (cedet-called-interactively-p))
         (pulse-momentary-highlight-one-line (point))))

     (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
       "Cause the line that is `goto'd to pulse when the cursor gets there."
       (when (and pulse-command-advice-flag (interactive-p)
                  (> (abs (- (point) (mark))) 400))
                   (pulse-momentary-highlight-one-line (point))))
     ))
         

;;** misc
;;*** higlight-indentation
;;very useful for indentation sensit
(autoload 'highlight-indentation "highlight-indentation"
  "Toggle highlight indentation." t)
(autoload 'highlight-indentation-mode "highlight-indentation"
  "Toggle highlight indentation." t)

(eval-after-load "highlight-indentation"
  `(unless (fboundp 'highlight-indentation-mode)
     (defalias 'highlight-indentation-mode  'highlight-indentation)))

(global-set-key (kbd "<f10> hi") 'highlight-indentation-mode)


;;*** fixme-mode 
(autoload 'fixme-mode "fixme-mode"
  "A minor mode for making FIXME and other warnings stand out" t)
(autoload 'fic-ext-mode "fic-ext-mode"
  "minor mode for highlighting FIXME/TODO in comments" t)

;;*** highlight misc url, fixme, todo

(defun highlight-misc-stuff/bmz ()
  (interactive)
  (highlight-regexp "https?://[^ \n]*" 'link)
  (highlight-regexp "\\<\\(FIXME\\|TODO\\|NOTE\\|BUG\\):" 'font-lock-warning-face))

(add-hook 'find-file-hook 'highlight-misc-stuff/bmz)

;;*** rainbow-mode: colorize strings like 'red', "#3303c4"
(autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors." t)
(global-set-key (kbd "<f10> rb") 'rainbow-mode)

;;*** markerpen / himark
;;TODO: foo



