;;;_ S(@* "gui options")
(setq use-dialog-box nil)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
    (set-scroll-bar-mode 'right))


;;;_ S(@* "windows & frames")
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;;_. tabbar
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-ske', don't directly load `tabbar' after `ide-ske'
;; as this would mess up the tab group definition of `ide-skel'
(when (or (featurep 'tabbar)
          (load "tabbar" t))
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )

;;;_ S(@* "files & buffers")
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)

;;;_. backup rules
;;(setq make-backup-files t) ;;to disable backup, set it to nil

;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

;; (setq version-control t
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2)

;;;_. recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-menu-path '("File"))
(recentf-mode t)

;;;_. tempbuf
;;(autoload 'turn-on-tempbuf-mode "tempbuf")
(when (load "tempbuf" t)
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'view-mode-hook 'turn-on-tempbuf-mode))
;;;See also: midnight-mode

;;;_ S(@* "windows")
;;;_. winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;;_ tabbar
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if (and window-system
;;         (> emacs-major-version 23))
;;  (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-ske'
;; as this would mess up the tab group definition of `ide-skel'
(when (or (featurep 'tabbar)
          (load "tabbar" t))
  (tabbar-mode t)
  (global-set-key (kbd "<C-tab>") 'tabbar-forward)
  (global-set-key (kbd "<C-S-tab>") 'tabbar-backward))


;;;_ S(@* "key bindings")

;;;_. key modifiers and prefix keys

(when (eq window-system 'w32)
  ;;(setq w32-lwindow-modifier 'super)
  
  (setq w32-lwindow-modifier 'nil)  ;;<lwindow> used as a prefix key
  (setq w32-pass-lwindow-to-system t) ;;if set to nil, a single press <lwindow> would prevent Start Menu
  (define-key key-translation-map (kbd "<lwindow>") (kbd "<f11>"))

  ;; (setq w32-rwindow-modifier 'alt)      
  (setq w32-rwindow-modifier 'hyper)
  (setq w32-pass-rwindow-to-system nil)
  ;;(define-key key-translation-map (kbd "<rwindow>") (kbd "C-c"))
  
  (setq w32-apps-modifier 'hyper)
  
  (setq w32-scroll-lock-modifier nil)
  )
  
;;FIXME: not work
(when (eq window-system 'x)
  (define-key key-translation-map (kbd "<super>") (kbd "<f11>"))
  )

;;;_. <f1> .. <f12> as prefix key

(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

;; toggle minor modes
(global-set-key (kbd "<f10> c") 'highlight-changes-visible-mode)
(global-set-key (kbd "<f10> f") 'auto-fill-mode)
(global-set-key (kbd "<f10> p") 'show-paren-mode)
(global-set-key (kbd "<f10> w") 'whitespace-mode)
(global-set-key (kbd "<f10> h") 'hs-minor-mode)
(global-set-key (kbd "<f10> o") 'outline-minor-mode)
(global-set-key (kbd "<f10> r") 'ruler-mode)
(global-set-key (kbd "<f10> t") 'toggle-truncate-lines)
(global-set-key (kbd "<f10> C-w") 'visual-line-mode)
(global-set-key (kbd "<f10> l") 'linum-mode)
(global-set-key (kbd "<f10> v") 'toggle-viper-mode)


;;;_ S(@* "editing")
(transient-mark-mode t)
(setq shift-select-mode t)
(delete-selection-mode t)

;;;_. CUA
(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'hyper)  ;;leave C-RET
(cua-mode t)

(global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)

;;;_. tab key & indent
(setq tab-always-indent t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;_. parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;;;_. newline & line-wrap
(setq require-final-newline 'ask)
(setq-default truncate-lines t)
(setq-default fill-column 100)
;;(auto-fill-mode t)

(global-set-key (kbd "C-c C-w") 'toggle-truncate-lines)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;;;_. changes
(if (require 'undo-tree nil 'noerror)
    (progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)
      )
  (message "%s: failed to load `undo-tree'."  load-file-name))

(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

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

(if (load "drag-stuff" t)
    (progn
;;    (setq drag-stuff-modifier 'hyper)
      (add-to-list 'drag-stuff-except-modes 'org-mode)
      (drag-stuff-global-mode t))
  (progn
      (global-set-key (kbd "<M-up>") 'swap-line-up)
      (global-set-key (kbd "<M-down>") 'swap-line-down)
      ))


;;;_. vi-style join-line
(defun join-line ()
  "Join the following line with current line"
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-c J") 'join-line)

;;;_. misc

(global-set-key (kbd "C-=") 'align-regexp)


;;;_ S(@* "completion")
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete))


;; Emacs default:
;;   M-TAB - lisp-complete-symbol(<24)/completion-at-point(v24)
;;   M-/ - dabbrev-expand

(global-set-key (kbd "M-/") 'hippie-expand)

;;;_. ido
(require 'ido)
(unless (fboundp 'ido-common-initialization)   ;;workaround for emacs 23.1's bug(?)
  (defun ido-common-initialization ()
    (ido-init-completion-maps)
    (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
    (add-hook 'choose-completion-string-functions 'ido-choose-completion-string))
  )
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
(ido-mode t)



;;;_. smex : ido for M-x
(if (require 'smex nil t)
    (progn
      (smex-initialize)
  
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      ;; This is your old M-x.
      (global-set-key (kbd "ESC M-x") 'execute-extended-command))
  (progn
    (message "%s: failed to load `smex'." load-file-name)
    ;;fall back to Emacs' icomplete-mode
    (icomplete-mode t)))

;;;_. anything
(if (and (load "anything" t)
         (load "anything-config" t))
    (progn
      ;;enable multiple keyword/regexp match
      ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
      ;;(global-set-key (kbd "M-x") 'anything-M-x)
  
      (global-set-key (kbd "<f5> r") 'anything-recentf)
      (global-set-key (kbd "<f5> b") 'anything-buffers+)
      (global-set-key (kbd "<f5> B") 'anything-bookmarks)
      (global-set-key (kbd "<f5> l") 'anything-locate)
      (global-set-key (kbd "<f5> c") 'anything-browse-code)
      (global-set-key (kbd "<f5> i") 'anything-imenu)
      (global-set-key (kbd "<f5> o") 'anything-occur)
      )
  (message "%s: failed to load `anything'." load-file-name))


;;;_. auto-compelte
(if (and (load "auto-complete" t)
         (load "auto-complete-config" t))
    (progn
      
      (ac-config-default)
      (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)
      
      ;;(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)

      (if (load "auto-complete-scite-api" t)
          (add-to-list 'ac-sources 'ac-source-scite-api)
        (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))
  (message "%s: failed to load `auto-complete'." load-file-name))



;;;_ S(@* "code folding")

;;;_. hideshow
(autoload 'hideshowvis-enable "hideshowvis" "Add markers to the fringe for regions foldable by `hideshow-mode'." t)
(autoload 'hideshowvis-minor-mode "hideshowvis" "Will indicate regions foldable with hideshow in the fringe." 'interactive)

(eval-after-load "hideshowvis" '(load "hideshow-fringe" t))

;;(add-hook 'emacs-lisp-mode-hook 'hideshowvis-enable)

;;(eval-after-load 'python
;;  (add-hook 'python-mode-hook 'hideshowvis-enable))

;(eval-after-load "hideshow"
;  (define-key hs-minor-mode-map (kbd "C-+")  'hs-toggle-hiding))

;;;_. outline

(global-set-key (kbd "<C-M-up>")     'outline-previous-visible-heading)
(global-set-key (kbd "<C-M-down>")   'outline-next-visible-heading)

(global-set-key (kbd "C-c <up>")     'outline-previous-visible-heading)
(global-set-key (kbd "C-c <down>")   'outline-next-visible-heading)

(global-set-key (kbd "<C-wheel-up>") 'outline-previous-visible-heading)
(global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
(global-set-key (kbd "<C-mouse-1>")  'outline-toggle-children)
(global-set-key (kbd "<C-mouse-2>")  'hide-sublevels)

;;;_. allout
(eval-after-load "allout"
  '(progn
     (define-key allout-mode-map (kbd "<C-M-up>")     'allout-previous-visible-heading)
     (define-key allout-mode-map (kbd "<C-M-down>")   'allout-next-visible-heading)

     (define-key allout-mode-map (kbd "<C-wheel-up>")   'allout-previous-visible-heading)
     (define-key allout-mode-map (kbd "<C-wheel-down>") 'allout-next-visible-heading)
     (define-key allout-mode-map (kbd "<C-mouse-1>")    'allout-hide-current-subtree)
     (define-key allout-mode-map (kbd "<C-mouse-3>")    'allout-show-current-subtree)
     ))


;;;_ S(@* "some visual effect")
;;;_. highlight-symbol
(idle-require 'highlight-symbol)

(global-set-key (kbd "C-c j")          'highlight-symbol-at-point)
(define-key search-map (kbd "j")        'highlight-symbol-at-point)
(define-key search-map (kbd "<up>")   'highlight-symbol-prev)
(define-key search-map (kbd "<down>") 'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

;;;_. bm
(idle-require 'bm)

(global-set-key (kbd "<C-f2>")    'bm-toggle)
(global-set-key (kbd "<M-f2>")    'bm-next)
(global-set-key (kbd "<S-f2>")    'bm-previous)

(global-set-key (kbd "<left-fringe> <C-mouse-1>")     'bm-toggle-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-up>")    'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-down>")  'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <C-mouse-2>")     'bm-show)


;;;_ S(@* "programming")

;;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(which-func-mode t)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)


(define-key goto-map "e" 'find-tag)

;;;_. compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)


;;;_ S(@* "buffer navigations")
(global-set-key (kbd "C-`") 'set-mark-command)
(global-set-key (kbd "M-`") 'cua-exchange-point-and-mark)
(global-set-key (kbd "C-M-`") 'pop-to-mark-command)


;;;_. imenu
(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)
(define-key goto-map "i" 'idomenu)
(define-key goto-map "I" 'imenu)

;;;_. recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)

;;;_ S(@* "misc")

;;;_. org-mode

(setq org-CUA-compatible t)

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil ;; no ToC in export
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css
      )

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)


;;;_. utils
(define-key goto-map "d" 'dired-jump) ;;C-x C-j

	 
  
