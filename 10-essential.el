;;;_ S(@* "gui options")
(setq use-dialog-box nil
      menu-prompting nil)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)
    )

(setq mouse-yank-at-point t) ;;rather than the click point

;;;_. key modifiers and prefix keys

(when (eq window-system 'w32)
  ;;(setq w32-lwindow-modifier 'super)
  
  (setq w32-lwindow-modifier 'super) 
  (setq w32-pass-lwindow-to-system nil) ;;if set to nil, a single press <lwindow> would prevent Start Menu

  (setq w32-rwindow-modifier 'alt)      
  (setq w32-pass-rwindow-to-system nil)
  
  (setq w32-apps-modifier 'hyper)
  (setq w32-pass-apps-to-system nil)
  
  (setq w32-scroll-lock-modifier nil)
  )
  
;;FIXME: not work
(when (eq window-system 'x)
  (global-unset-key (kbd "<menu>"))
  (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier)  
  )


;;;_ S(@* "editing")
(global-set-key (kbd "C-`")   'set-mark)
;;(global-set-key (kbd "M-`") 'exchange-point-and-mark)
(global-set-key (kbd "M-`")   'pop-to-mark-command)
(global-set-key (kbd "C-M-`") 'pop-mark)

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
(setq require-final-newline 't)
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

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)


(idle-require 'drag-stuff)
(eval-after-load "drag-stuff"
  '(progn
;;    (setq drag-stuff-modifier 'hyper)
      (add-to-list 'drag-stuff-except-modes 'org-mode)
      (drag-stuff-global-mode t)))

;;;_. misc
(defun join-line ()
  "Join the following line with current line"
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-c J") 'join-line)

(global-set-key (kbd "C-=") 'align-regexp)


;;;_ S(@* "anything")

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

      (define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)
      )
  (message "%s: failed to load `anything'." load-file-name))

;;;_ S(@* "programming")

;;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(which-func-mode t)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(define-key goto-map "e" 'find-tag)

;;;_. compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))
(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)


;;;_ S(@* "buffer navigations")
;;;_. imenu
(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)
(define-key goto-map "i" 'idomenu)
(define-key goto-map "I" 'imenu)

(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<S-f2>") 'bm-next)
(global-set-key (kbd "<M-f2>") 'bm-previous)


;;;_. recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)


;;;_ S(@* "misc")

;;;_. highlight-symbol
(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" nil t)

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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c q") 'org-capture)


;;;_. utils
(define-key goto-map "d" 'dired-jump) ;;C-x C-j

	 
  
