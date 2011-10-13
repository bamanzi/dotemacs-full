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


