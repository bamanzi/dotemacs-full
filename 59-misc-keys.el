;; toggle minor modes
(global-set-key (kbd "<f10> hc") 'highlight-changes-visible-mode)
(global-set-key (kbd "<f10> af") 'auto-fill-mode)
(global-set-key (kbd "<f10> sp") 'show-paren-mode)
(global-set-key (kbd "<f10> W") 'whitespace-mode)
(global-set-key (kbd "<f10> hs") 'hs-minor-mode)
(global-set-key (kbd "<f10> O") 'outline-minor-mode)
(global-set-key (kbd "<f10> R") 'ruler-mode)
(global-set-key (kbd "<f10> tl") 'toggle-truncate-lines)
(global-set-key (kbd "<f10> C-w") 'visual-line-mode)
(global-set-key (kbd "<f10> L") 'linum-mode)
(global-set-key (kbd "<f10> V") 'toggle-viper-mode)


;;;_. hyper keys
(global-set-key (kbd "H-a") 'mark-whole-buffer)
;; H-s
;; H-d
;; H-f

(global-set-key (kbd "H-z") 'undo-tree-undo)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-c") 'kill-ring-save)
(global-set-key (kbd "H-v") 'cua-paste)


(global-set-key (kbd "H-b") 'select-parened-expression)

;; H-q
(global-set-key (kbd "H-w") 'toggle-truncate-lines)
(global-set-key (kbd "H-e") 'kill-whole-line)
;; H-r
(global-set-key (kbd "H-t") 'transpose-selections)

(global-set-key (kbd "H-g") 'keyboard-quit)

(global-set-key (kbd "H-y") 'undo-tree-redo)
(global-set-key (kbd "H-l") 'copy-line)


