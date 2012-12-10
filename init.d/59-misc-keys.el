;;* toggle minor modes
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


;;** hyper keys
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


;;** Insert Brackets by Pair
;; http://xahlee.org/emacs/emacs_insert_brackets_by_pair.html

(defun insert-bracket-pair (leftBracket rightBracket)
  "Insert a matching bracket and place the cursor between them."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) ) )
  (insert leftBracket rightBracket)
  (backward-char 1) )

(defun insert-pair-paren () (interactive) (insert-bracket-pair "(" ")") )
(defun insert-pair-brace () (interactive) (insert-bracket-pair "{" "}") )
(defun insert-pair-bracket () (interactive) (insert-bracket-pair "[" "]") )

(defun insert-pair-double-straight-quote () (interactive) (insert-bracket-pair "\"" "\"") )
(defun insert-pair-single-straight-quote () (interactive) (insert-bracket-pair "'" "'") )

(global-set-key (kbd "H-(") 'insert-pair-paren)              ;()
(global-set-key (kbd "H-{") 'insert-pair-brace)              ;{}
(global-set-key (kbd "H-[") 'insert-pair-bracket)            ;[]
(global-set-key (kbd "H-\"") 'insert-pair-double-straight-quote) ; "
(global-set-key (kbd "H-\'") 'insert-pair-single-straight-quote) ; '

(define-key key-translation-map (kbd "H-.") (kbd "=")) ; equal
(define-key key-translation-map (kbd "H-,") (kbd "+")) ; plus

(defun insert-pair-corner-bracket () (interactive) (insert-bracket-pair "「" "」") )
(defun insert-pair-white-corner-bracket () (interactive) (insert-bracket-pair "『" "』") )
(defun insert-pair-angle-bracket () (interactive) (insert-bracket-pair "〈" "〉") )
(defun insert-pair-double-angle-bracket () (interactive) (insert-bracket-pair "《" "》") )
(defun insert-pair-white-lenticular-bracket () (interactive) (insert-bracket-pair "〖" "〗") )
(defun insert-pair-black-lenticular-bracket () (interactive) (insert-bracket-pair "【" "】") )
(defun insert-pair-tortoise-shell-bracket () (interactive) (insert-bracket-pair "〔" "〕") )

(defun insert-pair-fullwith-paren () (interactive) (insert-bracket-pair "（" "）") )
(defun insert-pair-fullwith-bracket () (interactive) (insert-bracket-pair "［" "］") )
(defun insert-pair-fullwith-brace () (interactive) (insert-bracket-pair "｛" "｝") )

(defun insert-pair-white-paren () (interactive) (insert-bracket-pair "⦅" "⦆") )
(defun insert-pair-white-bracket () (interactive) (insert-bracket-pair "〚" "〛") )
(defun insert-pair-white-brace () (interactive) (insert-bracket-pair "⦃" "⦄") )

(global-set-key (kbd "H-A-'") 'insert-pair-single-curly-quote) ;‘’
(global-set-key (kbd "H-A-\"") 'insert-pair-double-curly-quote) ;“”
(global-set-key (kbd "H-A-<") 'insert-pair-single-angle-quote) ;‹›
;; (global-set-key (kbd "H-M-<") 'insert-pair-double-angle-quote) ;«»

;; (global-set-key (kbd "H-k") 'insert-pair-corner-bracket) ;「」
;; (global-set-key (kbd "H-K") 'insert-pair-white-corner-bracket) ;『』
;; (global-set-key (kbd "H-j") 'insert-pair-angle-bracket)        ;〈〉
;; (global-set-key (kbd "H-J") 'insert-pair-double-angle-bracket) ;《》
(global-set-key (kbd "H-A-[") 'insert-pair-black-lenticular-bracket) ;【】
;; (global-set-key (kbd "H-Q") 'insert-pair-white-lenticular-bracket) ;〖〗
;; (global-set-key (kbd "H-;") 'insert-pair-tortoise-shell-bracket)   ;〔〕

