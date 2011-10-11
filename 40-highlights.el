;;;_ S(@* "some visual effect")
(column-number-mode t)

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
(global-set-key (kbd "<H-f2>")    'bm-show)

(global-set-key (kbd "<left-fringe> <C-mouse-1>")     'bm-toggle-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-up>")    'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-down>")  'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <C-mouse-2>")     'bm-show)
