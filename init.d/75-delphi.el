
;;use pascal-mode instead of delphi-mode
;;(because you can't use either `highlight-symbol', `highlight-indentation' etc,
;; nor `font-lock-add-keywords' with `delphi-mode')

(add-to-list 'auto-mode-alist '("\\.dpr$" . pascal-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . pascal-mode))

(eval-after-load "pascal"
  '(progn
     (require 'delphi-ext nil t)))

(eval-after-load "delphi"
  '(progn
     (require 'delphi-ext nil t)))
