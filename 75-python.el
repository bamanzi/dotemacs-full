
(eval-after-load "python"
  `(progn

;;;_ epy
     ;; (require 'eepy-menu)
     ;; (require 'eepy-completion)
     ;; (require 'eepy-ropemacs)
     ;; (require 'eepy-pymisc)
     ;; (require 'eepy-misc)

;;;_ my enhancement for epy
;;;_. code folding
     (require 'hideshowvis)
     (require 'hideshow-fringe)
     (add-hook 'python-mode-hook 'hideshowvis-enable)

;;;_. view
     (autoload 'highlight-indentation "highlight-indentation" nil t)
     (add-hook 'python-mode-hook 'highlight-indentation)

;;;_. syntax check
;;;_ , pep8
;;;_ , pylint
;;;_ , pyflakes
;;;_ , pychecke


;;;_ python-mode
     (if nil
         (when (require 'python-mode nil t)
           (define-menu-for-epy py-mode-map)
           
           (setq outline-regexp "[[:space:]]*\\(?:\\(?:class\\|def\\)\\)\\_>")
           
           )
       )

     ))

