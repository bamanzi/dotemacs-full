
;;* ruby
;;** code folding
(defun bmz/ruby-mode-init-folding ()
  (setq outline-regexp " *\\(def \\|class\\|module\\)")

  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (arg) (ruby-end-of-block))
                 nil
                 ))
  )

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'bmz/ruby-mode-init-folding)
     ))

;;** inf-ruby
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
         ))

;;** code completion
;;...


;;** rdebug
;;for ruby > 1.9, install 'debugger' gem
;;
;;(add-to-list 'load-path "/usr/lib/ruby/gems/1.9.1/debugger-1.6.0/emacs")
(autoload 'rdebug "rdebug-core"
  "Invoke the rdebug Ruby debugger and start the Emacs user interface." t)
(eval-after-load "rdebug"
  `(progn
    ;;rdebug-core requires package `gdb-ui', but emacs 24 renamed it to gdb-mi
    (unless (locate-library "gdb-ui")
      (require 'gdb-mi)
      (provide 'gdb-ui))))


;;** misc
;;*** ruby-block
(autoload 'ruby-block-mode  "ruby-block"
  "In ruby-mode, Displays the line where there is keyword corresponding" t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'ruby-block-mode)

;;*** rake
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))


;;* ruby on rails
;;** rhtml-mode for editing html template
(autoload 'rhtml-mode "rhtml-mode"
  "Embedded Ruby Mode (RHTML)" t)

;;DONE: web-mode is better
;;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))


;;** rinari: rails code navigation & utils
;;http://rinari.rubyforge.org/
(autoload 'rinari-launch "rinari"
  "Call function `rinari-minor-mode' if inside a rails project." t)

(eval-after-load "ruby-mode"
  `(progn
     (add-hook 'ruby-mode-hook 'rinari-launch)))

(eval-after-load "web-mode"
  `(progn
     (add-hook 'web-mode-hook 'rinari-launch)))

(eval-after-load "rhtml-mode"
  `(progn
     (add-hook 'rhtml-mode-hook 'rinari-launch)))

(eval-after-load "yaml-mode"
  `(progn
     (add-hook 'yaml-mode-hook 'rinari-launch)))

;;** yaml-mode
(autoload 'yaml-mode  "yaml-mode"
  "Simple mode to edit YAML." t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
