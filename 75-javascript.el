;;;_. major modes

;; js2-mode has better imenu support for extjs style object method
(autoload 'js2-mode      "js2-mode" nil t)

(autoload 'espresso-mode "espresso" nil t)

(if (>= emacs-major-version 24)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode)))

;;;_. code folding
(defun js-mode-init-fold ()
  (if (require 'hideshowvis nil t)
      (hideshowvis-enable)
    (hs-minor-mode t))
  
  ;;TODO:
;;  (setq outline-regexp "...")
  )

;;;_. json-pretty-print

;;;_. code completion
;;;_.. autocomplete + semantic
(defun js-mode-init-auto-complete ()
  (add-to-list 'ac-modes 'espresso-mode)
  
  )
;;;_. lint
;;;_.. v8 (http://koansys.com/tech/flymake-mode-for-emacs-javascript-v8-edition)
(defun js-lint-with-v8 ()
  (let ( (compile-command (concat "jslint-v8 --vim " buffer-file-name)) )
    (call-interactively 'compile)))

(defun js-lint-with-jshint ()
  (let ( (compile-command (concat "jshint " buffer-file-name)) )
    (call-interactively 'compile)))

(defun js-lint-with-jsl ()
  (let ( (compile-command (concat "jsl process " buffer-file-name)) )
    (call-interactively 'compile)))

(defun js-lint-with-jsdb ()
  (let ( (compile-command (concat "jslint-jsdb " buffer-file-name)) )
    (call-interactively 'compile)))

(eval-after-load "espresso"
  '(define-key espresso-mode (kbd "<M-f9>")  'js-lint-with-v8))


;;;_. misc
;;;_.. which-func-mode
(add-to-list 'which-func-modes 'js-mode)
(add-to-list 'which-func-modes 'ecmacript-mode)
(add-to-list 'which-func-modes 'js2-mode)
(add-to-list 'which-func-modes 'espresso-mode)

