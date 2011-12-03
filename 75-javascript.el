;;* JavaScript
;;** major modes

;; js2-mode has better imenu support for extjs style object method
(autoload 'js2-mode      "js2-mode" nil t)

(autoload 'espresso-mode "espresso" nil t)

(if (>= emacs-major-version 24)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode)))

;;** code folding
(defun js-mode-init-fold ()
  (if (require 'hideshowvis nil t)
      (hideshowvis-enable)
    (hs-minor-mode t))
  
  ;;TODO:
;;  (setq outline-regexp "...")
  )


;;**  code completion
;; autocomplete + semantic
(defun js-mode-init-auto-complete ()
  (add-to-list 'ac-modes 'espresso-mode)
  
  )

;;** lint
;;*** jslint-v8 (http://koansys.com/tech/flymake-mode-for-emacs-javascript-v8-edition)
(defun jslint-with-v8 ()
  (interactive)
  (let ( (compile-command (concat "jslint-v8 --vim " buffer-file-name)) )
    (call-interactively 'compile)))

;;*** jshint (powered by jsdb intepreter)
;;    https://github.com/spytheman/jshint-cli-with-jsdb
(defun jslint-with-jshint ()
  (interactive)
  (let ( (compile-command (concat "jshint " buffer-file-name)) )
    (call-interactively 'compile)))

;;*** jsl (available only on windows)
;;    http://www.javascriptlint.com
(defun jslint-with-jsl ()
  (interactive)
  (let ( (compile-command (concat "jsl process " buffer-file-name)) )
    (call-interactively 'compile)))

;;*** nodejs + jslint
;;   https://github.com/timemachine3030/node-jslint
(defun jslint-with-node ()
  (interactive)
  (let ( (compile-command (concat "jslint-node " buffer-file-name)) )
    (call-interactively 'compile)))

;;*** rhino (powered by jsdb intepreter)
;;    https://github.com/spytheman/jshint-cli-with-jsdb
(defun jslint-with-rhino ()
  (interactive)
  (let ( (compile-command (concat "jslint-rhino " buffer-file-name)) )
    (call-interactively 'compile)))

;;*** rhino + jslint
;;.....

;;*** keybindings
(eval-after-load "espresso"
  '(define-key espresso-mode (kbd "<M-f9>")  'jslint-with-node))



;;** misc
;;*** which-func-mode
(add-to-list 'which-func-modes 'js-mode)
(add-to-list 'which-func-modes 'ecmacript-mode)
(add-to-list 'which-func-modes 'js2-mode)
(add-to-list 'which-func-modes 'espresso-mode)

;;*** json-pretty-print

(defun json-pretty-print (begin end)
  (interactive "r")
  (shell-command-on-region begin end "python -mjson.tool" nil 'replace))
