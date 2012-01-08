;;* JavaScript
;;** major modes

(autoload 'espresso-mode "espresso" nil t)
;;espresso-mode entered GNU Emacs 23.2
(if (string< emacs-version "23.2")
    (add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode)))

;; js2-mode has better imenu support for extjs style object method
(autoload 'js2-mode      "js2-mode" nil t)

(autoload 'javascript-mode  "javascript"
  "Major mode for editing JavaScript source text." t)

;;*** keybindings
(defvar bmz-js-keys-mode-map (make-sparse-keymap)
  "My keybindings for all javascript major modes.")

(define-minor-mode bmz-js-keys-mode
  "A dummy minor mode, just to inject my keybindigs into different JavaScript
major modes."
  nil
  nil
  bmz-js-keys-mode-map)

(defun turn-on-bmz-js-keys-mode ()
  (interactive)
  (bmz-js-keys-mode t))

(eval-after-load "espresso"
  `(add-hook 'espresso-mode-hook    'turn-on-bmz-js-keys-mode))
  
(eval-after-load "js"
  `(add-hook 'js-mode-hook          'turn-on-bmz-js-keys-mode))

(eval-after-load "js2-mode"
  `(add-hook 'js2-mode-hook         'turn-on-bmz-js-keys-mode))

(eval-after-load "javascript"
  `(add-hook 'javascript-mode-hook  'turn-on-bmz-js-keys-mode))



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

;;*** rhino + jslint
;;TODO: implement this
(defun jslint-with-rhino ()
  (interactive)
  (let ( (compile-command (concat "jslint-rhino " buffer-file-name)) )
    (call-interactively 'compile)))


;;*** keybindings
(define-key bmz-js-keys-mode-map (kbd "<M-f9>")  'jslint-with-v8)


;;** comint (interactive shell)
;; http://js-comint-el.sourceforge.net/
(autoload 'run-js "js-comint"
  "Run an inferior Javascript process, input and output via buffer `*js*'." t)

(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(if (eq system-type 'windows-nt)
    (setq inferior-js-program-command "jsdb")
  (setq inferior-js-program-command "/usr/bin/node"))

;;FIXME: only works with js2-mode?
(define-key bmz-js-keys-mode-map (kbd "C-x C-e") 'js-send-last-sexp)
(define-key bmz-js-keys-mode-map (kbd "C-x C-E") 'js-send-last-sexp-and-go)
;;(define-key bmz-js-keys-mode-map "\C-cb" 'js-send-buffer)
;;(define-key bmz-js-keys-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
;;(define-key bmz-js-keys-mode-map "\C-cl" 'js-load-file-and-go)
(define-key bmz-js-keys-mode-map (kbd "C-c C-r") 'js-send-region)
(define-key bmz-js-keys-mode-map (kbd "C-c C-R") 'js-send-region-and-go)
			    

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
