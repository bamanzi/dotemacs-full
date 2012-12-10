;;** AutoHotKey
(setq autohotkey-dir "d:/Programs/AutoHotKey/")


(defun compile-autohotkey ()
  (interactive)
  (let ( (compile-command (concat autohotkey-dir
                                  "Compiler/ahk2exe.exe /in "
                                  buffer-file-name)) )
    (call-interactively 'compile)))

(defun run-autohotkey ()
  (interactive)
  (async-shell-command (concat autohotkey-dir
                               "AutoHotkey.exe /ErrorStdOut "
                               buffer-file-name)))

(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))

;;*** ahk-mode
;; (this one is better on syntax highlighting, code indentation)
;; https://github.com/piyo/emacs-ahk-mode  (fixed some indentation problems)
(autoload 'ahk-mode "ahk-mode")
(defun ahk-mode-my-init ()
  (setq indent-tabs-mode t)
  (setq ahk-indetion tab-width)

  (modify-syntax-entry ?\; "< b") ;;comment start
  (modify-syntax-entry ?\n "> b") ;;comment end
s
  (if (boundp 'ac-source-scite-api)
      (add-to-list 'ac-sources 'ac-source-scite-api))

  (define-key ahk-mode-map (kbd "<C-f9>") 'compile-autohotkey)
  (define-key ahk-mode-map (kbd "<f9>")   'run-autohotkey)
  )

(setq ahk-syntax-directory (concat autohotkey-dir "Extras/Editors/Syntax/"))
(eval-after-load "ahk-mode"
  '(add-hook 'ahk-mode-hook 'ahk-mode-my-init))

(add-to-list 'which-func-modes 'ahk-mode)


;;*** xahk-mode
(autoload 'xahk-mode "xahk-mode")
(defun xahk-mode-my-init ()
  (setq indent-tabs-mode t)
  (setq c-basic-offset tab-width)
  (setq mode-name "xahk")
  
  (modify-syntax-entry ?_ "w") ;; '_' is part of a symbol
  (modify-syntax-entry ?# "_")
  (modify-syntax-entry ?* ".")
  
  (if (boundp 'ac-source-scite-api)
      (add-to-list 'ac-sources 'ac-source-scite-api))

  (define-key xahk-mode-map (kbd "<C-f9>") 'compile-autohotkey)
  (define-key xahk-mode-map (kbd "<f9>")   'run-autohotkey)
  )

(eval-after-load "xahk-mode"
  '(add-hook 'xahk-mode-hook 'xahk-mode-my-init))

(add-to-list 'which-func-modes 'xahk-mode)

