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
    
  (if (boundp 'ac-source-scite-api)
      (add-to-list 'ac-modes 'ac-source-scite-api))

  (define-key ahk-mode-map (kbd "<C-f9>") 'compile-autohotkey)
  (define-key ahk-mode-map (kbd "<f9>")   'run-autohotkey)  
  )

(setq ahk-syntax-directory (concat autohotkey-dir "Extras/Editors/Syntax/")
(eval-after-load "ahk-mode"
  '(add-hook 'ahk-mode-hook 'ahk-mode-my-init))

(add-to-list 'which-func-modes 'ahk-mode)


;;*** xahk-mode
(autoload 'xahk-mode "xahk-mode")
(defun xahk-mode-my-init ()
  (setq indent-tabs-mode t)
  (setq c-basic-offset tab-width)
  
  (modify-syntax-entry ?_ "_") ;; '_' is part of a symbol
  (modify-syntax-entry ?# "_")
  
  (if (boundp 'ac-source-scite-api)
      (add-to-list 'ac-modes 'ac-source-scite-api))

  (define-key xahk-mode-map (kbd "<C-f9>") 'compile-autohotkey)
  (define-key xahk-mode-map (kbd "<f9>")   'run-autohotkey)  
  )

(eval-after-load "xahk-mode"
  '(add-hook 'xahk-mode-hook 'xahk-mode-my-init))

(add-to-list 'which-func-modes 'xahk-mode)


;;** awk
(defun anything-info-awk ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-gawk )))

(eval-after-load "cc-mode"
  `(define-key awk-mode-map (kbd "<M-f1>") 'anything-info-awk))

;;** shell script
(defun anything-info-shell-script ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-bash
;;         anything-c-source-info-zsh
;;         anything-c-source-info-sh-utils
         anything-c-source-info-coreutils
;;         anything-c-source-info-textutils
;;         anything-c-source-info-fileutils
         anything-c-source-man-pages
         )))

(eval-after-load "sh-script"
  `(define-key sh-mode-map (kbd "<M-f1>") 'anything-info-shell-script))
