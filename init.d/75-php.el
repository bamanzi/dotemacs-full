
(autoload 'php-mode "php-mode" "PHP editing mode" t)
(add-to-list 'auto-mode-alist '("\\.php[s]?$\\|\\.phtm[l]?$" . php-mode))

;;(setq php-completion-file "~/.emacs.d/php-functions")

;; http://code.google.com/p/geben-on-emacs/
;;start xdebug client
(autoload 'geben "geben" "DBGp protocol front-end, PHP Debugger on Emacs" t)
