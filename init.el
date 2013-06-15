
(server-start)

;;** main
(setq idle-require-idle-delay 5)
(setq idle-require-load-break 2)

(unless (load "idle-require" t)
  ;; fail-safe for `idle-quire'
  (defun idle-require (feature &optional file noerror)
    (require feature)))
;; (setq idle-require-symbols '(cedet nxml-mode)) ;; <- Specify packages here.


(defun load-file-if-not-loaded (file)
  (interactive "f")
  (with-current-buffer "*Messages*"
    (goto-char (point-min))
    (if (search-forward-regexp (concat "Loading .*" file ".*\.\.\.done") 
                                   nil 
                                   'noerror)
        (message "%s already load. skipped." file) ;;skip this file and return true
      (load-file file)
      )))



(setq dotemacs-dir (if load-file-name
                       (file-name-directory load-file-name)
                     default-directory))

(add-to-list 'load-path (concat dotemacs-dir "lisp/"))
(add-to-list 'exec-path (concat dotemacs-dir "bin/"))

(setenv "PATH" (concat dotemacs-dir "bin/" path-separator
                       "~/bin" path-separator
                       (getenv "PATH")))

(if (>= emacs-major-version 24)
    (add-to-list 'custom-theme-load-path (concat dotemacs-dir "themes")))

(add-to-list 'Info-default-directory-list (concat dotemacs-dir "info"))
(setq Info-directory-list nil)


(defun load-dotemacs-files ()
  "Load all my dotemacs files from ~/.emacs.d/init.d.

If any error occurs, it would exit. After you fixed the errors,
 you can use this command again (and again) , to finish all the dotemacs files."
  (interactive)
  (mapc '(lambda (file)
           (unless ;;;(ignore-errors
               (let ( (debug-on-error nil) )
                 (load-file-if-not-loaded file)
                 );;;)
             (message "Failed to load %s." file)))
        (directory-files (concat dotemacs-dir "init.d/") t "^[0-9].*\\.el$"))

  (when (fboundp 'idle-require-mode)
    (idle-require-mode t)))
  

(load-dotemacs-files)

