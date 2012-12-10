;;{{{ packages, load-path and related

;; (loop for f in (directory-files "~/.emacs.d/init.d" nil ".*.el")
;;       by (message "loading %s" f))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el") t)
  (package-initialize))

(when (file-exists-p "~/.emacs.d/site-lisp/site-start.el")
  (load "~/.emacs.d/site-lisp/site-start.el"))

(add-to-list 'image-load-path "~/.emacs.d/etc/images/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(add-to-list 'load-path "~/.emacs.d/mylisp/")

;; leave customization & os/distro/installation-specific settings to another file
;; (customizations, paths, theme)
(setq custom-file "~/.emacs.d/customize.el")
(if (file-exists-p custom-file)
    (load custom-file))

;;}}}


;; ;;{{{ generate autoload and load them
;; (defun generate-dir-autoload-and-load (top-dir)
;;   (let ( (generated-autoload-file "~/.emacs.d/autoloads.el") )
;;     (mapc '(lambda (dir)
;;              (update-autoloads-from-directories dir))
;;           '("e:/emacs/site-lisp/misc_"
;;             "e:/emacs/site-lisp/filetype_"
;;             "e:/emacs/site-lisp/prog_"
;;             "e:/emacs/site-lisp/win32_"
;;             ))
;;     (load-file generated-autoload-file)))



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

;;test: (load-file-if-not-loaded "~/.emacs.d/init.d/20-windows.el")

(defun load-dotemacs-files ()
  "Load all my dotemacs files from ~/.emacs.d/init.d.

If any error occurs, it would exit. After you fixed the errors, you can use this command again (and again) , to finish all the dotemacs files."
  (interactive)
  (mapc '(lambda (file)
           (unless ;;;(ignore-errors
               (let ( (debug-on-error nil) )
                 (load-file-if-not-loaded file)
                 );;;)
             (message "Failed to load %s." file)))
        (directory-files "~/.emacs.d/init.d/" t "^[0-9].*\\.el$")))

(load-dotemacs-files)


(when nil
  ;; for debugging initscripts
  (progn
    (insert-string "\n  (progn\n")
    (mapc '(lambda (file)
             (insert-string (format "   (load \"%s\")\n" file)))
          (directory-files "~/.emacs.d/init.d/" t "[0-9[0-9].*\.el$"))
      (insert-string ")  \n"))  ;;put cursor on next line, and press C-x C-e
  
  (progn
   (load "/home/bamanzi/.emacs.d/init.d/00-init.el")
   (load "/home/bamanzi/.emacs.d/init.d/10-emacs-env.el")
   (load "/home/bamanzi/.emacs.d/init.d/10-essential.el")
   (load "/home/bamanzi/.emacs.d/init.d/20-buffers.el")
   (load "/home/bamanzi/.emacs.d/init.d/20-files.el")
   (load "/home/bamanzi/.emacs.d/init.d/20-windows.el")
   (load "/home/bamanzi/.emacs.d/init.d/25-minibuffer.el")
   (load "/home/bamanzi/.emacs.d/init.d/25-tabbar.el")
   (load "/home/bamanzi/.emacs.d/init.d/25-win-keys.el")
   (load "/home/bamanzi/.emacs.d/init.d/30-dict-spell.el")
   (load "/home/bamanzi/.emacs.d/init.d/30-shell.el")
   (load "/home/bamanzi/.emacs.d/init.d/40-completion.el")
   (load "/home/bamanzi/.emacs.d/init.d/40-edit-basic.el")
   (load "/home/bamanzi/.emacs.d/init.d/40-fold-nav.el")
   (load "/home/bamanzi/.emacs.d/init.d/40-highlights.el")
   (load "/home/bamanzi/.emacs.d/init.d/40-symbol-fns.el")
   (load "/home/bamanzi/.emacs.d/init.d/49-edit-misc.el")
   (load "/home/bamanzi/.emacs.d/init.d/50-cua-keys.el")
   (load "/home/bamanzi/.emacs.d/init.d/50-vi-emu.el")
   (load "/home/bamanzi/.emacs.d/init.d/50-vi-keys.el")
   (load "/home/bamanzi/.emacs.d/init.d/59-misc-keys.el")
   (load "/home/bamanzi/.emacs.d/init.d/59-mouse.el")
   (load "/home/bamanzi/.emacs.d/init.d/60-prog-basic.el")
   (load "/home/bamanzi/.emacs.d/init.d/65-cedet-ecb.el")
   (load "/home/bamanzi/.emacs.d/init.d/65-prog-help.el")
   (load "/home/bamanzi/.emacs.d/init.d/70-file-assoc.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-delphi.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-elisp.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-javascript.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-org-mode.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-python.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-sh-script.el")
   (load "/home/bamanzi/.emacs.d/init.d/75-xml.el")
   (load "/home/bamanzi/.emacs.d/init.d/79-autohotkey.el")
   (load "/home/bamanzi/.emacs.d/init.d/79-misc-lang.el")
   (load "/home/bamanzi/.emacs.d/init.d/80-ports.el")
   (load "/home/bamanzi/.emacs.d/init.d/90-foobar.el")
   (load "/home/bamanzi/.emacs.d/init.d/90-fun.el")
   (load "/home/bamanzi/.emacs.d/init.d/90-unicode.el")
   (load "/home/bamanzi/.emacs.d/init.d/95-one-key.el")
   (load "/home/bamanzi/.emacs.d/init.d/99-temp.el")
)  
  
  
 
  )




(when (fboundp 'idle-require-mode)
  (idle-require-mode t))

;;restore stub
;(defun idle-require (feature)
;  (require feature nil t))

