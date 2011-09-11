;;;_ win32
(when (eq window-system 'w32)
  (setq w32-lwindow-modifier nil)
  (setq w32-pass-lwindow-to-system nil)
  (define-key key-translation-map (kbd "<lwindow>") (kbd "<f11>"))

  (setq w32-rwindow-modifier 'super)
  (setq w32-pass-rwindow-to-system nil)

  (setq w32-apps-modifier 'hyper)
  
  (setq w32-scroll-lock-modifier nil)


    (defun toggle-w32-alt-is-meta ()
    (interactive)
    (setq w32-alt-is-meta (not w32-alt-is-meta))
    (setq w32-lwindow-modifier (if w32-lwindow-modifier
                                   nil
                                 'hyper))
    (message "Now ALT key %s META. LWin is %s."
             (if w32-alt-is-meta "is" "is not")
             w32-lwindow-modifier)
    )

  (global-set-key (kbd "<scroll>") 'toggle-w32-alt-is-meta)
  )

;;;_. cygwin
(defun cygwin-init-paths (cygwin-root-path)
  (if (file-directory-p cygwin-root-path)
      (progn
        ;;(add-to-list 'exec-path (expand-file-name "bin" cygwin-root-path)) ;;NOT recommended!!
        
        (add-to-list 'Info-default-directory-list (expand-file-name "usr/share/info" cygwin-root-path))
        (add-to-list 'Info-default-directory-list (expand-file-name "usr/local/share/info"  cygwin-root-path))

        (require 'woman)
        (add-to-list 'woman-manpath (expand-file-name "usr/share/man" cygwin-root-path))
        (add-to-list 'woman-manpath (expand-file-name "usr/local/share/man" cygwin-root-path))
        (add-to-list 'woman-manpath (expand-file-name "usr/local/man" cygwin-root-path))

        ;;  (if (not (locate-file ispell-program-name) ;;FIXME:
        (setq ispell-program-name (expand-file-name "bin/aspell.exe" cygwin-root-path))
        )
    (message "Path not exist or not a directory: %s" cygwin-root-path)))

(when (eq system-type 'windows-nt)
  (require 'w32shell) ;;for msys-shell, cygwin-shell & cmd-shell

  (defvar cygwin-root-path "e:/cygwin" "Root path of cygwin.")

  (cygwin-init-paths cygwin-root-path)

  )

;;;_ x window
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t)

  (global-unset-key (kbd "<menu>"))
  (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier)

;;  (define-key key-translation-map (kbd "<super>") (kbd "<f11>"))
  )


;;;_ xterm & console
(when (and (not window-system)
           (eq system-type 'gnu/linux))
  (xterm-mouse-mode t)
  ;;(gpm-mouse-mode t) ;;for Linux console

  
  )
               
