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

        (add-to-list 'Info-default-directory-list
                     (expand-file-name "usr/share/info" cygwin-root-path))
        (add-to-list 'Info-default-directory-list
                     (expand-file-name "usr/local/share/info"  cygwin-root-path))

        (require 'woman)
        (add-to-list 'woman-manpath
                     (expand-file-name "usr/share/man" cygwin-root-path))
        (add-to-list 'woman-manpath
                     (expand-file-name "usr/local/share/man" cygwin-root-path))
        (add-to-list 'woman-manpath
                     (expand-file-name "usr/local/man" cygwin-root-path))

        ;;  (if (not (locate-file ispell-program-name) ;;FIXME:
        (setq ispell-program-name
              (expand-file-name "bin/aspell.exe" cygwin-root-path))
        )
    (message "Path not exist or not a directory: %s" cygwin-root-path)))

(when (eq system-type 'windows-nt)
  (require 'w32shell) ;;for msys-shell, cygwin-shell & cmd-shell

  (defcustom cygwin-root-path "e:/cygwin" "Root path of cygwin."
    :type '(directory))

  (cygwin-init-paths cygwin-root-path)

  )

;;;_ x window
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t)

  (global-unset-key (kbd "<menu>"))
  (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier)

;;  (define-key key-translation-map (kbd "<super>") (kbd "<f11>"))
  (define-key key-translation-map (kbd "<backtab>")         (kbd "<S-tab>"))
  (define-key key-translation-map (kbd "<C-S-iso-lefttab>") (kbd "<C-S-tab>"))


  (defun toggle-x-super-hyper ()
    (interactive)
    (if x-hyper-keysym
        (setq x-hyper-keysym nil)
      (setq x-hyper-keysym 'super))

    (message "Now %s modifier is on SUPER key."
             (if x-hyper-keysym "super" "hyper"))
    )
  )


(defun map-mintty-keys ()
  ;; Mintty supports most combo keys (even telnet/ssh to another server)
  ;; such as C-%, C-&, C-(, C-., C-f1, M-f1, S-f1... while putty doesn't
  ;;   http://code.google.com/p/mintty/wiki/Keycodes
  (define-key key-translation-map (kbd "M-[ 1;5l") (kbd "C-,"))
  (define-key key-translation-map (kbd "M-[ 1;5n") (kbd "C-."))

  ;; The following are not recognizable on xterm:
  ;;   C-[, C-], C-{, C-}, C-\, C-|, C-/, C-?
  ;; The following are ambigious
  ;;   C-RET, C-backspace
  
  ;; iTerm meta-shift-<arrows> fix
  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])
  )

(defun config-for-linux-console ()
  (gpm-mouse-mode t) ;;for Linux console
  )

(defun config-for-linux-xterm ()
  (xterm-mouse-mode t)
  (require 'ext-mouse nil t)
  
  )

;;;_ xterm & console
(when (eq system-type 'gnu/linux)
  (if (not window-system)
      ;;FIXME: if xterm ?
      (config-for-linux-xterm)
      ;;(config-for-linux-console)
    )


  (load-library "help-mode")  ;; to avoid the error message:
                              ;;; "Symbol's value as variable is void: help-xref-following"
  )

