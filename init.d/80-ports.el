;;** win32
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

;;** cygwin
;;DOC: http://www.emacswiki.org/emacs/CygwinizedEmacsHOWTO
;;DOC: http://www.khngai.com/emacs/cygwin.php
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

(defun cygwin-env-on ()
  (interactive)

  ;; use cygwin bash as default shell
  ;;(w32shell-set-shell "cygwin")
  (setq explicit-shell-file-name (concat cygwin-root-path "/bin/bash")) ;; for M-x shell
  (setq explicit-bash-args   '("--login" "-i"))
  ;;NOTE: edit ~/.emacs_bash or ~/.emacs.d/init_bash.sh to customize special settings
  ;; for running bash in Emacs (refer `shell' for detail)
;;  (setenv "SHELL" shell-file-name)  ;;TODO: for what?

  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)


  ;; use bash for executing shell commands (implicitly)  
  (setq shell-file-name explicit-shell-file-name)  ;;for executing external programs
  
  (add-to-list 'exec-path (concat cygwin-root-path "/bin"))
  (add-to-list 'exec-path (concat cygwin-root-path "/usr/local/bin"))
  (setq env-path-before-cygwin (getenv "PATH"))
  (setenv "PATH" (concat cygwin-root-path "/bin" path-separator
                       cygwin-root-path "/usr/local/bin" path-separator
                       (getenv "PATH")))

;;  (setq tramp-default-method "scpx")
  
  (if (require 'cygwin-mount nil t)
      (cygwin-mount-activate))
  )
  
  
(defun cygwin-env-off ()
  (interactive)
  (setq explicit-shell-file-name  nil)
  (setq shell-file-name "cmdproxy.exe")

  (setq exec-path
        (remove (concat cygwin-root-path "/usr/local/bin")
                (remove (concat cygwin-root-path "/bin") exec-path)))
  (setenv "PATH" env-path-before-cygwin)

  ;;(setq tramp-default-method "pscp")

  (remove-hook 'comint-output-filter-functions
               'shell-strip-ctrl-m t)
  (remove-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt t)

  (if (featurep 'cygwin-mount)
      (cygwin-mount-deactivate))
  )


;;** x window
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


;;** xterm & console

(defun bmz/config-for-linux-console ()
  (gpm-mouse-mode t) ;;for Linux console
  )

(defun bmz/config-for-linux-xterm ()
  (xterm-mouse-mode t)
  (require 'ext-mouse nil t)
  
  )

(defun bmz/config-for-term-common ()
  (define-key key-translation-map  (kbd "ESC <up>")    (kbd "<M-up>"))
  (define-key key-translation-map  (kbd "ESC <down>")  (kbd "<M-down>"))
  (define-key key-translation-map  (kbd "ESC <left>")  (kbd "<M-left>"))
  (define-key key-translation-map  (kbd "ESC <right>") (kbd "<M-right>"))


  )
  
(when (eq system-type 'gnu/linux)
  (if (not window-system)
      (if (string= (getenv "TERM") "linux") ;;FIXME: if xterm ?
          (bmz/config-for-linux-console)
        (bmz/config-for-linux-xterm))
    )

  ;;(define-key key-translation-map (kbd "<select>") (kbd "<end>"))
  
  (load-library "help-mode")  ;; to avoid the error message:
                              ;;; "Symbol's value as variable is void: help-xref-following"
  )


;; better menu navigation
(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)


;;*** keychords on different terminals
  ;;not bound by Emacs, but mostly recognizable (at least when ESC used as Meta)
  ;; M-#
  ;; M-+  
  ;; M-_
  ;; M-]
  ;; M-"
  ;; M-? 

;; not available on term:
  ;;C-`: n.a. 
  ;;C-~: n.a. 
  ;;C-!: n.a. 
  ;;C-@: n.a.
  ;;C-#: n.a.
  ;;C-$: n.a.

  ;;C-: n.a.
  ;;C-; n.a.
  ;;C-' n.a.
  ;;C-" n.a.

;; others: 
  ;;C-[ -> esc
  ;;C-]: bound to `abort-recursive-edit'

  ;;C-\ bound to `toggle-input-method'
  ;;C-| \234    

  ;;C-{: \233
  ;;C-}: \235

  ;;C-/           -> C-_
  ;;C-?           -> DEL

  ;;C-backspace   -> C-_
  ;;C-RET         -> C-^

(defun map-mintty-keys ()
  ;; Mintty supports most combo keys (even telnet/ssh to another server)
  ;; such as C-%, C-&, C-(, C-., C-f1, M-f1, S-f1... while putty doesn't
  ;;   http://code.google.com/p/mintty/wiki/Keycodes

  (define-key input-decode-map (kbd "M-[ 1;6u") (kbd "C-%"))
  ;;C-^: recognizable on xterm & xshell, not bound
  (define-key input-decode-map (kbd "M-[ 1;6w") (kbd "C-&"))
  (define-key input-decode-map (kbd "M-[ 1;6x") (kbd "C-*")) ;;my: `highlight-symbol-next'
  (define-key input-decode-map (kbd "M-[ 1;6y") (kbd "C-("))
  (define-key input-decode-map (kbd "M-[ 1;6p") (kbd "C-)"))

  (define-key input-decode-map (kbd "M-[ 1;5m") (kbd "C--")) ;; `negative-argument'
  ;;C-_: recognizable on term, bound to `undo'
  (define-key input-decode-map (kbd "M-[ 1;6k") (kbd "C-+")) ;;my: `hs-toggle-hiding'
  (define-key input-decode-map (kbd "M-[ 1;5k") (kbd "C-=")) ;;my: `align-regexp'
  
  (define-key input-decode-map (kbd "M-[ 1;5l") (kbd "C-,"))
  (define-key input-decode-map (kbd "M-[ 1;5n") (kbd "C-."))
  (define-key input-decode-map (kbd "M-[ 1;6l") (kbd "C-<"))
  (define-key input-decode-map (kbd "M-[ 1;6n") (kbd "C->"))

  (define-key input-decode-map (kbd "M-[ 1;5i") (kbd "<C-tab>"))
  (define-key input-decode-map (kbd "M-[ 1;6i") (kbd "<C-S-tab>"))    
  
  ;; The following are not recognizable on xterm:
  ;;   C-[, C-], C-{, C-}, C-\, C-|, C-/, C-?
  ;; The following are ambigious
  ;;   C-RET, C-backspace

  ;; available on mintty (FIXME: what about other term?)
  (define-key key-translation-map  (kbd "<mouse-8>")   (kbd "<S-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-9>")   (kbd "<S-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-12>")  (kbd "<M-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-13>")  (kbd "<M-wheel-down>"))
  (define-key key-translation-map  (kbd "<mouse-20>")  (kbd "<C-wheel-up>"))
  (define-key key-translation-map  (kbd "<mouse-21>")  (kbd "<C-wheel-down>"))    
  )

(defun map-iterm-keys ()
  ;; http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html

  (define-key input-decode-map "\e[1;5A" [C-up])   
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5D" [C-left])

  (define-key input-decode-map "\e[1;4A" [M-up])   
  (define-key input-decode-map "\e[1;4B" [M-down])
  (define-key input-decode-map "\e[1;4C" [M-right])
  (define-key input-decode-map "\e[1;4D" [M-left])
  
  ;; iTerm meta-shift-<arrows> fix
  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])
  )


(defun map-putty-sco-keys ()
  "Using Emacs over PuTTY: how to use all function keys

Be sure to set tke keyboard type (Terminal | Keyboard) to SCO.
This is the only type that recognizes all function keys.
See http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-keyboard for more info.

stolen from http://emacswiki.org/emacs/PuTTY#toc9 "
  (if (eq system-uses-terminfo t)
      (progn                              ;; PuTTY hack - needs to be in SCO mode
        ;;      (define-key key-translation-map [\e] [\M])
        (define-key input-decode-map "\e[H" [home])
        (define-key input-decode-map "\e[F" [end])
        (define-key input-decode-map "\e[I" [prior])
        (define-key input-decode-map "\e[G" [next])
        
        (define-key input-decode-map "\e[D" [S-left])
        (define-key input-decode-map "\e[C" [S-right])
        (define-key input-decode-map "\e[A" [S-up])
        (define-key input-decode-map "\e[B" [S-down])
        
        ;;      (define-key input-decode-map "\e[M" [f1]) ;; conflict with `xterm-mouse-mode'
        (define-key input-decode-map "\e[Y" [S-f1])
        (define-key input-decode-map "\e[k" [C-f1])
        (define-key input-decode-map "\e\e[M" [M-f1])
        (define-key input-decode-map "\e[N" [f2])
        (define-key input-decode-map "\e[Z" [S-f2])
        (define-key input-decode-map "\e[l" [C-f2])
        (define-key input-decode-map "\e\e[N" [M-f2])
        (define-key input-decode-map "\e[O" [f3])
        (define-key input-decode-map "\e[a" [S-f3])
        (define-key input-decode-map "\e[m" [C-f3])
        (define-key input-decode-map "\e\e[O" [M-f3])
        (define-key input-decode-map "\e[P" [f4])
        (define-key input-decode-map "\e[b" [S-f4])
        (define-key input-decode-map "\e[n" [C-f4])
        (define-key input-decode-map "\e\e[P" [M-f4])
        (define-key input-decode-map "\e[Q" [f5])
        (define-key input-decode-map "\e[c" [S-f5])
        (define-key input-decode-map "\e[o" [C-f5])
        (define-key input-decode-map "\e\e[Q" [M-f5])
        (define-key input-decode-map "\e[R" [f6])
        (define-key input-decode-map "\e[d" [S-f6])
        (define-key input-decode-map "\e[p" [C-f6])
        (define-key input-decode-map "\e\e[R" [M-f6])
        (define-key input-decode-map "\e[S" [f7])
        (define-key input-decode-map "\e[e" [S-f7])
        (define-key input-decode-map "\e[q" [C-f7])
        (define-key input-decode-map "\e\e[S" [M-f7])
        (define-key input-decode-map "\e[T" [f8])
        (define-key input-decode-map "\e[f" [S-f8])
        (define-key input-decode-map "\e[r" [C-f8])
        (define-key input-decode-map "\e\e[T" [M-f8])
        (define-key input-decode-map "\e[U" [f9])
        (define-key input-decode-map "\e[g" [S-f9])
        (define-key input-decode-map "\e[s" [C-f9])
        (define-key input-decode-map "\e\e[U" [M-f9])
        (define-key input-decode-map "\e[V" [f10])
        (define-key input-decode-map "\e[h" [S-f10])
        (define-key input-decode-map "\e[_" [C-f10])
        (define-key input-decode-map "\e\e[V" [M-f10])
        (define-key input-decode-map "\e[W" [f11])
        (define-key input-decode-map "\e[i" [S-f11])
        (define-key input-decode-map "\e[u" [C-f11])
        (define-key input-decode-map "\e\e[W" [M-f11])
        (define-key input-decode-map "\e[X" [f12])
        (define-key input-decode-map "\e[j" [S-f12])
        (define-key input-decode-map "\e[v" [C-f12])
        (define-key input-decode-map "\e\e[X" [M-f12])))
  )


