
;;** find the files to open
;;*** recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 20)
(setq recentf-menu-path '("File"))
(customize-set-variable
 'recentf-save-file "~/.emacs.d/recentf")
(recentf-mode t)

(defun ido-recentf ()
  (interactive)
  (find-file (ido-completing-read "Recent File: " recentf-list)))

;;*** bookmarks
;;DOC: (info "(emacs) Bookmarks")
;;It supports: file, dired, info node
;; `C-x r m <RET>'              Set the bookmark for the visited file, at point.
;; `C-x r m BOOKMARK <RET>'     Set the bookmark named BOOKMARK at point (`bookmark-set').
;; `C-x r b BOOKMARK <RET>'     Jump to the bookmark named BOOKMARK (`bookmark-jump').
;; `C-x r l'                    List all bookmarks (`list-bookmarks').
;; `M-x bookmark-save'          Save all the current bookmark values in the default bookmark file.

(setq bookmark-default-file (expand-file-name "emacs.bmk" user-emacs-directory))

(global-set-key (kbd "<f5> B") 'anything-bookmarks)

;;*** locate
;; Emacs built-in: M-x locate


(if (eq system-type 'windows-nt)
    (setq anything-c-locate-command "locate %s")    ;;Use Locate32
  ;;(setq anything-c-locate-command "es -i -r %s")  ;;Use Everything
  ;;otherwise, use platform specific default
  )
(global-set-key (kbd "<f5> l") 'anything-locate)

;;*** others
(define-key global-map (kbd "M-s C-f")      'ffap)
(define-key global-map (kbd "ESC M-s C-f")  'ffap-other-window)

(define-key global-map (kbd "<f5> f")       'anything-find-files)
  

;;** Open methods

;;*** read-only
;;TIP: C-x C-r - find-file-read-only

;;*** different encoding
;;TIP: `revert-buffer-with-coding-system'

;;TIP: open file with specific encoding: C-x RET c utf-8 C-x C-f
(defun find-file-with-coding-system (coding-system)
  (interactive (let ((default (and buffer-file-coding-system
                                   (not (eq (coding-system-type buffer-file-coding-system)
                                            'undecided))
                                   buffer-file-coding-system)))
                 (list (read-coding-system
                        (if default
                            (format "Coding system for find-file (default %s): " default)
                          "Coding system for find-file: ")
                        default))))
  (let ((coding-system-for-read coding-system)
        (coding-system-for-write coding-system)
        (coding-system-require-warning t))
    (call-interactively 'find-file)))

;;*** auto-compress-mode
;;TIP: emacs would open .gz, .Z,
;;TIP: emacs 24 supports .7z

;;*** archive
;;TIP: emacs would open .tar, .zip/.xpi

;;*** tramp
;;...

;;**** open file with sudo
(when (memq system-type '(gnu gnu/linux darwin))

  ;; (defun revert-buffer-with-sudo ()
  ;;   (interactive)
  ;;   (let ( (filename (buffer-file-name)) )
  ;;     (if filename
  ;;         (let ( (trampfilename (concat "sudo::" filename)) )
  ;;           (find-alternate-file trampfilename))
  ;;       (message "buffer not saved yet."))))

  ;;from: http://xahlee.org/emacs/xah_emacs_generic.el
  ; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02
  (defun open-current-file-as-admin ()
        "Open the current buffer as unix root.
    This command works on unixes only."
        (interactive)
        (when buffer-file-name (find-alternate-file
                                (concat "/sudo:root@localhost:" buffer-file-name))))
 
  (defalias 'revert-buffer-with-sudo 'open-current-file-as-admin)
   
  (defun find-file-sudo (filename)
        "find-file with `sudo' method."
        (interactive
        (find-file-read-args "Find file: "
                            (confirm-nonexistent-file-or-buffer)))
        (let ((value (find-file-noselect
                    (concat "/sudo:root@localhost:" filename)
                    nil nil nil)))
        (if (listp value)
            (mapcar 'switch-to-buffer (nreverse value))
            (switch-to-buffer value))))
 
    (defun write-file-with-sudo (filename &;optional username)
    "Write current buffer into file FILENAME, with sudo method."
    (interactive
    (list (read-file-name "Write file (with sudo): "
                            nil
                            (if (buffer-file-name)
                                (if (string-match "^/sudo:" (buffer-file-name))
                                    (replace-regexp-in-string "/sudo:.*@localhost:" "" (buffer-file-name))
                                (buffer-file-name))
                                default-directory))
            (if current-prefix-arg
                (read-string "Username: "
                            "root"))))
    (let ((tramp-filename (format "/sudo:%s@localhost:%s" username filename)))
        (write-file tramp-filename t)))                        
  ))

;;*** sudo without tramp
(autoload 'sudo-find-file  "sudo"
  "Open a file, which may or may not be readable. If we can't" t)

(autoload 'sudo-save-current-buffer  "sudo"
  "Save current buffer, running sudo if necessary." t)

(defalias 'save-buffer-with-sudo 'sudo-save-current-buffer)


(autoload 'sudoedit  "sudo-ext"
  "Run `sudoedit FILE' to edit FILE as root." t)
;;`sudo-ext' also has sudo support in shell execution in Emacs

(when (memq system-type '(gnu gnu/linux darwin))
  ;;another way, hook would be installed on `find-file-hooks', `write-file-hooks'
  (idle-require 'sudo-save)
  )

;;** Save File
;;...
;;*** Emacs built-in backup rules
;;(setq make-backup-files t) ;;to disable backup, set it to nil

;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t
      backup-by-copying-when-linked nil)

;; (setq version-control t
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2)

;;*** backup-each-save.el: tree structure mirrored in backup dir

;;(setq backup-each-save-mirror-location "~/.backups")

(idle-require 'backup-each-save)

(eval-after-load "backup-each-save"
  `(progn
     (add-hook 'after-save-hook 'backup-each-save)

     (if (memq system-type '(windows-nt ms-dos cygwin))

         ;; for windows, remove ':' in backup filename 
         (defun backup-each-save-compute-location (filename)
           ;;(let* ((containing-dir (file-name-directory filename))
           (let* ((containing-dir (replace-regexp-in-string ":" "" (file-name-directory filename)))
                  (basename (file-name-nondirectory filename))
                  (backup-container
                   (format "%s/%s"
                           backup-each-save-mirror-location
                           containing-dir)))
             (when (not (file-exists-p backup-container))
               (make-directory backup-container t))
             (format "%s/%s-%s" backup-container basename
                     (format-time-string backup-each-save-time-format))))
       )
     ))


;;** filesystem navigation & management
;;*** dired
(global-set-key (kbd "M-g d") 'dired-jump) ;;C-x C-j

(defun bmz/dired-jump ()
  "If current buffer is in an archive(zip/tar), jump to it.
Otherwise, call the original `dired-jump'."
  (interactive)
  (let ( (pair (split-string buffer-file-name ":")) )
    (if (> (length pair) 2)
		(let ( (arcfile  (mapconcat 'identity
                                    (butlast pair)
                                    ":")) )
          (find-file arcfile))
      (call-interactively 'dired-jump))))

(define-key goto-map "d" 'bmz/dired-jump)

;;**** dired-single
(autoload 'joc-dired-single-buffer "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(autoload 'joc-dired-single-buffer-mouse "dired-single"
"Visits the selected directory in the current buffer, replacing the" t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map [return] 'joc-dired-single-buffer)
     (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
     (define-key dired-mode-map "^"
       (function
        (lambda nil (interactive) (joc-dired-single-buffer ".."))))
     )) 

;;*** nav: simple file system navigation
(autoload 'nav "nav" "Opens Nav in a new window to the left of the current one." t)

(autoload 'nav-toggle "nav" "Toggles the nav panel." t)

(defadvice nav (after set-nav-window-dedicated activate)
  (let ( (window (get-buffer-window "*nav*")) )
    (if window
        (set-window-dedicated-p window t))))

(defun bmz/nav-goto-dir (dir)
  (interactive "Dnav to: ")
  (unless (get-buffer "*nav*")
    (nav))
  (let ( (window (get-buffer-window "*nav*")) )
    (set-window-dedicated-p window t))
  (with-current-buffer "*nav*"
      (nav-jump-to-dir dir)))

(define-key goto-map "D" 'bmz/nav-goto-dir)

;;*** nc.el: norton commander clone
(autoload 'nc "nc" "Major mode for File Browser in GNU emacs." t)

(eval-after-load "nc"
  `(progn
     (defadvice nc (around nc-window-configuration activate)
       ;;save window configuration before nc starts
       (frame-configuration-to-register ? )
       ad-do-it
       (let ( (nc-win1 (get-buffer-window "*NC 1*"))
              (nc-win2 (get-buffer-window "*NC 2*"))
              (nc-win3 (get-buffer-window "*NC shell*")) )
         (set-window-dedicated-p nc-win1 t)
         (set-window-dedicated-p nc-win2 t)
         (set-window-dedicated-p nc-win3 t)
         (unless (get-register ?n)
           (frame-configuration-to-register ?n))))
     ))

(defun nc-goto-dir (dir)
  (interactive "Dnc to: ")
  (nc)
  (with-current-buffer nc-active-nc-buffer
    (nc-display-new-dir dir)))

;;(define-key goto-map "\M-n" 'nc-goto-dir)


;;** misc
;;*** describe file
(defun describe-this-file ()
  (interactive)
  (require 'help-fns+)
  (if buffer-file-name
      (describe-file buffer-file-name)
    (message "file not saved. ")))

(global-set-key (kbd "<f6> M-g") 'describe-this-file)

;;simulate vi's C-g (:file)
(autoload 'ex-set-visited-file-name "viper-ex" nil nil)
(defun viper-describe-file ()
  (interactive)
  (ex-set-visited-file-name))

(global-set-key (kbd "<f6> C-g") 'viper-describe-file)

;;*** make built-in lisp file read-only
(defvar emacs-lisp-directory (expand-file-name "../" (locate-library "subr")))

(defun make-builtin-lisp-file-read-only ()
  (let ((filename (buffer-file-name)))
    (if (string= emacs-lisp-directory
                 (substring filename 0 (length emacs-lisp-directory)))
        (toggle-read-only t))))

(add-hook 'find-file-hook 'make-builtin-lisp-file-read-only)

