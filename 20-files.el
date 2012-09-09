
;;** find the files to open
;;*** recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 20)
(setq recentf-menu-path '("File"))
(setq recentf-save-file "~/.emacs.d/recentf")
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
(defun revert-buffer-with-sudo ()
  (interactive)
  (let ( (filename (buffer-file-name)) )
    (if filename
        (let ( (trampfilename (concat "sudo::" filename)) )
          (find-alternate-file trampfilename))
      (message "buffer not saved yet."))))


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

     (if (eq system-type 'windows-nt)

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

;;*** nav: simple file system navigation
(autoload 'nav "nav" "Opens Nav in a new window to the left of the current one." t)

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



;;** ediff

;;*** command line args support
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))  ;;FIXME: which one?

;;*** window configuration
;;I don't like multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)
;; split the window depending on the frame width:
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))


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
(defun find-file-on-built-in-lisp-file ()
  (let ((lisp-dir (file-name-directory (locate-library "subr"))))
    (if (string-match lisp-dir buffer-file-name)
        (toggle-read-only t))))

(add-hook 'find-file-hook 'find-file-on-built-in-lisp-file)
