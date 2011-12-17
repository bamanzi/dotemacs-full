
;;** find the files to open
;;*** recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 20)
(setq recentf-menu-path '("File"))
(setq recentf-save-file "~/.emacs.d/recentf")
(recentf-mode t)

;;*** bookmarks
;;(define-key search-key (kbd "C-f") 'ffap-other-window)


;;** Open methods
;;TIP: C-x C-r - find-file-read-only

;;*** auto-compress-mode
;;TIP: emacs would open .gz, .Z,
;;TIP: emacs 24 supports .7z

;;*** archive
;;TIP: emacs would open .tar, .zip/.xpi

;;*** tramp
;;...
;;**** sudo
(defun revert-buffer-with-sudo ()
  (interactive)
  (let ( (filename (buffer-file-name)) )
    (if filename
        (let ( (trampfilename (concat "sudo::" filename)) )
          (find-alternate-file trampfilename))
      (message "buffer not saved yet."))))

(defun save-buffer-with-sudo ()
  (interactive)
  (message "TODO: not implemented yet."))


;;** Save File
;;...
;;*** backup rules
;;(setq make-backup-files t) ;;to disable backup, set it to nil

;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

;; (setq version-control t
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2)

;;*** TODO: backup-each-save.el

;;** filesystem navigation & management
;;*** dired
;;(global-set-key (kbd "M-g d") 'dired-jump) ;;C-x C-j

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
  (interactive
   (list (read-directory-name
          "NAV to dir"
          nil
          (if buffer-file-name
              (file-name-directory buffer-file-name)
            default-directory)) ))
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

(defun nc-goto-dir (arg)
  (interactive "P")
  (let* ( (curdir (if buffer-file-name
                     (file-name-directory buffer-file-name)
                   default-directory))
         (dir (if current-prefix-arg
                  (read-directory-name "nc to: " nil curdir)
                curdir)) )
    (nc)
    (with-current-buffer nc-active-nc-buffer
      (nc-display-new-dir dir))))

(define-key goto-map "c" 'nc-goto-dir)



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

