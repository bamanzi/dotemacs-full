;;** recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 20)
(setq recentf-menu-path '("File"))
(setq recentf-save-file "~/.emacs.d/recentf")
(recentf-mode t)

;;** Open File
;;(define-key search-key (kbd "C-f") 'ffap-other-window)


;;** archive
;;....


;;** tramp
;;...

;;** Save File
;;** backup rules
;;(setq make-backup-files t) ;;to disable backup, set it to nil

;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

;; (setq version-control t
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2)

;;*** TODO: backup-each-save.el


;;** dired
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
