;;;_. backup rules
;;(setq make-backup-files t) ;;to disable backup, set it to nil

;;(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

;; (setq version-control t
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2)


;;;_ S(@* "dired")
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

;;;_ S(@* "ediff")


;;;_. command line args support
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))  ;;FIXME: which one?

;;;_. window configuration
;;I don't like multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)
;; split the window depending on the frame width:
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))
