
;;;_. options
(global-set-key (kbd "<C-f10> g") 'customize-group)
(global-set-key (kbd "<C-f10> v") 'customize-variable)
(global-set-key (kbd "<C-f10> f") 'customize-face)
(global-set-key (kbd "<C-f10> t") 'customize-themes)


;;;_. emacs-lisp
(global-set-key (kbd "<f12> l l") 'load-library)
(global-set-key (kbd "<f12> l t") 'load-theme)

(global-set-key (kbd "<C-f10> d") 'toggle-debug-on-error)

(defun load-and-execute (library)
  "load a library 'foobar' and execute the command with same name (foobar or foobar-mode"
  (interactive
   (list (completing-read "Load library: "
                          (apply-partially 'locate-file-completion-table
                                           load-path
                                           (get-load-suffixes)))))
  (when (load library)
    (let ( (command (if (fboundp (intern library))
                        (intern library)
                      (intern (concat library "-mode")))) )
      (message "try to execute `%s'" command)
      (call-interactively command))))


;;; message
  (defadvice message (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


   (ad-disable-advice 'message 'before 'who-said-that)
   (ad-update 'message)


;;;_. "utils"
(global-set-key (kbd "<f12> a") 'apropos)  ;;sys-apropos ?
(global-set-key (kbd "<f12> c") 'quick-calc)
(global-set-key (kbd "<f12> C") 'calc-dispatch)
(global-set-key (kbd "<f12> d") 'ediff)
(global-set-key (kbd "<f12> g") 'grep)
(global-set-key (kbd "<f12> i") 'info-apropos)

(global-set-key (kbd "<f12> m") 'woman)
(global-set-key (kbd "<f12> r") 'regexp-builder)
(global-set-key (kbd "<f12> s") 'eshell)
(global-set-key (kbd "<f12> S") 'shell)

;;;_ S(@* "eshell")


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
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;;I don't like multiframe






