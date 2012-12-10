
;;use pascal-mode instead of delphi-mode
;;(because you can't use either `highlight-symbol', `highlight-indentation' etc,
;; nor `font-lock-add-keywords' with `delphi-mode')

(add-to-list 'auto-mode-alist '("\\.dpr$" . pascal-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . pascal-mode))

(eval-after-load "pascal"
  '(progn
;;;_. imenu improvement / jump between declaration/impletion
     (load "delphi-imenu")

;;;_. highlight more keywords/types
     (load "delphi-misc")

     (load "pascal-mode+")

;;;_. compiler
(defun pascal-compile ()
  "guest compile command for pascal/delphi"
  (interactive)
  (let* ( (filename (or (buffer-file-name) (buffer-name)))
          (ext (downcase (substring filename (- (length filename) 3)))) 
          (prjfile (if (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "program\> \w+\W;" nil t))
                       filename
                     (car (directory-files default-directory t "\\.[ld]pr$"))))
          (compiler-type (cond
                          ( prjfile
                            (if (string-match-p "lpr$" prjfile)
                                'fpc
                              'delphi) )
                          ( (member ext '("pp"))
                            'fpc )
                          ( (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "{$mode " nil t))
                            'fpc)
                          ( (eq system-type 'windows-nt)
                            'delphi)
                          (t
                           fpc))))
    (setq compile-command (if (eq compiler-type 'delphi)
                               (format "dcc32 -Q %s" (or prjfile filename))
                             (format "fpc %s" (or prjfile filename))))
    (call-interactively 'compile)))


;;;_ . how to parse compiler's output
  ;; e.g. D:\Workspace\iso\iso_io.pas(47) Warning: Comparison always evaluates to True
(add-to-list 'compilation-error-regexp-alist-alist
             '(dcc32 "^\\([a-zA-Z0-9._:\\]+\\)(\\([0-9]+\\)) \\(Error\\|Warning\\|Fatal\\|Hint\\): \\(.*\\)$" 1 2 3))
;; fpc
(add-to-list 'compilation-error-regexp-alist-alist
             '(fpc "^\\([a-zA-Z0-9.]+\\)(\\([0-9]+\\),\\([0-9]+\\)) \\(Error\\|Warning\\|Fatal\\): \\(.*\\)$" 1 2 3))

(defun pascal-mode-init-compiler ()
  (add-to-list 'compilation-error-regexp-alist 'fpc)
  (add-to-list 'compilation-error-regexp-alist 'dcc32)

  (define-key pascal-mode-map (kbd "<C-f9>") 'pascal-compile)
  (define-key delphi-mode-map (kbd "<C-f9>") 'pascal-compile)  
  )

(add-hook 'delphi-mode-hook 'pascal-mode-init-compiler)
(add-hook 'pascal-mode-hook 'pascal-mode-init-compiler)

  

))
