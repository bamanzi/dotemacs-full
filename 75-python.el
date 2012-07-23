
;; add site-lisp/proglangs/python/python-libs to PYTHONPATH
(let ((path (locate-library "pymacs")))
  (if path
      (concat
       (setenv "PYTHONPATH"
               (concat (file-name-directory path)
                       "python-libs/"
                       path-separator))
       (getenv "PYTHONPATH"))))

;;** code folding
(eval-after-load "python"
  `(progn
     (require 'hideshow)
     (let ((python-hideshow-exp
            '("^\\s-*\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|try\\|except\\)\\>"
              nil
              "#" 
              (lambda (arg)
                (python-end-of-block)
                (skip-chars-backward " \t\n"))
              nil))
           (old-config (assoc 'python-mode hs-special-modes-alist)))
       (if old-config
           (setcdr old-config python-hideshow-exp)
         (add-to-list 'hs-special-modes-alist `(python-mode ,python-hideshow-exp))))
     ))

(defun python-mode-init-folding ()
  ;;hideshow
  (if (and (require 'hideshowvis nil t)
           (require 'hideshow-fringe nil t))
      (hideshowvis-enable)
    (hs-minor-mode t))

  ;;outline
  (setq outline-regexp "[[:space:]]*\\(?:\\(?:class\\|def\\)\\)\\_>")
  ;;   (if (fboundp 'qtmstr-outline-mode)
  ;;       (qtmstr-outline-mode t)
  ;;     (if (fboundp 'qtmstr-outline-mode-hook)
  ;;         (qtmstr-outline-mode-hook)))
  )

(eval-after-load "python"
  `(add-hook 'python-mode-hook 'python-mode-init-folding))


;;** code completion
(defun python-symbol-completions-maybe (prefix)
  (let ((python-el (symbol-file major-mode)))
    (if (string-match "lisp/progmodes/python.el" python-el) ;;Emacs builtin python.el
        (python-symbol-completions prefix)
      nil) ;;otherwise, return nil
    ))

(eval-after-load "auto-complete"
  `(progn
    (ac-define-source python-builtin
      '( (candidates . (python-symbol-completions-maybe ac-prefix))
         (symbol . "py")
         (prefix . "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=") ))

    (add-hook 'python-mode-hook
              #'(lambda ()
                  (add-to-list 'ac-sources 'ac-source-python-builtin)))
    ))
  

;;** document lookup
;;*** python.info
(eval-after-load "pydoc-info"
  `(progn
     (add-to-list 'Info-default-directory-list
                  (file-name-directory (symbol-file 'pydoc-info)))
     (add-to-list 'Info-directory-list
                  (file-name-directory (symbol-file 'pydoc-info)))
     ;;then use C-h S (`info-lookup-symbol') to lookup python doc
     ))

(eval-after-load "python"
  `(progn
     (require 'pydoc-info nil t)
     ))

;;*** pydoc command line
;;stolen from http://stackoverflow.com/a/1068731
(defun pydoc (&optional arg)
  (interactive (list
				(read-string "Call pydoc with arg: "
							 (with-syntax-table python-dotty-syntax-table
							   (current-word)))))
  (setq cmd (concat "pydoc " arg))
  (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
  (shell-command cmd)
  (setq pydoc-buf (get-buffer "*Shell Command Output*"))
  ;;(switch-to-buffer-other-window pydoc-buf)
  (with-current-buffer pydoc-buf
    (python-mode))
  (ad-deactivate-regexp "auto-compile-yes-or-no-p-always-yes")
)

;;*** pylookup
;; $ apt-get install python-doc
;; $ ./pylookup.py -u /usr/share/doc/python2.7/html/
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(eval-after-load "pylookup"
  `(progn
     (let ((dir (file-name-directory (symbol-file 'pylookup-lookup))))
       (setq pylookup-program (concat dir "pylookup.py"))
       (setq pylookup-db-file (concat dir "pylookup.db"))
       ;;(setq pylookup-search-options '("--insensitive" "0" "--desc" "0")
       )))




;;** run
;;*** highlight error line in compilation result / shell-mode
;; stolen from http://www.loveshack.ukfsn.org/emacs/python.el
(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defun python-compilation-minor-mode (arg)
  (interactive "P")
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-minor-mode t))


;;** python shell
;;*** ipython
;;ipython.el needs python-mode.el, thus we use a lightweight solution
(defun ipython ()
  "Run ipython in `ansi-term'."
  (interactive)
  (ansi-term "ipython" "ipython"))

(defun bpython ()
  "Run bpython in `ansi-term'."
  (interactive)
  (ansi-term "bpython" "bpython"))


;;** lint
;;;_ , pep8
;;;_ , pylint
;;;_ , pyflakes
;;;_ , pychecke
(defun pylint ()
  (interactive)
  (let ((compile-command (concat "epylint "  ;; "pylint -rn -f parseable "
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively 'compile)  
  ))
;;TIPS: or you can use `python-check' with `pychecker'

;;** misc
;;*** highlight-indentation
(autoload 'highlight-indentation "highlight-indentation" nil t)
(eval-after-load "python"
  `(add-hook 'python-mode-hook 'highlight-indentation))
;;*** ropemacs
(setq ropemacs-global-prefix "C-c C-p")






