;;* python-related configuration

;;** pymacs: interface between emacs lisp and python
;; add site-lisp/proglangs/python/python-libs to PYTHONPATH
(let ((path (locate-library "pymacs")))
  (if path
      (concat
       (setenv "PYTHONPATH"
               (concat (file-name-directory path)
                       "python-libs/"
                       path-separator))
       (getenv "PYTHONPATH"))))

(autoload 'pymacs-load  "pymacs"
  "Import the Python module named MODULE into Emacs." t)
(autoload 'pymacs-eval  "pymacs"
  "Compile TEXT as a Python expression, and return its value." t)
(autoload 'pymacs-autoload  "pymacs"
  "Pymacs's equivalent of the standard emacs facility `autoload'." t)

(idle-require 'pymacs)  ;;load `pymacs', for `pymacs-autoload'

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
;;*** auto-complete front-end for GNU Emacs built-in completion
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
                  (add-to-list 'ac-sources 'ac-source-python-builtin 'append)))
    ))

;;*** pycompletemine from PDEE (https://github.com/pdee/pdee/ )
;; You need `pycompletemine.{el,py}' from PDEE and pymacs
;;advantages:
;;   + differ from `pycomplete', this one would work on both python-mode.el
;;     and GNU Emacs built-in python.el
;;   + doc info and signature for completions
;;disadvantages:
;;   - `pymacs' needed
;;   - no 'send region' support, thus no completion for dynamic object

(eval-after-load "pymacs"
  `(require 'pycompletemine nil t)
  )

(eval-after-load "pycompletemine"
  `(progn
     ;;use auto-complete as frond-end
     (defun py-complete-get-help-short (prefix)
       (let ((result (py-complete-help prefix)))
         (if (> (length result) 300)
             (substring result 0 300)
           result)))
     
     (when (and (featurep 'auto-complete)
                ;;need pycompletemine.el/pycomplete.py hacked by myself
                (fboundp 'pycomplete-get-all-completions-for-ac))
       
       (defun py-complete-help-short (str1)
         (let ((result (if (fboundp 'py-complete-get-help)
                           (py-complete-get-help str1)
                         (py-complete-help str1))))
           (if (> (length result) 300)
               (substring result 0 300)
             result)))
       
       (ac-define-source pycompletemine
         '((depends pycompletemine)  ;;FIXME: ok?
           (prefix .  "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=")
           (candidates . (pycomplete-get-all-completions-for-ac ac-prefix))
           (symbol . "pyc")
           (document . py-complete-get-help-short)))
       
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (add-to-list 'ac-sources 'ac-source-pycompletemine)))
       )))

;;*** gpycomplete
;;based on pycomplete but works on built-in python-mode
(autoload 'gpycomplete-mode "gpycomplete"
  "Enables gpycomplete autocompletion minor-mode" t)
  
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

(progn
  (defvar anything-c-source-info-python
    '((name . "Info index: python")
      (info-index . "python")))

  (defun anything-info-python ()
    (interactive)
    (anything
     :input (thing-at-point 'symbol)
     :prompt "Info about: "
     :candidate-number-limit 20
     :sources
     '( anything-c-source-info-python
        ;;         anything-c-source-info-elib
        ;;anything-c-source-info-cl
        )))

  (define-key python-mode-map (kbd "M-s <f1>") 'anything-info-python)
  )

;;*** pydoc command line
;;stolen from http://stackoverflow.com/a/1068731
(defun pydoc (&optional arg)
  (interactive (list
				(read-string "Call pydoc with arg: "
							 (with-syntax-table python-dotty-syntax-table
							   (current-word)))))
  (setq cmd (concat "pydoc " arg))
  (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
  (shell-command cmd "*Pydoc Output*")
  (setq pydoc-buf (get-buffer "*Pydoc Output*"))
  ;;(switch-to-buffer-other-window pydoc-buf)
  (with-current-buffer pydoc-buf
    ;;(python-mode)
    (require 'woman)
    (woman-man-buffer)
    (help-mode))
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
       (setq pylookup-program (concat dir "pylookup"))
       (setq pylookup-db-file (concat dir "pylookup.db"))
       ;;(setq pylookup-search-options '("--insensitive" "0" "--desc" "0")
       )

     ;;redefine `pylookup-mode-quit-window', to fix issue when pylookup-return-window-config is nil
     (defun pylookup-mode-quit-window ()
       "Leave the completions window."
       (interactive)
       (if pylookup-return-window-config
           (set-window-configuration pylookup-return-window-config)))
     ))


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
;;TIPS: or you can M-x `python-check' (provided by python.el)

;;*** pep8
(autoload 'python-pep8  "python-pep8"
  "Run PEP8, and collect output in a buffer." t)
(autoload 'pep8 "python-pep8"
  "Run PEP8, and collect output in a buffer." t)

(defun pep8-simple ()
  (interactive)
  (let ((compile-command (concat "pep8 "
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively 'compile)  
  ))

;;*** pylint
(autoload 'python-pylint "python-pylint"
  "Run PYLINT, and collect output in a buffer." t)
(autoload 'pylint  "python-pylint"
  "Run PYLINT, and collect output in a buffer." t)

;;my simple way
(defun pylint-simple ()
  (interactive)
  (let ((compile-command (concat "epylint "  ;; "pylint -rn -f parseable "
                                 (file-name-nondirectory (buffer-file-name))))
        (compilation-ask-about-save nil))
    (call-interactively 'compile)  
  ))

;;*** pyflakes
;;*** pychecker

;;** ropemacs
(setq ropemacs-confirm-saving nil
      ropemacs-guess-project t
      ropemacs-separate-doc-buffer t)
(setq ropemacs-enable-autoimport t)
(setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
                                    "django.*"))

;;if you got this error: Key sequence C-x p n d starts with non-prefix key C-x p
;;you need this:
(setq ropemacs-global-prefix "C-c C-p")

(eval-after-load "pymacs"
  `(progn
     (if (fboundp 'pymacs-autoload) ;;pymacs>=0.25 needed
         (progn
           (pymacs-autoload 'rope-open-project "ropemacs" "rope-"
                            "Open a rope project." t)
           (pymacs-autoload 'ropemacs-mode "ropemacs" "rope-"
                            "Toggle ropemacs-mode." t))
       (progn
         (if nil
             (pymacs-load "ropemacs" "rope-"))))
     ))

;;** misc
;; By default, Emacs inhibits (for `run-python') the loading of Python
;; modules from the current working directory, for security reasons.
;; To disable it:
;;(setq python-remove-cwd-from-path nil)
;;*** highlight-indentation
(autoload 'highlight-indentation "highlight-indentation" nil t)
(eval-after-load "python"
  `(add-hook 'python-mode-hook 'highlight-indentation))
