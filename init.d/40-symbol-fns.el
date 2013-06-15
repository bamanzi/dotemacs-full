;;* some commands for the symbol/word at point




;;** some internal functions
(defun get-symbol-selected-or-current ()
  "Get the selected text or (if nothing selected) current symbol."
  (if (and transient-mark-mode mark-active)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (regexp-quote (thing-at-point 'symbol))))

;;** search selection
(defun search-selection-forward ()
  (interactive)
  (let ( (symbol (get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-forward)
  ))

(defun search-selection-backward ()
  (interactive)
  (let ( (symbol (get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-backward)
  ))

;;** highlight-symbol
(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" nil t)
(autoload 'highlight-symbol-query-replace "highlight-symbol" nil t)



;;** bookmark all lines containing current symbol
;; based on code of `bm-bookmark-regexp'
(defun bm-bookmark-symbol-at-point ()
  "Set bookmark on lines that containing current symbol."
  (interactive)
  (let ( (beg (point-min))
         (end (point-max))
         (regexp (get-symbol-selected-or-current))
         (annotation nil)
         (count 0) )
    (unless (featurep 'bm) (require 'bm))
    (save-excursion
      (if bm-annotate-on-create
          (setq annotation (read-from-minibuffer
                            "Annotation: " nil nil nil 'bm-annotation-history)))
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (bm-bookmark-add annotation)
        (setq count (1+ count))
        (forward-line 1)))
    (message "%d bookmark(s) created." count)))

(defun copy-symbol-at-point ()
  (interactive)
  (if (get 'symbol 'thing-at-point)
      (funcall (get 'symbol 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ( (begin (car bounds))
               (end   (cdr bounds)) )
          (kill-ring-save begin end)
          (if (fboundp 'pulse-momentary-highlight-region)
              (pulse-momentary-highlight-region begin end)))
          ))))

;;** symbol definition
(defun imenu-at-point ()
  (interactive)
  (imenu (get-symbol-selected-or-current)))

(defun anything-imenu-at-point ()
  (interactive)
  (anything
   :input (get-symbol-selected-or-current)
   :sources '(anything-c-source-browse-code
              anything-c-source-imenu)))

(defun find-symbol-definition-across-files ()
  (interactive)
    (cond
     ( (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
       (call-interactively 'find-function-at-point) )
     ( (and semantic-mode
	     (memq major-mode '(c-mode java-mode python-mode)))
       (call-interactively 'semantic-complete-jump) )
     (t
      (call-interactively 'find-tag))))

;;** occur
(defun occur-at-point (nlines)
  (interactive "P")
  (occur (format "%s" (get-symbol-selected-or-current)) nlines))

(defun multi-occur-at-point (nlines)
  (interactive "P")
  ;;FIXME: use multi-occur?
  (multi-occur nil (format "%s" (get-symbol-selected-or-current)) nlines))


;;*** multi-occur in same mode buffers
;; stolen from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (format "%s" (get-symbol-selected-or-current))))
  

;;** grep
(defun grep-symbol-at-point-same-ext()
  (interactive)
  (grep (format "grep -nH %s *.%s %s"
		(get-symbol-selected-or-current)
        (file-name-extension buffer-file-name)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

(defun grep-symbol-at-point()
  (interactive)
  (grep (format "grep -nH %s *.* %s"
		(get-symbol-selected-or-current)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

;;*** ack: a better grep  http://betterthangrep.com/
;;;  - it would ignore .svn, CVS etc by default
;;;  - it would ignore binary files, core dumps, backup files by default
;;;  - to limit search in some file types, you can easily use `--type=perl'
;;;    rather than combining grep with `find'
;;;
(autoload 'ack "ack" "Use ack where you might usually use grep." t)
(autoload 'ack-mode "ack" "Use ack where you might usually use grep." nil)
(defun ack-at-point (typep recursively)
  (require 'ack)
  (let ( (command (concat (if typep (ack-build-command)
                            ack-command)
                          (if recursively " -r "
                              " ")
                          (get-symbol-selected-or-current))) )
    (compilation-start command 'ack-mode)))

(defun ack-at-point-in-same-type-files (arg)  
  "Use `ack' to search current symbol in same type files. if ARG given, search recursively."
  (interactive "P")
  (ack-at-point 'same-type arg))

(defun ack-at-point-in-all-files (arg)
    "Use `ack' to search current symbol in all files. if ARG given, search recursively."
  (interactive "P")
  (ack-at-point nil arg))

;;** local dictionary
(autoload 'sdcv-search "sdcv-mode" nil t)

;;** dict lookup
;;*** DICT protocol with dictionary.el
;;    (setq dictionary-server "localhost")
(autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary" "Ask for a word and search all matching words in the dictionaries" t)

;;*** DICT protocol with dictem.el (external program `dict' needed)
;;(require 'dictem)
;;(setq dictem-server "localhost")
;;(require 'dictem)
;;(dictem-initialize)
(autoload 'dictem-run-search "dictem" nil t)
(autoload 'dictem-run-match  "dictem" nil t) 

;;*** DICT protocol with dict.el (external program `dict' needed)
;;;  (NOTE: it's hard to use it on windows)
;;(setq dict-servers '("localhost" "dict.org"))
;;(setq dict-enable-key-bindings t)
;;(setq dict-databases '("gcide" "pydict"))

(autoload 'dict "dict" "Lookup a WORD on dict.org." t)
(defun dict-org-at-point  ()
  (interactive)
  (let ( (word (get-symbol-selected-or-current)) )
    (dict word)))

;;** lookup on google
;;stolen from Xah Lee's http://xahlee.org/emacs/xah_emacs_generic.el
(defun lookup-google ()
  "Look up current word in Google Search.
If a there is a text selection (a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
  (interactive)
  (let ( inputstr myurl)
    (setq inputstr (get-symbol-selected-or-current))
;;    (setq inputstr (dehtmlize-in-string inputstr) )
    (setq myurl (concat "http://www.google.com/search?q=%22" inputstr "%22"))

    (cond
     ((string-equal system-type "windows-nt") ; any flavor of Windows
      (browse-url-default-windows-browser myurl)
      )
     ((string-equal system-type "gnu/linux")
      (browse-url myurl)
      )
     ((string-equal system-type "darwin") ; Mac
      (browse-url myurl)
      )
     )))

;;** lookup on wikipedia
(defun lookup-wikipedia ()
  "Look up current word in Wikipedia.
If there is a text selection (e.g. a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
 (interactive)
 (let (inputstr myurl)
    (setq inputstr (get-symbol-selected-or-current))
    (setq inputstr (replace-regexp-in-string " " "_" inputstr))
    (setq myurl (concat "http://en.wikipedia.org/wiki/" inputstr))
    ;; (browse-url-default-windows-browser myurl)
    (browse-url myurl)
   ))


;;** overall
(defun init-word-ops-keys (search-map)

    (define-key search-map "i" 'anything-imenu-at-point)
    (define-key search-map "I" 'imenu-at-point)

    ;;(define-key search-map "g"          'goto-symbol-definition-in-buffer)
    (define-key search-map (kbd "M-.")    'find-symbol-definition-across-files) ;; Emacs style key
    (define-key search-map (kbd "C-]")    'find-symbol-definition-across-files) ;; Vi style key
    ;;(define-key search-map "G"          'find-symbol-definition-across-files)

    (define-key search-map (kbd "<f3>")   'isearch-repeat-forward)
    (define-key search-map (kbd "<S-f3>") 'isearch-repeat-backward)
    
    (define-key search-map (kbd "j")      'highlight-symbol-at-point)
    (define-key search-map (kbd "*")      'highlight-symbol-next)
    (define-key search-map (kbd "#")      'highlight-symbol-prev)
    (define-key search-map (kbd "<up>")   'highlight-symbol-prev)
    (define-key search-map (kbd "<down>") 'highlight-symbol-next)
    (define-key search-map (kbd "M-%")    'highlight-symbol-query-replace)
    
    (define-key search-map (kbd "O")     'occur-at-point)

    (define-key search-map (kbd "M-o")   'multi-occur-in-this-mode)
    (define-key search-map (kbd "M-O")   'multi-occur-at-point)

    (define-key search-map (kbd "g")     'nil)
    (define-key search-map (kbd "G")     'grep-symbol-at-point-same-ext)
    (define-key search-map (kbd "gg")    'grep-symbol-at-point-same-ext)
    (define-key search-map (kbd "gG")    'grep-symbol-at-point)
    (define-key search-map (kbd "g SPC") 'grep)
    (define-key search-map (kbd "gr")    'rgrep)
    (define-key search-map (kbd "gl")    'lgrep)

    (define-key search-map (kbd "aa")    'ack-at-point-in-same-type-files)
    (define-key search-map (kbd "aA")    'ack-at-point-in-all-files)
    (define-key search-map (kbd "a SPC") 'ack)
    (define-key search-map (kbd "A")    'ack-at-point-in-same-type-files)
    
    ;; (define-key search-map (kbd "f") 'find-function-at-point)
    ;; (define-key search-map (kbd "v") 'find-variable-at-point)
    ;; (define-key search-map (kbd "l") 'find-library)
    ;; (define-key search-map (kbd "C-f") 'ffap-other-window)


    (define-key search-map (kbd "D")   'sdcv-search) ;;sdcv-mode.el needed

    (define-key search-map (kbd "d")    'nil)
    (define-key search-map (kbd "do")   'dict-org-at-point)       ;;dict.el needed
    (define-key search-map (kbd "ds")   'dictionary-search)       ;;dictionary.el needed
    (define-key search-map (kbd "dm")   'dictionary-match-words)  ;;dictionary.el needed
    
    (define-key search-map (kbd "G")   'lookup-google)
    (define-key search-map (kbd "W")   'lookup-wikipedia)

    (define-key search-map (kbd "C-f") 'ffap-other-window)
    (define-key search-map (kbd "RET") 'browse-url-at-point)

    (define-key search-map (kbd "<f2>") 'bm-bookmark-symbol-at-point)
    (define-key search-map (kbd "M-w")  'copy-symbol-at-point)
    
    t
    )

(init-word-ops-keys search-map)

(define-key global-map (kbd "<f3>") search-map)


;; other keys
;;(define-key global-map (kbd "<C-f3>") 'isearch-repeat-forward)
;;(define-key global-map (kbd "<S-f3>") 'isearch-repeat-backward)
(define-key global-map (kbd "<C-f3>")   'search-selection-forward)
(define-key global-map (kbd "<S-f3>")   'search-selection-backward)



