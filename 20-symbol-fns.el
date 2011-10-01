;; some commands for the symbol/word at point

(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" nil t)


;;;_. internal functions
(defun bmz/get-symbol-selected-or-current ()
  "Get the selected text or (if nothing selected) current symbol."
  (if (and transient-mark-mode mark-active)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol)))

;;;_. search selection
(defun bmz/search-selection-forward ()
  (interactive)
  (let ( (symbol (bmz/get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-forward)
  ))

(defun bmz/search-selection-backward ()
  (interactive)
  (let ( (symbol (bmz/get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-backward)
  ))


;;;_. symbol definition
(defun bmz/imenu-at-point ()
  (interactive)
  (imenu (bmz/get-symbol-selected-or-current)))

(defun bmz/anything-imenu-at-point ()
  (interactive)
  (anything
   :input (bmz/get-symbol-selected-or-current)
   :sources '(anything-c-source-browse-code
              anything-c-source-imenu)))

(defun bmz/find-symbol-definition-across-files ()
  (interactive)
    (cond
     ( (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
       (call-interactively 'find-function-at-point) )
     ( (and semantic-mode
	     (memq major-mode '(c-mode java-mode python-mode)))
       (call-interactively 'semantic-complete-jump) )
     (t
      (call-interactively 'find-tag))))

;;;_. occur
(defun bmz/occur-at-point ()
  (interactive)
  (occur (format "%s" (bmz/get-symbol-selected-or-current))))

(defun bmz/multi-occur-at-point ()
  (interactive)
  ;;FIXME: use multi-occur?
  (multi-occur (format "%s" (bmz/get-symbol-selected-or-current))))


;;;_.. multi-occur in same mode buffers
;; stolen from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun bmz/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (format "%s" (bmz/get-symbol-selected-or-current))))
  

;;;_. grep
(defun bmz/grep-symbol-at-point-same-ext()
  (interactive)
  (grep (format "grep -nH %s *.%s %s"
		(bmz/get-symbol-selected-or-current)
        (file-name-extension buffer-file-name)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

(defun bmz/grep-symbol-at-point()
  (interactive)
  (grep (format "grep -nH %s *.* %s"
		(bmz/get-symbol-selected-or-current)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

;;;_. "network lookup"
(autoload 'dict "dict" "Lookup a WORD on dict.org." t)
(defun dict-org-at-point  ()
  (interactive)
  (let ( (word (bmz/get-symbol-selected-or-current)) )
    (dict word)))

;;stolen from Xah Lee's http://xahlee.org/emacs/xah_emacs_generic.el
(defun lookup-google ()
  "Look up current word in Google Search.
If a there is a text selection (a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
  (interactive)
  (let ( inputstr myurl)
    (setq inputstr (bmz/get-symbol-selected-or-current))
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

(defun lookup-wikipedia ()
  "Look up current word in Wikipedia.
If there is a text selection (e.g. a phrase), lookup that phrase.
Launches default browser and opens the doc's url."
 (interactive)
 (let (inputstr myurl)
    (setq inputstr (bmz/get-symbol-selected-or-current))
    (setq inputstr (replace-regexp-in-string " " "_" inputstr))
    (setq myurl (concat "http://en.wikipedia.org/wiki/" inputstr))
    ;; (browse-url-default-windows-browser myurl)
    (browse-url myurl)
   ))


;;;_. overall
(defun init-word-ops-keys (search-map)

    (define-key search-map "i" 'bmz/anything-imenu-at-point)
    (define-key search-map "I" 'bmz/imenu-at-point)

    ;;(define-key search-map "g"          'bmz/goto-symbol-definition-in-buffer)
    (define-key search-map (kbd "M-.")    'bmz/find-symbol-definition-across-files) ;; Emacs style key
    (define-key search-map (kbd "C-]")    'bmz/find-symbol-definition-across-files) ;; Vi style key
    ;;(define-key search-map "G"          'bmz/find-symbol-definition-across-files)

    (define-key search-map (kbd "<f3>")   'bmz/search-selection-forward)
    (define-key search-map (kbd "<f4>")   'bmz/search-selection-backward
    
    (define-key search-map (kbd "j")      'highlight-symbol-at-point)
    (define-key search-map (kbd "*")      'highlight-symbol-next)
    (define-key search-map (kbd "#")      'highlight-symbol-prev)
    (define-key search-map (kbd "<up>")   'highlight-symbol-prev)
    (define-key search-map (kbd "<down>") 'highlight-symbol-next)
    (define-key search-map (kbd "M-%")    'highlight-symbol-query-replace)
    
    (define-key search-map (kbd "O")     'bmz/occur-at-point)

    (define-key search-map (kbd "M-o")   'bmz/multi-occur-in-this-mode)
    (define-key search-map (kbd "M-O")   'bmz/multi-occur-at-point)

    (define-key search-map (kbd "g")     'nil)
    (define-key search-map (kbd "gg")    'bmz/grep-symbol-at-point-same-ext)
    (define-key search-map (kbd "gG")    'bmz/grep-symbol-at-point)
    (define-key search-map (kbd "g SPC") 'grep)
    (define-key search-map (kbd "gr")    'rgrep)
    (define-key search-map (kbd "gl")    'lgrep)

    ;;TODO: ack

    ;; (define-key search-map (kbd "f") 'find-function-at-point)
    ;; (define-key search-map (kbd "v") 'find-variable-at-point)
    ;; (define-key search-map (kbd "l") 'find-library)
    ;; (define-key search-map (kbd "C-f") 'ffap-other-window)

    (define-key search-map (kbd "<f3>") 'isearch-repeat-forward)
    (define-key search-map (kbd "<S-f3>") 'isearch-repeat-backward)

    (autoload 'sdcv-search "sdcv-mode" nil t)
    (define-key search-map (kbd "d")   'sdcv-search) ;;sdcv-mode.el needed

    (define-key search-map (kbd "D")   'dict-org-at-point)
    (define-key search-map (kbd "G")   'lookup-google)
    (define-key search-map (kbd "W")   'lookup-wikipedia)

    (define-key search-map (kbd "C-f") 'ffap-other-window)
    (define-key search-map (kbd "RET") 'browse-url-at-point)
    
    t
    )

(init-word-ops-keys search-map)

(define-key global-map (kbd "<f3>") search-map)


;; other keys
(define-key global-map (kbd "<C-f3>") 'isearch-repeat-forward)
(define-key global-map (kbd "<S-f3>") 'isearch-repeat-backward)



