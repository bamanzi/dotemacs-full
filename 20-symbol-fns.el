;; some commands for the symbol/word at point

(require 'highlight-symbol)

(defun bmz/goto-symbol-occurrence (forward)
  (let ( (symbol (highlight-symbol-get-symbol)) )
    (unless symbol (error "No symbol at point"))  
    (unless hi-lock-mode (hi-lock-mode 1))  
    (if (not (member symbol highlight-symbol-list))  
	(highlight-symbol-at-point)))  
  (if forward
      (highlight-symbol-next)
    (highlight-symbol-prev)))

(defun bmz/goto-symbol-next-occur ()
  (interactive)
  (bmz/goto-symbol-occurrence t))

(defun bmz/goto-symbol-prev-occur ()
  (interactive)
  (bmz/goto-symbol-occurrence nil))


(defun bmz/get-symbol-selected-or-current ()
  "Get the selected text or (if nothing selected) current symbol."
  (if (and transient-mark-mode mark-active)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol)))


(defun bmz/goto-symbol-definition-in-buffer ()
  (interactive)
  (if (fboundp 'idomenu)
      (call-interactively 'idomenu)
    (call-interactively 'imenu)))

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
   (car (occur-read-primary-args))))

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


;;--- overall
(defun init-word-ops-keys (search-map)

    (define-key search-map "i" 'idomenu)
    (define-key search-map "I" 'imenu)

    ;;(define-key search-map "g" 'bmz/goto-symbol-definition-in-buffer)
    (define-key search-map (kbd "M-.") 'bmz/find-symbol-definition-across-files) ;; Emacs style key
    (define-key search-map (kbd "C-]") 'bmz/find-symbol-definition-across-files) ;; Vi style key
    ;;(define-key search-map "G" 'bmz/find-symbol-definition-across-files)
    
    (define-key search-map (kbd "SPC")    'highlight-symbol-at-point)
    (define-key search-map (kbd "*")      'bmz/goto-symbol-next-occur)
    (define-key search-map (kbd "#")      'bmz/goto-symbol-prev-occur)
    (define-key search-map (kbd "<up>"    'bmz/goto-symbol-prev-occur)
    (define-key search-map (kbd "<down>") 'bmz/goto-symbol-next-occur)
    (define-key search-map (kbd "M-%")    'highlight-symbol-query-replace)

    (define-key search-map (kbd "O")   'bmz/occur-at-point)
    (define-key search-map (kbd "M-o") 'multi-occur-in-this-mode)
    (define-key search-map (kbd "M-O") 'bmz/multi-occur-at-point)

    (define-key search-map (kbd "g")  'nil)
    (define-key search-map (kbd "gg") 'bmz/grep-symbol-at-point-same-ext)
    (define-key search-map (kbd "gG") 'bmz/grep-symbol-at-point)
    (define-key search-map (kbd "g SPC") 'grep)
    (define-key search-map (kbd "gr") 'rgrep)
    (define-key search-map (kbd "gl") 'lgrep)

    ;; (define-key search-map (kbd "f") 'find-function-at-point)
    ;; (define-key search-map (kbd "v") 'find-variable-at-point)
    ;; (define-key search-map (kbd "l") 'find-library)
    ;; (define-key search-map (kbd "C-f") 'ffap-other-window)

    (define-key search-map (kbd "<f3>") 'isearch-repeat-forward)
    (define-key search-map (kbd "<S-f3>") 'isearch-repeat-backward)

    (autoload 'sdcv-search "sdcv-mode" nil t)
    (define-key search-map (kbd "d") 'sdcv-search) ;;sdcv-mode.el needed

    (define-key search-map (kbd "D") 'dict-org-at-point)
    (define-key search-map (kbd "G") 'lookup-google)
    (define-key search-map (kbd "W") 'lookup-wikipedia)

    (define-key search-map (kbd "C-f") 'ffap-other-window)
    (define-key search-map (kbd "RET") 'browse-url-at-point)
    
    t
    )

(init-word-ops-keys search-map)

(define-key global-map (kbd "<f3>") search-map)


;; other keys
(define-key global-map (kbd "<C-f3>") 'isearch-repeat-forward)
(define-key global-map (kbd "<S-f3>") 'isearch-repeat-backward)

(define-key goto-map (kbd "i") 'bmz/goto-symbol-definition-in-buffer)
(define-key goto-map (kbd "I") 'bmz/find-symbol-definition-across-files)


