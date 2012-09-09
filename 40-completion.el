;;* buffer completion
;; (for minibuffer completion, see 25-minibuffer.el)

;;** built-in mechanism
;; Emacs default:
;; M-TAB    complete-symbol     (< 23.2)
;;          completion-at-point (>=23.2)

;;*** completion at point
;; use TAB for completion
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete)
  (require 'smart-tab nil t)
  )

;;**** Use popup-menu* to provide a drop-down menu for completion-at-point command
;; http://www.emacswiki.org/PopUp#toc2
;;
;; (usually accessible via <TAB>). This is much simpler than
;; AutoComplete or CompanyMode if you only want the drop-down menu for
;; completion. --LinhDang

(autoload 'popup-menu* "popup" "Show a popup menu" nil)

(defcustom complete-in-region-use-popup t
    "If non-NIL, complete-in-region will popup a menu with the possible completions."
    :type 'boolean
    :group 'completion)

(defun popup-complete-in-region (next-func start end collection &optional predicate)
  (if (not complete-in-region-use-popup)
      (funcall next-func start end collection predicate)
    (let* ((prefix (buffer-substring start end))
           (completion (try-completion prefix collection predicate))
           (choice (and (stringp completion)
                        (string= completion prefix)
                        (popup-menu* (all-completions prefix collection predicate))))
           (replacement (or choice completion))
           (tail (and (stringp replacement)
                      (not (string= prefix replacement))
                      (substring replacement (- end start)))))
      (cond ((eq completion t)
             (goto-char end)
             (message "Sole completion")
             nil)
            ((null completion)
             (message "No match")
             nil)
            (tail
             (goto-char end)
             (insert tail)
             t)
            (choice
             (message "Nothing to do")
             nil)
            (t
             (message "completion: something failed!")
             (funcall next-func start end collection predicate))))))

(add-hook 'completion-in-region-functions 'popup-complete-in-region)



;;*** dabbrev
;;   M-/ - dabbrev-expand
(define-key global-map (kbd "M-/")  'dabbrev-expand)

;;*** hippie-expand 
(define-key global-map (kbd "ESC M-/")      'hippie-expand)

(defun hippie-expand-filename ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially
                                            try-complete-file-name)))
    (call-interactively 'hippie-expand)))

(if (boundp 'undo-tree-map)
    (define-key undo-tree-map (kbd "C-/") nil))

(define-key global-map (kbd "C-/") 'hippie-expand-filename)

;;*** words
(define-key global-map (kbd "ESC M-$") 'ispell-complete-word)

;;** auto-compelte
(autoload 'auto-complete-mode  "auto-complete"
  "AutoComplete mode" t)
(global-key-binding (kbd "<f10> ac") 'auto-complete-mode)

(idle-require 'auto-complete)
(idle-require 'auto-complete-config)
(eval-after-load "auto-complete-config"
  `(progn
     (ac-config-default)
     (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)

     ;;(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)

     (if (require 'auto-complete-scite-api nil t)
         (add-to-list 'ac-sources 'ac-source-scite-api)
       (message "%s: failed to load `auto-complete-scite-api'." load-file-name))
     )
  )

;;*** helper command: ac-toggle-source
(defun ac-toggle-source (source &optional desire)
  "Add or remove a SOURCE in `ac-sources'.

If DESIRE given, this source would be absolutely added (if DESIRE > 0) or
remove (if DESIRE <= 0). If DESIRE not given, it would be toggled."
  (interactive
   (list (intern-soft (ido-completing-read "Source: "
										   (loop for x being the symbols
												 if (and (boundp x)
														 (string-match "^ac-source-" (symbol-name x)))
												 collect (symbol-name x))))))
  (when (and source (symbolp source))
	(if desire
		(if (> desire 0)
			(add-to-list 'ac-sources source)
		  (setq ac-sources (remq source ac-sources)))
	  (if (memq source ac-sources)
		  (setq ac-sources (remq source ac-sources))
		(add-to-list 'ac-sources source)))
	(message "Source `%s' %s." source (if (memq source ac-sources)
										  "enabled"
										"disabled"))))

;;*** use `pos-tip' to fix the popup window position issue
;; `auto-complete' 1.4 already use `pos-tip'
(when (require 'popup-pos-tip nil t)
  (defadvice popup-tip
    (around popup-pos-tip-wrapper (string &rest args) activate)
    (if (memq window-system '(x w32))
        (apply 'popup-pos-tip string args)
      ad-do-it)))

;;*** complete file name
(defun ac-expand-filename ()  ;;FIXME: `ac-complete-filename'?
  (interactive)
  (let ( (ac-sources '(ac-source-filename ac-source-files-in-current-dir)) )
    (call-interactively 'ac-start)))

(eval-after-load "auto-complete-config"
  `(progn
     (define-key global-map (kbd "C-. f") 'ac-expand-filename)
     (define-key global-map (kbd "C-. /") 'ac-expand-filename)))

;;*** complete english words
;; (defun ac-expand-dabbrev ()
;;   (interactive)
;;   (when (not (featurep 'ac-dabbrev)) (require 'ac-dabbrev))
;;   (flet ( (ac-dabbrev-get-candidates (abbrev)
;;                                      '(ac-dabbrev-get-limit-candidates abbrev t)) )
;;     (let ( (ac-sources '(ac-source-abbrev ac-source-dabbrev))
;;            (ac-candidate-max 50)
;;            )
;;       (call-interactively 'ac-start))))

;;(define-key global-map (kbd "C-M-/") 'ac-expand-dabbrev)
(define-key global-map (kbd "C-. w") 'ac-complete-words-in-all-buffer)

;; (defun ac-expand-english-words ()
;;   "complete english words."
;;   (interactive)
;;   (find-file-noselect "/usr/share/dict/words")
;;   (call-interactively 'ac-expand-dabbrev))

(defun ac-expand-english-words ()
  (interactive)
  (if (file-exists-p "/usr/share/dict/words")
      (find-file-noselect "/usr/share/dict/words")
    (if (file-exists-p "~/.emacs.d/etc/words")
        (find-file-noselect "~/.emacs.d/etc/words")))
  (call-interactively 'ac-complete-words-in-all-buffer))

(define-key global-map (kbd "ESC C-M-/") 'ac-expand-english-words)
(define-key global-map (kbd "C-. W") 'ac-expand-english-words)

;;*** ispell
(defun ac-ispell-get-candidates ()
  (let ((word (car (ispell-get-word nil "\\*")))
        (interior-frag nil))
    (lookup-words (concat (and interior-frag "*") word
                    (if (or interior-frag (null ispell-look-p))
                    "*"))
                  ispell-complete-word-dict)))

(eval-after-load "auto-complete"
  `(progn
     (ac-define-source ispell
       '((symbol . "i")
         (candidates . ac-ispell-get-candidates)))
     ))
 
(defun ac-expand-ispell-word ()
  (interactive)
  (require 'auto-complete-config)
  (let ((ac-sources '(ac-source-ispell)))
    (call-interactively 'ac-start)))
 
(define-key global-map (kbd "C-. $") 'ac-expand-ispell-word)

;;*** misc
(define-key global-map (kbd "C-. i") 'ac-complete-imenu)
(define-key global-map (kbd "C-. y") 'ac-complete-yasnippet)


;;** completion-ui
(autoload 'complete-dabbrev "completion-ui" nil t)
(autoload 'complete-etags   "completion-ui" nil t)
(autoload 'complete-files   "completion-ui" nil t)

;; NOTE: `C-,' couldn't be recognized on terminal
(define-key global-map (kbd "C-, d") 'complete-dabbrev)
(define-key global-map (kbd "C-, t") 'complete-etags)
(define-key global-map (kbd "C-, f") 'complete-files)
;;(define-key global-map (kbd "C-, s") 'complete-symbol) ;;elisp
;;(define-key global-map (kbd "C-, >") 'complete-nxml)
;;(define-key global-map (kbd "C-, <") 'complete-nxml)
(define-key global-map (kbd "C-, $") 'complete-ispell)

(autoload 'complete-ispell-lookup "completion-ui-more-source")
(define-key global-map (kbd "C-, $") 'complete-ispell-lookup)


;;** pabbrev ...
;;TODO: pabbrev...

