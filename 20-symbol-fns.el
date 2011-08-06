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

;; occur
(defun bmz/occur-at-point ()
  (interactive)
  (occur (format "%s" (bmz/get-symbol-selected-or-current))))

(defun bmz/multi-occur-at-point ()
  (interactive)
  ;;FIXME: use multi-occur?
  (multi-occur (format "%s" (bmz/get-symbol-selected-or-current))))

;;TODO: grep
;;...


;;--- overall
(defun init-word-ops-keys (map)

    (define-key map "g" 'bmz/goto-symbol-definition-in-buffer)
    
    (define-key map (kbd "M-.") 'bmz/find-symbol-definition-across-files) ;; Emacs style key
    (define-key map (kbd "C-]") 'bmz/find-symbol-definition-across-files) ;; Vi style key
    (define-key map "G" 'bmz/find-symbol-definition-across-files)
    
    (define-key map (kbd "SPC") 'highlight-symbol-at-point)
    (define-key map (kbd "*") 'bmz/goto-symbol-next-occur)
    (define-key map (kbd "#") 'bmz/goto-symbol-prev-occur)

    (define-key map (kbd "O") 'bmz/occur-at-point)
    (define-key map (kbd "M-o") 'bmz/multi-occur-at-point)

    ;; (define-key map (kbd "f") 'find-function-at-point)
    ;; (define-key map (kbd "v") 'find-variable-at-point)
    ;; (define-key map (kbd "l") 'find-library)
    ;; (define-key map (kbd "C-f") 'ffap-other-window)

    (define-key map (kbd "<f3>") 'isearch-repeat-forward)
    (define-key map (kbd "<S-f3>") 'isearch-repeat-backward)

    (autoload 'sdcv-search "sdcv-mode" nil t)
    (define-key map (kbd "d") 'sdcv-search) ;;sdcv-mode.el needed

    map
    )

(init-word-ops-keys search-map)

(define-key global-map (kbd "<f3>") search-map)


;; other keys
(define-key global-map (kbd "<C-f3>") 'isearch-repeat-forward)
(define-key global-map (kbd "<S-f3>") 'isearch-repeat-backward)

(define-key goto-map (kbd "i") 'bmz/goto-symbol-definition-in-buffer)
(define-key goto-map (kbd "I") 'bmz/find-symbol-definition-across-files)



