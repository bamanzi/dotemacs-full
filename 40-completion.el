;;* buffer completion
;; (for minibuffer completion, see 25-minibuffer.el)

;;** built-in mechanism
;; Emacs default:
;; M-TAB    complete-symbol     (< 23.2)
;;          completion-at-point (>=23.2)

;; use TAB for completion
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete)
  (require 'smart-tab nil t)
  )

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
(eval-after-load "auto-complete"
  `(progn
     (if (require "auto-complete-config" t)
         (progn      
           (ac-config-default)
           (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)
           
           ;;(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)

           (if (load "auto-complete-scite-api" t)
               (add-to-list 'ac-sources 'ac-source-scite-api)
             (message "%s: failed to load `auto-complete-scite-api'." load-file-name))
           )
       (message "%s: failed to load `auto-complete'." load-file-name))
  ))

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
  `(define-key global-map (kbd "C-/") 'ac-expand-filename))

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
(define-key global-map (kbd "C-M-/") 'ac-complete-words-in-all-buffer)

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
(define-key global-map (kbd "C-, w") 'ac-expand-english-words)

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

