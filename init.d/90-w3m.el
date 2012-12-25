
;;** w3m
(define-key search-map (kbd "RET") 'browse-url)
(define-key goto-map   (kbd "RET") 'browse-url)

;; use w3m as default browser. you need to install w3m binary[1] and w3m elisp library[2]
;; [1]  http://w3m.sourceforge.net
;;      cygwin binrary:  http://www.gi.kernel.org/sites/sourceware.cygnus.com/pub/cygwin/release/w3m/
;; [2]  http://emacs-w3m.namazu.org/
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;(setq browse-url-browser-function 'w3m-browse-url)

(if (boundp 'cygwin-root-path)
    (setq w3m-command (concat cygwin-root-path "/bin/w3m.exe")))

(define-key goto-map (kbd "M-RET") 'w3m-browse-url)

;; default to new tab
(defun w3m-new-tab ()
  (require 'w3m)
  (interactive)
  (w3m-copy-buffer nil nil nil t))

 (defun w3m-browse-url-new-tab (url &optional new-session)
   (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
   (w3m-new-tab)
   (w3m-browse-url url new-session))
;;(setq browse-url-browser-function 'w3m-browse-url-new-tab)

;;(define-key search-map (kbd "C-M-RET") 'w3m-browse-url-new-tab)
;;NOTE: use C-u M-x w3m-browse-url for a new tab


 (eval-after-load "w3m"
   `(progn
      (setq w3m-use-title-buffer-name nil)
     (setq browse-url-browser-function 'w3m-browse-url)
	 
     (define-key w3m-mode-map (kbd "<down>") 'next-line)
     (define-key w3m-mode-map (kbd "<up>") 'previous-line)
     (define-key w3m-mode-map (kbd "<left>") 'backward-char)
     (define-key w3m-mode-map (kbd "<right>") 'forward-char)

     (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
     (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
     (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
     (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-next-page)
       
    ))
      
