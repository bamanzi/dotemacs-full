;;* General settings for programming

;;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(which-func-mode t)
(when (require 'bmz-misc nil t)
  ;; move which-func to the front of mode-line
  (mode-line-uninstall-element 'which-func-mode)
  (mode-line-install-element 'which-func-format)

  (define-key which-func-keymap (kbd "<mode-line> <mouse-2>") 'imenu)
  
  )


;;** compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))
(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)


;;** imenu
(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)
(define-key goto-map "i" 'idomenu)
(define-key goto-map "I" 'imenu)

(global-set-key (kbd "<left-fringe> <mouse-3>") 'imenu)


(defun reset-imenu-function ()
  (interactive)
  (setq imenu-create-index-function 'imenu-default-create-index-function))


;;*** imenu-tree window
(autoload 'imenu-tree "imenu-tree" "Display tree view of imenu." t)
(defun bmz/imenu-tree (arg)
  (interactive "P")
   ;; delete other-buffers' tree
  (let ( (buffer (get-buffer "*imenu-tree*")) )
    (if buffer
        (with-current-buffer buffer
          (beginning-of-buffer)
          (ignore-errors
            (tree-mode-delete (tree-mode-tree-ap))))))
  ;; build current buffer's tree
  (imenu-tree arg)
  (if (featurep 'ide-skel)
      (add-to-list 'ide-skel-tabbar-hidden-buffer-names-regexp-list "^\\*imenu-tree\\*$"))
  ;; make this window sticky to buffer '*imenu-tree*'
  (let* ( (windows (delq nil (mapcar '(lambda (window)
                                       (if (equal "*imenu-tree*" (buffer-name (window-buffer window)))
                                           window
                                         nil))
                                  (window-list))))
          (window (car windows)) )
    (when window
        (set-window-dedicated-p window t)
        (with-selected-window window
          (tree-mode-expand-level 2)))))


;;** tags (etags, ctags, cscope, gnu global)
(define-key goto-map "e" 'find-tag)

;;anything-c-etag-select


;;** list & choose method
(defun bmz/select-method()
  (interactive)
  (require 'idomenu "idomenu" t)  
  (cond
   ( (and (fboundp 'anything-browse-code)
	  (memq major-mode '(emacs-lisp-mode lisp-interaction-mode python-mode)))
     (call-interactively 'anything-browse-code))
   ( (fboundp 'anything-imenu)
     (call-interactively 'anything-imenu) )
   ( (and (fboundp 'semantic-mode)
          (memq major-mode '(emacs-lisp-mode c-mode java-mode python-mode))
          (memq 'semantic-mode minor-mode-alist))
     (when (require 'eassist "eassist" t)  ;; for `eassist-list-methods'
       (call-interactively 'eassist-list-methods)))
   ( (fboundp 'idomenu)
     (call-interactively 'idomenu) )
   (t
    (call-interactively 'imenu))))

(global-set-key (kbd "C-c C-o") 'bmz/select-method)
(global-set-key (kbd "<f5> I") 'bmz/select-method)
