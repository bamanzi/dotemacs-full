;;* General settings for programming

;;** comment
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(defun comment-or-uncomment-line (arg)
  (interactive "*P")
  (comment-or-uncomment-region (line-beginning-position) (line-end-position) arg))

(define-key global-map (kbd "ESC M-;") 'comment-or-uncomment-line)

;;*** another implementation of comment-line
;; if nothing marked, use current line as region
;;http://pastebin.com/G7N4F4eE
(if (require 'bmz-misc nil t)
    (when (fboundp 'allow-line-as-region-for-function)
      (allow-line-as-region-for-function comment-region)
      (allow-line-as-region-for-function uncomment-region)
      (allow-line-as-region-for-function comment-or-uncomment-region)
    ))

;;** compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

;;toggle to parse & highlight error lines in shell-mode etc
(global-set-key (kbd "<S-f9>") 'compilation-minor-mode)


(eval-after-load "flymake"
  `(require 'flymake-cursor nil t))
(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)

;;*** mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "<C-f9>") 'mode-compile)

(autoload 'mode-compile-kill "mode-compile"
   "Command to kill a compilation launched by `mode-compile'" t)
;(global-set-key "\C-ck" 'mode-compile-kill)

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

;;** which-func mode: show the 'nearest' function name
;; which-func-mode used imenu to get the function name, thus check imenu settings if it not work
;; refer `which-func-modes'
(which-func-mode t)

(when (require 'bmz-misc nil t)
  ;; move which-func to the front of mode-line
  (mode-line-uninstall-element 'which-func-mode)
  (mode-line-uninstall-element 'which-func-format)
  (mode-line-install-element 'which-func-format)

  (define-key which-func-keymap (kbd "<mode-line> <C-mouse-3>") 'imenu)
  (define-key which-func-keymap (kbd "<header-line> <C-mouse-3>") 'imenu)
  
  )

;;** tags (etags, ctags, cscope, gnu global)
(define-key goto-map "e" 'find-tag)

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (require 'etags)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tag-names)))
	  tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))
 
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))
 
(global-set-key [remap find-tag] 'ido-find-tag)
;;(global-set-key (kbd "C-M-.") 'ido-find-file-in-tag-files)

;;*** anything-tags
;;anything-c-etag-select


;;** list & choose method
(defun anything-goto-symbol ()
  "Show anything list, using current symbol as input to narrow the choices."
  (interactive)
  (anything
   :prompt "Go to:"
   :candidate-number-limit 10
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-imenu
         anything-c-source-browse-code
         anything-c-source-semantic
        ;; anything-c-source-info-emacs-lisp-intro
         )))

(define-key goto-map "s" 'anything-goto-symbol)


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

(defun bmz/go-to-symbol-within-buffer ()
  "Go to symbol (definition) within current buffer.

This would get rid of some annoyance:
- imenu hererachy (anything flats them)
- imenu not working because of cedet
"
  (interactive)
  (anything
   :prompt "Go to: "
   :input (thing-at-point 'symbol)
   :sources
   '(anything-c-source-imenu
     anything-c-source-browse-code)))

(define-key goto-map "s" 'bmz/go-to-symbol-within-buffer)
   
