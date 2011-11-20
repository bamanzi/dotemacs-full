;;** some special highlights & overlay

;;** highlight symbol
;;*** manually
(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" nil t)

(idle-require 'highlight-symbol)

(global-set-key (kbd "C-c j")          'highlight-symbol-at-point)
(define-key search-map (kbd "j")        'highlight-symbol-at-point)
(define-key search-map (kbd "<up>")   'highlight-symbol-prev)
(define-key search-map (kbd "<down>") 'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

;;*** automatically highlight symbol at point for a short time
(autoload 'idle-highlight "idle-highlight" nil t)
(global-set-key (kbd "<f10> ih") 'idle-highlight)


;;** in-buffer bookmark with visual effects

(idle-require 'bm)

(global-set-key (kbd "<C-f2>")    'bm-toggle)
(global-set-key (kbd "<M-f2>")    'bm-next)
(global-set-key (kbd "<S-f2>")    'bm-previous)
(global-set-key (kbd "<H-f2>")    'bm-show)

(global-set-key (kbd "<left-fringe> <S-mouse-1>")     'bm-toggle-mouse)
(global-set-key (kbd "<left-fringe> <S-wheel-up>")    'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <S-wheel-down>")  'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <S-mouse-2>")     'bm-show)

(autoload 'highlight-indentation "highlight-indentation" "Toggle highlight indentation." t)
(global-set-key (kbd "<f10> hi") 'highlight-indentation)

;;** linkd : links to file/man/info/url
;; e.g. (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode" :display "LinkdMode - EmacsWiki")
;; (linkd-follow (@file :file-name "linkd.el" :to "linkd-mode"))
(autoload 'linkd-mode "linkd.el" "Create or follow hypertext links." t)
(autoload 'linkd-insert-link "linkd.el" "Insert a link." t)
(eval-after-load "linkd"
  `(progn
       ;;;_.. linkd icons path
     (let ( (dir (concat (file-name-directory (locate-library "linkd")) "icons")) )
        (when (file-exists-p dir)
          (setq linkd-icons-directory dir)
          (setq linkd-use-icons t)))))

;; turn
;;    http://www.emacswiki.org/emacs/LinkdMode
;; into:
;;    (linkd-follow (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode"))
(defun linkd-wrap-url (begin end)
  (interactive "r")
  (goto-char end)
  (insert-string "\"))")
  (goto-char begin)
  (insert-string "(linkd-follow (@url :file-name \""))
