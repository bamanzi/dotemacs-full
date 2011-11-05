;;; outline-org-like.el - Use org-mode style headings in other major modes.

;;* Org-mode style headings in comment
;;** (comments & header)
;; Author: bamanzi AT gmail DOT com
;; All right reversed.

;;; Commentary:
;; This package allow you to use org-mode style headings in other major modes.
;;
;; Features:
;; 1. org-style heading highlighting
;; 2. org-style cycling heading visibilities with just one key (`H-tab' by default)
;; 3. org-style move-up/down/promote/demote heading
;; 4. other outline operations proxied via another prefix key (`C-z' by default)
;;
;; Usage:
;;   Use `*' `**' `***' in comments as outline heading in any major modes.
;;   Then you can either:
;;      1. To leave you own `outline-regexp' setting untouched, use `outline-org-heading-mode' 
;;         to turn on the heading highlighting, and use `outline-org/outline-command-dispatcher'
;;         to navigate/fold the org-mode-style headings (and your original keybindings
;;         for `outline-minor-mode' could be used.)
;;      2. For convinience, you can also use `outline-org-mode', this would change the
;;         `outline-regexp' settings. But you can use `outline-minor-mode' keybindings
;;         to navigate/fold the org-mode-style headings.
;;

;;; Code:

(require 'outline)
(require 'outline-magic) ;; for `outline-cycle'
(require 'org)           ;; for face org-level-1..6

;;** we'll use an internal `outline-regexp' value
(defun outline-org/get-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (let ((comment-starter (replace-regexp-in-string
						  "[[:space:]]+" "" comment-start)))
	(when (string= comment-start ";")
	  (setq comment-starter ";;"))
 ;; (concat "^" comment-starter "\\*+")))
	(concat "^" comment-starter "[*]+ ")))

;;** heading highlighting
(defun outline-org/get-heading-font-lock-keywords ()
  (let ( (outline-regexp (outline-org/get-outline-regexp)) )
    (let ( (heading-1-regexp
            (concat (substring outline-regexp 0 -1) "\\{1\\} \\(.*\\)"))
           (heading-2-regexp
            (concat (substring outline-regexp 0 -1) "\\{2\\} \\(.*\\)"))
           (heading-3-regexp
            (concat (substring outline-regexp 0 -1) "\\{3\\} \\(.*\\)"))
           (heading-4-regexp
            (concat (substring outline-regexp 0 -1) "\\{4,\\} \\(.*\\)")) )
      `((,heading-1-regexp 1 'org-level-1 t)
            (,heading-2-regexp 1 'org-level-2 t)
            (,heading-3-regexp 1 'org-level-3 t)
            (,heading-4-regexp 1 'org-level-4 t)))))        

(define-minor-mode outline-org-heading-mode
  "org-mode like heading highlighting."
  nil
  :group 'outline
  (let ( (keywords (outline-org/get-heading-font-lock-keywords)) )
    (if outline-org-heading-mode
      (font-lock-add-keywords nil keywords)
    (font-lock-remove-keywords nil keywords)))
  (font-lock-mode -1)
  (font-lock-mode t)
  )


;; This would use another `outline-regexp' value inside to find the headings,
;; you don't need to change your own settings. Thus you can use `C-c @ C-u' for your old
;; `outline-up-heading', along with the new `C-z C-u' (which would behaves according
;; org-style headings.)

(defun outline-org/outline-command-dispatcher (key)
  (interactive "KOutline operation: ")
  (let ( (outline-regexp (outline-org/get-outline-regexp))
         (command (lookup-key outline-mode-prefix-map key)) )
    (if (or (equal key (kbd "<f1>"))
            (equal key (kbd "<f1> <f1>")))  
          (describe-variable 'outline-mode-prefix-map)
      (if (and command (commandp command))
          (progn
            (message "%s" command)
            (call-interactively command))
      (message "no command for that key in `outlint-mode-prefix-map'.")))))

;; Use `C-z' as prefix key for other outline commands
;;  e.g. C-z C-u similar to C-c @ C-u, but use our `outline-regexp'
;;(global-set-key (kbd "C-z") 'outline-org/outline-command-dispatcher)


;;** our new `outline-cycle'
(defun outline-org/outline-cycle ()
  (interactive)
  (let ( (outline-regexp (outline-org/get-outline-regexp)) )
    (if (and (not outline-minor-mode) (not (eq major-mode 'outline-mode)))
        (outline-minor-mode t))
    (if (not outline-org-heading-mode)
        (outline-org-heading-mode t))
    (call-interactively 'outline-cycle)))

;;TODO: wrap outline-promote/demote, outline-move-subtree-up

(defvar outline-regexp-old nil "backup of value of `outline-regexp")

(define-minor-mode outline-org-mode
  "A special `outline-minor-mode' that use org-mode-style headings."
  nil
  :group 'outline
  (let ( (keywords (outline-org/get-heading-font-lock-keywords)) )
    (if outline-org-mode
        (progn  ;; to turn on
          (if (not outline-minor-mode)
              (outline-minor-mode t))
          (set (make-local-variable 'outline-regexp-old) outline-regexp)
          (setq outline-regexp (outline-org/get-outline-regexp))
          (if (not outline-org-heading-mode)
              (outline-org-heading-mode t))
          (hide-body))
      (progn
        (setq outline-regexp-old outline-regexp)
        (outline-org-heading-mode -1))
        )))


(provide 'outline-org-like)
