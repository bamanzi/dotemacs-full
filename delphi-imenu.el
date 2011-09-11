
;; bamanzi <bamanzi@gmail.com>

;;{{{ IMenu improvments for delphi/pascal: better support for Object Pascal
;; - support 'record', 'class' and 'interface' as level 1 menu
;; - methods show as submenu of class
;; - correctly jump to the implementation part (i.e. rather than interface part)
;; - works even if there's no `interface/implementation' (some freepascal code)
;; Based on code from http://www.emacswiki.org/emacs/DelphiMode

(defvar imenu--function-name-regexp-delphi
  (concat
   "^[ \t]*\\(function\\|procedure\\|constructor\\|destructor\\)[ \t]+"
   "\\([_a-zA-Z][_a-zA-Z0-9]*\\.\\)?"   ; class?
   "\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
  "Re to get function/procedure names in Delphi.")

(defvar imenu--type-name-regexp-delphi
   (concat "^[ \t]*\\([a-zA-Z0-9]*\\)[ \t]*=[ \t]*\\(class\\|record\\|interface\\)"
           "")
   "regexp to get class/record namesin Delphi.")

(defun imenu--create-delphi-index-enh (&optional regexp)
  (let ((index-alist '())
		(progress-prev-pos 0)
		(case-fold-search t))
	(goto-char (point-min))
	(imenu-progress-message progress-prev-pos 0)
    ;;first, scan interface part for types
	(if (eq nil (re-search-forward "interface" nil t)) 
        (goto-char (point-min))
      (progn    ;; if we have interface..implementaion
       (save-match-data
         (while (re-search-forward
                 (or regexp imenu--type-name-regexp-delphi)
                 nil t)
           (imenu-progress-message progress-prev-pos)
           (let ((pos (save-excursion
                        (beginning-of-line)
                        (if imenu-use-markers (point-marker) (point))))
                  (sub-alist '())
                 (type-name (match-string-no-properties 1)))
              (progn
                (push (cons "(declaration)" pos) sub-alist)
                (push (cons (format "%s." type-name) sub-alist) index-alist))))
         )
       ))
    ;;now, scan implementation part for methods (and other functions)
    (goto-char (point-min))
    (if (eq nil (re-search-forward "implementation" nil t)) ;;advance to the interface part
        (goto-char (point-min))    
      (save-match-data
        (while (re-search-forward
                (or regexp imenu--function-name-regexp-delphi)
                nil t)
          (imenu-progress-message progress-prev-pos)
          (let* ((pos (save-excursion
                        (beginning-of-line)
                        (if imenu-use-markers (point-marker) (point))))
                 (class-name (match-string-no-properties 2))
                 (function-name (match-string-no-properties 3))
                 (sub-menu (assoc class-name index-alist)))
            (if (eq nil sub-menu)
                (push (cons (format "%s%s()"
                                    (if (eq nil class-name) "" class-name)
                                    function-name)
                            pos)
                      index-alist)
              (setcdr sub-menu (cons (cons function-name pos) (cdr sub-menu)))
              )
            )))
      )
	(imenu-progress-message progress-prev-pos 100)
	(nreverse index-alist))) 

(defun delphi-pascal-init-imenu ()
  (require 'imenu)
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-create-index-function
        #'imenu--create-delphi-index-enh)
  (imenu-add-menubar-index))

(add-hook 'delphi-mode-hook 'delphi-pascal-init-imenu)
(add-hook 'pascal-mode-hook 'delphi-pascal-init-imenu)
;;}}}


