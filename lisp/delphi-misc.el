
(require 'delphi)
(require 'pascal)

;;{{{ jump to declaration/implementation
(defun delphi-jump-to-declaration ()
  (interactive)
  (let ( (pt (ignore-errors
               (save-excursion
        (end-of-line)
        (re-search-backward "^\\(procedure\\|function\\|constructor\\|destructor\\)[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\>\\)?\\.?\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
        (let ( (method-type  (match-string-no-properties 1)) 
               (class-name   (if (match-string 3) (match-string-no-properties 2) "" ))
               (func-name    (or (match-string-no-properties 3) (match-string-no-properties 2))) )
          (beginning-of-buffer)
          (if (> (length class-name) 0)
              (re-search-forward (format "%s[ \t]+=" class-name)))
          (when (re-search-forward (format "%s[ \t]+%s\\>" method-type func-name))
              (backward-word)
              (point)))))) )
    (when pt (goto-char pt))))


(defun delphi-parse-function-decl-line ()
  "Parse current line to get the class name, class type, function name, method type."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\(procedure\\|function\\|constructor\\|destructor\\)[ \\t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\(.*\\)"
                             nil
                             'noerror)
      (let ( (func-type  (match-string-no-properties 1))  ;;procedure/function/constructor
             (func-name    (match-string-no-properties 2))
             (func-params  (match-string-no-properties 3))
             type-name
             type-type )
        (setq result (list func-type func-name func-params))
        (let ( (pt-class-end (save-excursion
                               (if (re-search-backward "\\<end;" nil 'noerror)
                                   (point))))
               (pt-class-begin (progn
                                 (if (re-search-backward "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \\t]*=[ \\t]*\\(class\\|interface\\)"
                                                         nil
                                                         'noerror)
                                     (point)))) )
          (if (or (not pt-class-end) ;;no `end;' before current line, what we got is current class/interface
                  (and pt-class-begin pt-class-end
                       (> pt-class-begin pt-class-end))) ;;this current function do lies within a class
            (progn
              (setq type-name (match-string-no-properties 1))
              (setq type-type (match-string-no-properties 2))
              (append result (list type-type type-name)))
          result))))))

      
(defun delphi-search-func-implementation()
  "Search the implementation for the function/procedure in current line.
Return the point of implementation part."
   (save-excursion               
     (let* ( (result      (delphi-parse-function-decl-line))
             (method-type (car result))
             (func-name   (nth 1 result))
             (type-type   (nth 3 result))
             (type-name   (nth 4 result)) )
       (if (string= type-type "interface")
           (message "interface has no implementation part: %s" type-name)
         (progn
           (re-search-forward "^implementation\\>" nil t)
           ;;(message "%s|%s|%s" method-type type-name func-name)
           (if type-name
               (re-search-forward (format "%s[ \\t]+%s\\.[ \\t]?%s\\>" method-type type-name func-name)
                                  nil
                                  'noerror)
             (re-search-forward (format "%s[ \\t]+%s\\>" method-type func-name)
                                nil
                                'noerror)))))))
			
(defun delphi-jump-to-implementation ()
  (interactive)
  (let ( (pt (delphi-search-func-implementation)) )
    (if pt
        (progn
          (push-mark)
          (goto-char pt)
          (beginning-of-line)
          (point))
      (message "Implementation of this function not found."))))

(defun delphi-complete-class ()
  "Create the implementation skeletion for newly declared function/method.

Not only function/procedure in class supported, but also plain function/procedure.
Current limitation: only ONE function could be completed, and it should be
declared on the current line."
  (interactive)        
  (save-excursion
      (when (or (not (re-search-forward "\\<implementation\\>" nil 'noerror))
                (not (re-search-backward "\\<interface\\>"     nil 'noerror)))
        (message "Cursor should be in section between `interface' & `implementation'.")))

  (if (delphi-search-func-implementaion)
      (message "Method already implemented.")
    (let* ( (result      (delphi-parse-function-decl-line))
            (func-type   (car result))
            (func-name   (nth 1 result))
            (func-params (nth 2 result))
            (class-name  (nth 4 result)) )
      (unless (progn
            ;;jump to the implementation of previous function        
            (previous-line)
            (let ( (pt (delphi-search-func-implementation)) )
              (when pt
                  (goto-char pt)
                  (pascal-end-of-defun))
              pt))
        ;; go to the end of implementation part
        (if (re-search-forward "\\<initialization\\>" nil 'noerror)
            (previous-line 2)
          (end-of-buffer)))
      (insert-string (format "\n\n%s %s%s%s\nbegin\n\nend;"
                             func-type
                             (if class-name (concat class-name ".") "")
                             func-name
                             func-params
                             ))
        (previous-line 2)                               
      )))

(define-key delphi-mode-map (kbd "<C-S-up>")   'delphi-jump-to-declaration)
(define-key delphi-mode-map (kbd "<C-S-down>") 'delphi-jump-to-implementaion)
(define-key delphi-mode-map (kbd "<C-S-c>")    'delphi-complete-class)
(define-key pascal-mode-map (kbd "<C-S-up>")   'delphi-jump-to-declaration)
(define-key pascal-mode-map (kbd "<C-S-down>") 'delphi-jump-to-implementaion)
(define-key pascal-mode-map (kbd "<C-S-c>")    'delphi-complete-class)
;;}}}





