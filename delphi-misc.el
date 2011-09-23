
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

;;{{{ highlight more - pascal-mode
;;; for better supporting Object Pascal 
(font-lock-add-keywords 'pascal-mode              
    '( ("\\<\\(class\\|uses\\|as\\|is\\|while\\|until\\|unit\\|private\\|public\\|protected\\|interface\\|implementation\\|resourcestring\\)\\>"  
        1 font-lock-keyword-face)  
       ("\\<\\(integer\\|string\\|char\\|shortint\\|smallint\\|longint\\|longword|\\|int64\\|byte\\|word\\|cardinal\\|dword\\|qword\\|null\\|variant\\|pointer\\|set\\|tdatetime\\)\\>"
        1 font-lock-type-face)  
       ("\\<\\(exit\\|break\\|continue\\|assert\\|inc\\|dec\\|copy\\setlength\\|sizeof\\|assigned\\|ord\\|pred\\|succ\\|new\\|dispose\\|allocmem\\|getmem\\|freemem\\|low\\|high\\|\\lo\\|hi\\|include\\|exclude\\)\\>"
        1 font-lock-function-name-face)  
       ("\\<\\(true\\|false\\|nil\\)\\>"
        1 font-lock-constant-face)  
       ("\\<\\(FIXME\\|TODO\\):"
        1 font-lock-warning-face prepend))) 

;;}}}

;;{{{ highlight more - delphi-mode
;;; font-lock-add-keywords won't work for delphi-mode,
;;; thus I have to override `delphi-face-of' and `delphi-word-token-at'

(defconst delphi-data-types  
  '( integer shortint smallint longint longword int64 byte word cardinal dword qword  
             boolean bytebool longbool real     
             char string shortstring ansistring widestring pchar  
             array record set file  pointer variant tdatetime  
             )  
  "Delphi/FreePascal built-in data types")  
  
(defconst delphi-system-funcs  
  '( allocmem assert assigned break continue copy dec dispose exit exclude freemem  
              getmem hi high inc include length lo low new  
              ord pred reallocmem setlength sizeof str succ val)  
  "Delphi/FreePascal functions in System unit")  

(defcustom delphi-datatype-face 'font-lock-type-face
  "*Face used to color delphi data types."
  :type 'face
  :group 'delphi)

(defcustom delphi-function-face 'font-lock-function-name-face
  "*Face used to color delphi `built-in` functions."
  :type 'face
  :group 'delphi)

;; re-define delphi-face-of
(defun delphi-face-of (token-kind)
  ;; Returns the face property appropriate for the token kind.
  (cond ((delphi-is token-kind delphi-comments) delphi-comment-face)
        ((delphi-is token-kind delphi-strings) delphi-string-face)
        ((string= token-kind '_type) delphi-datatype-face)
        ((string= token-kind '_func) delphi-function-face)
        ((delphi-is token-kind delphi-keywords) delphi-keyword-face)
        (delphi-other-face)))

(defun delphi-word-token-at (p)
  ;; If point p is over a word (i.e. identifier characters), then return a word
  ;; token. If the word is actually a keyword, then return the keyword token.
  (let ((word (delphi-charset-token-at p delphi-word-chars 'word)))
    (when word
      (let* ((word-image (downcase (delphi-token-string word)))
             (keyword (intern-soft word-image)))
        (when (or keyword (string= "nil" word-image))
              (if (delphi-is keyword delphi-keywords)
                  (delphi-set-token-kind word keyword)
                (if (delphi-is keyword delphi-data-types)
                    (delphi-set-token-kind word '_type)
                  (if (delphi-is keyword delphi-system-funcs)
                      (delphi-set-token-kind word '_func)))))
        word))))
;;}}}



