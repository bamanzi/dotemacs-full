;;; delphi-ext.el -- Make pascal-mode supports object pascal better

;; Author: Ba Manzi <bamanzi@gmail.com>
;; Keywords: pascal, delphi, freepascal, imenu
;; Version: 0.3

;; This is not part of GNU Emacs

;;; Commentary:

;; This package enhanced `pascal-mode' (and `delphi-mode') in some
;; aspects:
;;  + make it supports object pascal (delphi & freepascal) better:
;;    + syntax highlighting keywords and some common datatype of object
;;      pascal (class, interface, public etc)
;;    + imenu: now records, classes & interfaces is recognized and
;;      shown in level 1 menu, method shown as submenu
;;  + other imenu improvements:
;;    + correctly jump to the implementation part (i.e. rather than
;;      interface part)
;;    + works even if there's no `interface/implementation' (some
;;      freepascal code)
;;  + jump from class/method declaration to implementation, or vice
;;    versa (similar to Delphi IDE's Ctrl+Shift+Up/Down)
;;  + auto guess compile command (see `pascal-compile')

;; How to use:
;;   Just load this package, it would take care almost everything.
;;  
;;     (eval-after-load "pascal" `(require 'delphi-ext))
;;     (eval-after-load "delphi" `(require 'delphi-ext))

;;   As `delphi-mode' is implemented in a weired way, which prevent us
;;   adding more font-lock keywords (such as `highlight-regexp',
;;   `hightlight-symbol'), thus I recommend you use `pascal-mode':
;;
;;     (add-to-list 'auto-mode-alist '("\\.dpr$" . pascal-mode))
;;     (add-to-list 'auto-mode-alist '("\\.pp$" . pascal-mode))

;;   For command `pascal-compile', you need to add a keybinding by
;;   yourself:
;;       (eval-after-load "pascal"
;;         `(progn
;;            (define-key pascal-mode-map (kbd "<C-f9>") 'pascal-compile)))

;;; Code:

(require 'pascal)
(require 'delphi)

;;; ** highlight more - pascal-mode
;; for better supporting Object Pascal
(defconst object-pascal-keywords
  '("class" "interface" "uses" "as" "is" "while" "unit" "try" "except"
    "finally" "message" "private" "property" "public" "protected"
    "resourcestring" "implementation" "initialization" "finalization")
  "Object Pascal keywords not supported by pascal.el.")

(defconst pascal-plus-data-types
  '("integer" "string" "char" "shortint" "smallint" "longint" "longword"
    "int64" "byte" "word" "cardinal" "dword" "qword" "null" "variant"
    "pointer" "set" "tdatetime" "tobject")
  "Some base data types of freepascal/delphi.")

(defconst pascal-plus-functions
  '("exit" "break" "continue" "assert" "inc" "dec" "copy" "length"
  "setlength" "sizeof" "assigned" "ord" "pred" "succ" "new" "dispose"
  "allocmem" "getmem" "freemem" "low" "high" "lo" "hi" "include" "exclude")
  "Some base function/procedure of freepascal/delphi.")

(defconst pascal-plus-constants
  '("true" "false" "nil")
  "Some constants.")

(defconst pascal-plus-todos
  '("TODO" "FIXME" "NOTE" "BUG")
  "ToDo marks.")

(let ( (object-pascal-keywords-re (regexp-opt object-pascal-keywords 'words))
       (pascal-plus-data-types-re (regexp-opt pascal-plus-data-types 'words))
       (pascal-plus-functions-re  (regexp-opt pascal-plus-functions  'words))
       (pascal-plus-constants-re  (regexp-opt pascal-plus-constants  'words))
       (pascal-plus-todos-re      (regexp-opt pascal-plus-todos      'words)) )
  (font-lock-add-keywords 'pascal-mode
                          `( (,object-pascal-keywords-re  1 font-lock-keyword-face) 
                             (,pascal-plus-data-types-re  1 font-lock-type-face)
                             (,pascal-plus-functions-re   1 font-lock-function-name-face)
                             (,pascal-plus-constants-re   1 font-lock-constant-face)
                             (,pascal-plus-todos-re       1 font-lock-warning-face prepend))))


;;;_. use pascal-mode for object pascal
(defun pascal-mode-for-objpas-init ()
  ;; make // starts the comment line
  (modify-syntax-entry ?/   ". 12b" pascal-mode-syntax-table)
  (modify-syntax-entry ?\n  "> b"   pascal-mode-syntax-table)

  (setq comment-start "// "
        comment-end "")

  ;; make `mark-defun' works like other major-modes
  (set (make-variable-buffer-local 'beginning-of-defun-function) 'pascal-beg-of-defun)
  (set (make-variable-buffer-local 'end-of-defun-function)       'pascal-end-of-defun)
  (define-key pascal-mode-map (kbd "C-M-h") nil)
  
  ;;add try/except/finally
  (defconst pascal-beg-block-re "\\<\\(begin\\|case\\|record\\|repeat\\|try\\|except\\|finally\\)\\>")
  ;; add finally
  (defconst pascal-noindent-re "\\<\\(begin\\|end\\|until\\|else\\|finally\\)\\>")

  )

(add-hook 'pascal-mode-hook 'pascal-mode-for-objpas-init)

;;redefine pascal-mark-defun, to activate the region
(defun pascal-mark-defun ()
  "Mark the current pascal function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (pascal-beg-of-defun)
  (push-mark (point) t t)
  (pascal-end-of-defun))


;;; ** highlight more - delphi-mode
;; font-lock-add-keywords won't work for delphi-mode,
;; thus I have to override `delphi-face-of' and `delphi-word-token-at'

(defconst delphi-data-types
  '( integer shortint smallint longint longword int64 byte word cardinal dword
             qword boolean bytebool longbool real
             char string shortstring ansistring widestring pchar
             array record set file  pointer variant tdatetime
             )
  "Delphi/FreePascal built-in data types")

(defconst delphi-system-funcs
  '( allocmem assert assigned break continue copy dec dispose exit exclude
              freemem getmem hi high inc include length lo low new
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

;;; * IMenu improvments for delphi/pascal: better support for Object Pascal
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
      (progn    ;; if we have interface..implementation
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



(require 'delphi)
(require 'pascal)

;;; ** jump to declaration/implementation
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

;;###autoload
(defun delphi-jump-to-declaration ()
  "When cursor is in class/method implmentation part, jump to corresponding
 declaration. Similar to Delphi's Ctrl+Shift+Up."
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

;;###autoload
(defun delphi-jump-to-implementation ()
  "When cursor is in class/method 'interface' part, jump to corresponding implementation.
Similar to Delphi's Ctrl+Shift+Down."
  (interactive)
  (let ( (pt (delphi-search-func-implementation)) )
    (if pt
        (progn
          (push-mark)
          (goto-char pt)
          (beginning-of-line)
          (point))
      (message "Implementation of this function not found."))))

;;###autoload
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

  (if (delphi-search-func-implementation)
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
(define-key delphi-mode-map (kbd "<C-S-down>") 'delphi-jump-to-implementation)
(define-key delphi-mode-map (kbd "<C-S-c>")    'delphi-complete-class)
(define-key pascal-mode-map (kbd "<C-S-up>")   'delphi-jump-to-declaration)
(define-key pascal-mode-map (kbd "<C-S-down>") 'delphi-jump-to-implementation)
(define-key pascal-mode-map (kbd "<C-S-c>")    'delphi-complete-class)


;;; ** set compiler command
(defun pascal-compile ()
  "Auto guest compile command for current buffer.
1. try to guest it's freepascal or delphi.
2. try to find the project file."
  (interactive)
  (let* ( (filename (or (buffer-file-name) (buffer-name)))
          (ext (downcase (substring filename (- (length filename) 3)))) 
          (prjfile (if (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "program\> \w+\W;" nil t))
                       filename
                     (car (directory-files default-directory t "\\.[ld]pr$"))))
          (compiler-type (cond
                          ( prjfile
                            (if (string-match-p "lpr$" prjfile)
                                'fpc
                              'delphi) )
                          ( (member ext '("pp"))
                            'fpc )
                          ( (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "{$mode " nil t))
                            'fpc)
                          ( (eq system-type 'windows-nt)
                            'delphi)
                          (t
                           fpc))))
    (setq compile-command (if (eq compiler-type 'delphi)
                               (format "dcc32 -Q %s" (or prjfile filename))
                             (format "fpc %s" (or prjfile filename))))
    (call-interactively 'compile)))


;;; *** how to parse compiler's output
  ;; e.g. D:\Workspace\iso\iso_io.pas(47) Warning: Comparison always evaluates to True
(add-to-list 'compilation-error-regexp-alist-alist
             '(dcc32 "^\\([a-zA-Z0-9._:\\]+\\)(\\([0-9]+\\)) \\(Error\\|Warning\\|Fatal\\|Hint\\): \\(.*\\)$" 1 2 3))
;; fpc
(add-to-list 'compilation-error-regexp-alist-alist
             '(fpc "^\\([a-zA-Z0-9.]+\\)(\\([0-9]+\\),\\([0-9]+\\)) \\(Error\\|Warning\\|Fatal\\): \\(.*\\)$" 1 2 3))

(defun pascal-mode-init-compiler ()
  (add-to-list 'compilation-error-regexp-alist 'fpc)
  (add-to-list 'compilation-error-regexp-alist 'dcc32)

  (define-key pascal-mode-map (kbd "<C-f9>") 'pascal-compile)
  (define-key delphi-mode-map (kbd "<C-f9>") 'pascal-compile)  
  )

(add-hook 'delphi-mode-hook 'pascal-mode-init-compiler)
(add-hook 'pascal-mode-hook 'pascal-mode-init-compiler)

;;(add-to-list 'auto-mode-alist '("\\.dpr$" . pascal-mode))
;;(add-to-list 'auto-mode-alist '("\\.pp$" . pascal-mode))

(provide 'delphi-ext)

;;;delphi-ext.el ends here
