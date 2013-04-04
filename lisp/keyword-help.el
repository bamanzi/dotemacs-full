;;; keyword-help.el -- various way to query documentation on current symbol

;; Copyright (C) 2011 - 2013 Ba Manzi <bamanzi@gmail.com>

;; Author: Ba Manzi
;; Keywords: help documentation winhelp chm mshelp devhelp info
;; URL: https://bitbucket.org/bamanzi/dotemacs-full/src/default/lisp/
;; Version: 0.2

;; This file is NOT part of GNU Emacs.

;;; Usage

;; This package provides an unified way (`keyword-help-lookup') to
;; query documentation for current symbol.

;; Features:
;;   - supports Info, WinHelp(.hlp), HtmlHelp (.chm), MSHelp 2, DevHelp & web url
;;   - supports Multiple help sources for each major mode

;; How to use:
;; 1. customize `keyword-help-lookup-alist'
;; 2. invoke `keyword-help-lookup'

;;; Code

;; various backends

(defun keyword-help-lookup-hlp (keyword file-path)
   "Call winhlp32 to display documentation on KEYWORD."
   (interactive "sKeyword: \nfHlp File: ")
   (start-process "winhlp32" nil "winhlp32.exe"
		  "-k" keyword
		  file-path)
   (set-process-query-on-exit-flag (get-process "winhlp32") nil)
   )

;; CHM (HtmlHelp)
;;note: KeyHH.exe needs to be in $PATH.
;;KeyHH -MyHelp -#klink "ActiveX Control Wizard" htmlhelp.chm
(defun keyword-help-lookup-chm (keyword file-path)
   "Call keyhh to command hh.exe to display documentation on KEYWORD."
   (interactive "sKeyword: \nfSHelp-file: ")
   (start-process "keyhh" nil "keyhh.exe"
		  (concat "-" mode-name) ;; use mode name as ID
		  "-#klink" (format "'%s'" keyword)
		  file-path)
   (set-process-query-on-exit-flag (get-process "keyhh") nil)
   )

(defun keyword-help-lookup-info (keyword)
  (interactive "Skeyword: ")
  (info-lookup-symbol keyword))


;; MS Help 2 (MSDN)
;; http://www.emacswiki.org/emacs/MsdnHelp
;; You need `h2viewer' utility from 
;; 	http://www.helpware.net/mshelp2/h2viewer.htm
;; invoke it as this:
;; 	h2viewer /helpcol MS.PSDK.1033 /keyword K$CreateWindowEx
(defun keyword-help-lookup-hh2 (keyword helpcol)
  "Open a window showing the MSDN documentation for the word under the point"
  (interactive)
  (start-process "h2viewer" nil 
                 "h2viewer.exe"
                 "/appID" "MSDN-Viewer"
                 "/helpcol" helpcol
                 ;; 		     "/filtername" "Visual C++"
                 "/keyword" (concat "K$" (current-word)))
  (set-process-query-on-exit-flag (get-process "h2viewer") nil))


;; GNOME DevHelp
;; got from devhelp's source package
(defun keyword-help-lookup-devhelp (keyword)
  "Searches for the current word in Devhelp"
  (start-process-shell-command "devhelp" nil "devhelp" "-s" keyword)
  (set-process-query-on-exit-flag (get-process "devhelp") nil))


(defun keyword-help-lookup-web (keyword url)
  ;;TODO: implement this
  ;;FIXME: use `webjump'?
  )


;; front-end
(setq keyword-help-lookup-alist      
  '(
    (python-mode ("default" chm "e:\\python\\ActivePython27.chm")
                  ("online"  pylookup)
                  ("django"  pylookup))
    (pascal-mode ("default" hlp "d:\\Borland\\Delphi7\\Help\\d7.hlp")
                  ("d2009" chm "f:\\_Cloud\\borland\\delphi2009\\delphivclwin32.chm")
                  ("lazarus" chm "e:\\lazarus\\docs/chm\\lazarus.chm"))
    (emacs-lisp-mode ("default" chm "e:\\emacs\\doc\\elisp-24.3.chm")
                      ("info" info))
    (xahk-mode ("default" chm  "d:\\Programs\\AutoHotkey\\AutoHotkey-chs.chm"))
    (ahk-mode . xahk-mode)
    (php-mode web  "http://www.php.net/manual/en/")
    (".afaf" hlp "afafa")))
    
(defun keyword-help--get-mode-config (majormode)
  (let ((cfg (cdr (assq majormode keyword-help-lookup-alist))))
    (if cfg
        (if (symbolp cfg)
            (keyword-help--get-mode-config cfg)
          cfg))))


(defun keyword-help-lookup (keyword &optional category)
  "Invoke documentation query backends for KEYWORD.

Without prefix key, only the 'default' help source would be invoked.
If invoked with prefix key, it would let you choose which source to invoke."
  (interactive
   (list (read-string "Keyword: "
                      (if (and transient-mark-mode mark-active)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (thing-at-point 'symbol))
                      )
         (if current-prefix-arg
             (completing-read "Category: "
                              (mapcar 'car (keyword-help--get-mode-config major-mode))
                              nil
                              t
                              "default"))))
  (let* ((all-sources (keyword-help--get-mode-config major-mode))
         (category (or category "default"))
         (help-source (if all-sources                 
                   (assoc category all-sources)))
         (method (if help-source
                     (concat "keyword-help-lookup-" (symbol-name (nth 1 help-source)))
                   "keyword-help-lookup-default"))
         (params (if help-source
                     (cddr help-source))))
    (if help-source
        (if (inter-soft method)
            (progn
              (message "Calling '%s' for \"%s\" with params: %s" method keyword params)
              (apply (intern func) keyword params))
          (message "No backend for: %s" (symbol-name (nth 1 help-source))))
      (message "No configuration for '%s' in `keyword-help-lookup-alist'" major-mode)          
      )))

(define-key global-map (kbd "<C-f1>") 'keyword-help-lookup)


;;; OLD IMPLEMENTATION
(defcustom keyword-help-url nil
  "Help file path used to show keyword sensitive help.
  .CHM and .HLP format are supported."
  :type 'string)

 "Paths of Help files used to show keyword sensitive help."
;; Major mode name -> help file path
(setq keyword-help-url-alist 
  '( ("Pascal"		 "e:/lazarus/docs/chm/lazarus.chm")
     ("Delphi"		 "d:/Borland/Delphi7/Help/d7.hlp")
     ;;("PHP"		 "e:/Apache2.2/php_manual_en.chm")
     ("PHP"		 "http://php.chinaunix.net/manual-lookup.php?pattern=%s")
     ("Python"		 "e:/Python/ActivePython25.chm")
     ("Pascal"		 "e:/lazarus/docs/chm/lazarus.chm")
     ("Emacs-Lisp"	 "e:/emacs/doc/manual-chm/elisp.chm")
     ("Help"		 "e:/emacs/doc/manual-chm/elisp.chm")
     ("Lisp Interaction" "e:/emacs/doc/manual-chm/elisp.chm")
     ("AHK"		 "d:/Programs/AutoHotKey/AutoHotkey-chs.chm")
     ;;("AHK"              "http://www.autohotkey.com/docs/commands/%s.htm")
     ("AutoIt"		 "D:/Programs/AutoIt3/AutoIt3.chm")
     ("Java"		 "d:/java/jdk140.chm")
     ("iss"		 "d:/Program Files/Inno Setup 5/ISetup.chm")
     ("Perl"		 "http://perldoc.perl.org/search.html?q=%s")
     ("AWK/l"		 "E:/docs/gnuwin32/gawk.chm")
     ))

(defun keyword-help()
  "Show the help info for the keyword at point. CHM and HLP are supported.
  
You need to set `keyword-help-url-alist' or `keyword-help-url'
before using this.  By default, the path of help file is
looked-up from `keyword-help-url-alist' by mode name.  If any
ARG given, `keyword-help-url' is used."
  (interactive)
  
  (let ( (typeid    mode-name)
	 (help-url (or (car (cdr (assoc mode-name keyword-help-url-alist)))
			keyword-help-url))
	 (keyword   (if (and transient-mark-mode mark-active)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol)) ))
    (when help-url
      (if (string= "http" (substring help-url 0 4))
	  (keyword-help-lookup-web help-url keyword)				   
	(let* ( (help-file help-url)
		(ext (downcase (substring help-file (- (length help-file) 3)) )) )
	  (if (file-exists-p help-file)
	      (progn
		(message "invoke help for '%s': %s " keyword help-file)
		(cond
		 ((string= ext "chm") (keyword-help-lookup-chm help-file keyword))
		 ((string= ext "hlp") (keyword-help-lookup-hlp help-file keyword))))
	    (message "Help file not exist: %s" help-file)))))
	))
  
(provide 'keyword-help)

  
