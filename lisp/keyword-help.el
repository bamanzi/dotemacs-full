;;; keyword-help.el -- various way to query documentation on current symbol

;; Copyright (C) 2011 - 2013 Ba Manzi <bamanzi@gmail.com>

;; Author: Ba Manzi
;; Keywords: help documentation winhelp chm mshelp devhelp info
;; URL: https://bitbucket.org/bamanzi/dotemacs-full/src/default/lisp/
;; Version: 0.3

;; This file is NOT part of GNU Emacs.

;;; Usage

;; This package provides an unified way (`keyword-help-lookup') to
;; query documentation for current symbol.

;; Features:
;;   - supports Info, WinHelp(.hlp), HtmlHelp (.chm), MSHelp 2, DevHelp & web url
;;   - supports Multiple help sources for each major mode

;; How to use:
;; 1. customize `keyword-help-source-alist'
;; 2. invoke `keyword-help-lookup'

;;; Code

;;; Various Backends

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
		  "-Emacs"  ;;(concat "-" mode-name) ;; use mode name as ID
		  "-#klink" (format "'%s'" keyword)
		  file-path)
   (set-process-query-on-exit-flag (get-process "keyhh") nil)
   )

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



(defun keyword-help-lookup-info (keyword)
  "Use `info-lookup-symbol' (C-h S) to lookup documentation.

You may need to customize `info-lookup-file-name-alist' to use this method.
Refer `info-lookup-add-help' and `info-lookup-maybe-add-help' for detail info."
  (interactive "Skeyword: ")
  (info-lookup-symbol keyword))


(defun keyword-help-lookup-info-external (keyword node)
  "Use external executable GNU Info to lookup documentation.

Commandd line e.g.:
  info elisp --index-search=mapcar

NOTE: gnu info is needed. pinfo won't work.
NOTE: not work on windows (maybe works on cygwin)."
  (interactive "Skeyword: \nSInfo Node: ")
  (let* ((program "info")
         (args (list node (format "--index-search=\"%s\"" keyword)))
         (term-buf
          (generate-new-buffer
           (format "*%s-%s*" program node)))
         (keyword-help-info-sentinel
          #'(lambda (proc string)
              (message "event %s from process %s" string proc)
              ;;TODO: this doesn't work. how to close buffer automatically
              (let (buffer (process-buffer proc))
                (if buffer
                    (kill-buffer buffer)
                  (message "no buffer found for process %s" proc))))))
        (progn  ;;save-current-buffer
          (switch-to-buffer term-buf)
          (term-mode)
          (term-exec term-buf program program nil args)
          (let ((proc (get-buffer-process term-buf)))
            (if (and proc (eq 'run (process-status proc)))
                (set-process-sentinel proc 'keyword-help-info-sentinel)
              (error "Failed to invoke visual command")))
          (term-char-mode)
          (term-set-escape-char ?\C-x))))

;; GNOME DevHelp
;; got from devhelp's source package
(defun keyword-help-lookup-devhelp (keyword)
  "Searches for the current word in Devhelp"
  (start-process-shell-command "devhelp" nil "devhelp" "-s" keyword)
  (set-process-query-on-exit-flag (get-process "devhelp") nil))

(defun keyword-help-lookup-web (keyword url)
  (require 'url-util)
  (if (string-match "%s" url)
      (setq url (replace-regexp-in-string "%s" (url-hexify-string keyword) url))
    (setq url (concat url (url-hexify-string keyword))))
  (browse-url url)
  )

(defun keyword-help-lookup-cmdline (keyword cmdline &optional bufname)
  (if (string-match "%s" cmdline)
      (setq cmdline (replace-regexp-in-string "%s" keyword cmdline))
    (setq cmdline (concat cmdline (url-hexify-string keyword))))
  (let ((result (shell-command-to-string cmdline)))
         (display-message-or-buffer result (or bufname (format "*%sHelp*" mode-name))))
  )

(defun keyword-help-lookup-elisp (keyword func &optional pkg args)
  "Call an elisp function to lookup documentation."
  (if (and pkg
           (not (require pkg nil t)))
      (message "failed to load package '%s' for '%s'" pkg func)
    (if (functionp func)
        (apply func keyword args)
      (message "error: %s is not a elisp function." (symbol-name func)))))

;;; Sources
(setq keyword-help-source-alist      
  '(
    (fundamental-mode ("*google" web "http://www.google.com.hk/search?q=%s")
                      ("*ddg" web "https://duckduckgo.com/?q=%s"))  
    (python-mode ("chm" chm "e:\\python\\ActivePython27.chm")
                 ("web" web "http://docs.python.org/2/search.html?q=%s&check_keywords=yes&area=default")
                 ("web-py3" web "http://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")
                 ("pydoc" cmdline "pydoc %s")
                 ("pylookup"  pylookup)
                 ("django"  pylookup)
                 ("wx-chm" chm "E:\\docs\\python\\wxPython\\wxpython.chm"))
    (pascal-mode ("d7help" hlp "d:\\Borland\\Delphi7\\Help\\d7.hlp")
                 ;; delphi/c++builder/radstudio http://docs.embarcadero.com/products/rad_studio/
                 ("rs009" chm "f:\\borland\\rs2009\\delphivclwin32.chm")
                 ;; fpc-lazarus-doc-chm: http://sourceforge.net/projects/lazarus/files/Lazarus%20Documentation/Lazarus%201.0/
                 ("fp-rtl"  chm "e:\\lazarus\\docs\\chm\\rtl.chm")
                 ("fp-fcl"  chm "e:\\lazarus\\docs\\chm\\fcl.chm")
                 ("lazarus-fcl" chm "e:\\lazarus\\docs\\chm\\lcl.chm"))                
    (emacs-lisp-mode ("chm" chm "e:\\emacs\\doc\\elisp-24.3.chm")
                     ("info" info)
                     ("web" web "http://www.emacswiki.org/cgi-bin/info-ref?find=%s"))
    (help-mode . emacs-lisp-mode)
    (ruby-mode   ("yari" elisp yari yari)
                 ;; ruby-doc-chm http://rubyforge.org/projects/rubyinstaller/
                 ("chm1.8" chm "d:\\Rails\\ruby18.chm")
                 ("chm1.9.3" chm "d:\\Rails\\ruby19.chm")
                 ;; rails 2.3 chm http://rubyforge.org/projects/rdoc-chm/
                 ("rails-2.3" chm "d:\\Rails\\Rails-2.3.2.chm")
                 ;; http://apidock.com/
                 ("apidocruby" web "http://apidock.com/ruby/search?query=%s")
                 ("apidocror" web  "http://apidock.com/rails/search?query=%s")
                 ("railsdock" web  "http://apidock.com/rails/search?query=%s"))                 
    (enh-ruby-mode . ruby-mode)    
    (xahk-mode ("default" chm  "d:\\Programs\\AutoHotkey\\AutoHotkey-chs.chm"))
    (ahk-mode . xahk-mode)
    (php-mode ("default" web "http://www.php.net/%s")      ;; refer http://www.php.net/urlhowto.php for more info
              ("chinese" web "http://www.php.net/zh/%s"))
    ))
    

(setq keyword-help-default-source-alist
      '((python-mode . "chm")
        (emacs-lisp-mode . "chm")
        (xahk-mode . "chm")))

(defun keyword-help--get-mode-config (majormode &optional noparent)
  (let ((cfg (cdr (assq majormode keyword-help-source-alist)))
        (parent-mode (get majormode 'derived-mode-parent)))
    (append (if cfg
                (if (symbolp cfg)
                    (keyword-help--get-mode-config cfg)
                  cfg))
           (if (and parent-mode (not noparent))
               (keyword-help--get-mode-config parent-mode)))))

(defun keyword-help--get-mode-sources (majormode)
  (append (keyword-help--get-mode-config majormode)
          (keyword-help--get-mode-config 'fundamental-mode)))

;;; User Commands

;;;###autoload
(defun keyword-help-lookup (keyword &optional source)
  "Invoke documentation query backends for KEYWORD.

Without prefix key, only the 'default' help source would be invoked.
If invoked with prefix key, it would let you choose which source to invoke."
  (interactive
   (list (read-string "Keyword: "
                      (if (and transient-mark-mode mark-active)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (thing-at-point 'symbol))
                      )
         (let* ((all-sources (keyword-help--get-mode-sources major-mode))
                (all-sources-names (mapcar 'car all-sources))
                (completing-read-func (if (fboundp 'ido-completing-read)
                                          'ido-completing-read
                                        'completing-read))
                (default-source (or (cdr (assq major-mode keyword-help-default-source-alist))
                                    "default")))
           (if (or current-prefix-arg
                   (not (member default-source all-sources-names)))
             (apply completing-read-func
                    "Source: "
                    all-sources-names
                    nil
                    t
                    nil)))))
  (let* ((all-sources (keyword-help--get-mode-sources major-mode))
         (source (or source
                       (cdr (assq major-mode keyword-help-default-source-alist))
                       "default"))
         (help-source (if all-sources                 
                   (assoc source all-sources)))
         (method (if help-source
                     (concat "keyword-help-lookup-" (symbol-name (nth 1 help-source)))
                   "keyword-help-lookup-default"))
         (params (if help-source
                     (cddr help-source))))
    (if help-source
        (if (intern-soft method)
            (progn
              (message "Calling '%s' for \"%s\" with params: %s" method keyword params)
              (apply (intern method) keyword params))
          (message "keyword-help: no backend configurated for: %s" (symbol-name (nth 1 help-source))))
      (message "No configuration for '%s' in `keyword-help-source-alist' (source:%s)" major-mode source)
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
		 ((string= ext "chm") (keyword-help-lookup-chm keyword help-file))
		 ((string= ext "hlp") (keyword-help-lookup-hlp keyword help-file))))
	    (message "Help file not exist: %s" help-file)))))
	))
  
(provide 'keyword-help)

  
