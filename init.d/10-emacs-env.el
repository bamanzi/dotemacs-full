
;;** options
;;(setq custom-unlispify-tag-names nil)
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

(global-set-key (kbd "<C-f10> g") 'customize-group)
(global-set-key (kbd "<C-f10> v") 'customize-variable)
(global-set-key (kbd "<C-f10> f") 'customize-face)
(global-set-key (kbd "<C-f10> t") 'customize-themes)

(global-set-key (kbd "<C-f10> F") 'menu-set-font)

(global-set-key (kbd "<mode-line> <C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<mode-line> <C-wheel-down>") 'text-scale-decrease)


;;** emacs-lisp
(global-set-key (kbd "<f12> l l") 'load-library)
(global-set-key (kbd "<f12> l t") 'load-theme)

(global-set-key (kbd "<C-f10> d") 'toggle-debug-on-error)

(defun load-and-execute (library)
  "load a library 'foobar' and execute the command with same name (`foobar' or `foobar-mode')"
  (interactive
   (list (completing-read "Load library: "
                          (apply-partially 'locate-file-completion-table
                                           load-path
                                           (get-load-suffixes)))))
  (when (load library)
    (let ( (command (if (fboundp (intern library))
                        (intern library)
                      (intern (concat library "-mode")))) )
      (message "try to execute `%s'" command)
      (call-interactively command))))

(global-set-key (kbd "M-X") 'load-and-execute)


;;** some debugging tricks
;; let `message' and `error' print where it comes from
  (defadvice message (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)

  (defadvice error (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


(ad-disable-advice 'error 'before 'who-said-that)
(ad-update 'error)

(defun toggle-debug-on-error/bmz ()
  (interactive)
  (if debug-on-error
      (progn ;;turn if off
        (setq debug-on-error nil)
        (ad-deactivate 'error)
        (ad-deactivate 'message))
    (progn  ;;turn it on
      (setq debug-on-error t)
      (ad-activate 'error)
      (ad-activate 'message)))
  (message "Debug on Error %s globally" (if debug-on-error
                                            "enabled"
                                          "disabled")))

(global-set-key (kbd "<C-f10> d") 'toggle-debug-on-error/bmz)


;;** helps
(idle-require 'help-mode)  ;;on linux sometime C-h v/f would complain 'help-setup-xref is void'
(add-hook 'help-mode-hook 'visual-line-mode)
(define-key help-map "F"  'describe-face)
(define-key help-map "\C-h" nil) ;;force '<f1> C-h' to list keymap of `help-map'

(idle-require 'help-fns+)
(autoload 'describe-command  "help-fns+"
  "Describe an Emacs command (interactive function)." t)
(define-key help-map "K"   'describe-key-briefly)
(define-key help-map "c"   'describe-command)


(unless (fboundp 'describe-package)
  (defalias 'describe-package 'finder-commentary) ;;not a good alias
)

(defun describe-major-mode ()
  (interactive)
  (let ( (mode major-mode) )
    (with-help-window
        (format "%s" mode)
      (insert-string (describe-function mode)))))

(defun describe-major-mode ()
  (interactive)
  (describe-function major-mode))

(define-key help-map "M"  'describe-major-mode)


(defun show-variable-value (var)
  (interactive
     (let ((symb                          (or (and (fboundp 'symbol-nearest-point)
                                                   (symbol-nearest-point))
                                              (and (symbolp (variable-at-point)) (variable-at-point))))
           (enable-recursive-minibuffers  t)
           val)
       (setq val  (completing-read "variable: " obarray
                                   (if current-prefix-arg
                                       (lambda (vv) (user-variable-p vv))
                                     (lambda (vv) (or (boundp vv) (get vv 'variable-documentation))))
                                   t
                                   (symbol-name symb)
                                   nil (and (symbolp symb) (symbol-name symb))))
       (list (if (equal val "") symb (intern val)))))
  (message "%s: %s" (symbol-name var) (symbol-value var)))

(define-key help-map "V" 'show-variable-value)

;;*** describe file
(autoload 'describe-file  "help-fns+"
  "Describe the file named FILENAME." t)

(define-key help-map (kbd "M-f") 'describe-file)

(defun describe-this-file ()
  (interactive)
  (require 'help-fns+)
  (if buffer-file-name
      (describe-file buffer-file-name)
    (message "file not saved. ")))

;;help-fns+:  C-h M-f - describe-file
(define-key help-map (kbd "M-F") 'describe-this-file)

;;*** describe keymap
(autoload 'describe-keymap  "help-fns+"
  "Describe bindings in KEYMAP, a variable whose value is a keymap." t)

(define-key help-map (kbd "M-k") 'describe-keymap)

;;a lightweight implementation
(defun describe-keymap- (keymap)
    (interactive
     (list (intern (completing-read "Keymap: " obarray
                                    (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
                                    t nil 'variable-name-history))))
    (with-output-to-temp-buffer "*Help*"
      (princ (substitute-command-keys (concat "\\{" (symbol-name keymap) "}")))
      ))
(define-key help-map (kbd "M-k") 'describe-keymap-)

(define-key help-map " "  #'(lambda ()
                              (interactive)
                              (describe-keymap help-map)))  ;;help-fns+ needed?

;;** info
(define-key help-map "i"  nil)
(define-key help-map "ii" 'info)
(define-key help-map "ia" 'info-apropos)
(define-key help-map "is" 'info-lookup-symbol)
(define-key help-map "im" 'info-emacs-manual)
(define-key help-map "ic" 'Info-goto-emacs-command-node)
(define-key help-map "ik" 'Info-goto-emacs-key-command-node)
(define-key help-map (kbd "i l") 'elisp-index-search)  ;;info-lookup-symbol is better?

(idle-require 'info+)

(add-hook 'Info-mode-hook #'(lambda ()
                              ;; cancel binding to `Info-history-forward' and `Info-history-back'
                              (define-key Info-mode-map (kbd "<mouse-4>") nil)
                              (define-key Info-mode-map (kbd "<mouse-5>") nil)
                              ))


;;Read specific info file, similar to C-u M-x info
(defun info-view-file (file-or-node &optional buffer)
  "Read specific info file."
  (interactive (list
                (read-file-name "Info file name: " nil nil t)
                (if (numberp current-prefix-arg)
                    (format "*info*<%s>" current-prefix-arg))))
  (info-setup file-or-node
	      (pop-to-buffer-same-window (or buffer "*info*"))))
(define-key help-map "i " 'info-view-file)

;;** apropos
(define-key help-map "a"  nil)
(define-key help-map "aa" 'apropos)
(define-key help-map "ac" 'apropos-command)
(define-key help-map "ad" 'apropos-documentation)
(define-key help-map "ai" 'info-apropos)
(define-key help-map "al" 'apropos-library)
(define-key help-map "ao" 'apropos-user-options)
(define-key help-map "ag" 'apropos-group)
(define-key help-map "av" 'apropos-variable)
(define-key help-map "aV" 'apropos-value)
(autoload 'sys-apropos "sys-apropos" nil t)
(define-key help-map "as" 'sys-apropos)


;;** ediff

;;*** command line args support
;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))
(add-to-list 'command-switch-alist '("-diff" . command-line-diff))  ;;FIXME: which one?

;;*** window configuration
;;I don't like multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)
;; split the window depending on the frame width:
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))

;;*** ediff-buffer-with-file
;;stolen from http://www.loveshack.ukfsn.org/emacs/fx-misc.el
(defun ediff-buffer-with-file()
  "Run Ediff between the (modified) current buffer and the buffer's file.

A new buffer is created containing the disc file's contents and
`ediff-buffers' is run to compare that with the current buffer."
  (interactive)
  (unless (buffer-modified-p)
    (error "Buffer isn't modified"))
  (let ((current (buffer-name))
        (file (or (buffer-file-name)
                  (error "Current buffer isn't visiting a file")))
        (mode major-mode))
    (set-buffer (get-buffer-create (generate-new-buffer-name
                                    (concat current "-on-disc"))))
    (buffer-disable-undo)
    (insert-file-contents file)
    (set-buffer-modified-p nil)
    (funcall mode)
    (ediff-buffers (buffer-name) current)))

;;** commands
;;*** command log
;; http://www.foldr.org/~michaelw/emacs/mwe-log-commands.el
;;(idle-require 'mwe-log-commands)

;;*** command frequency
;;http://xahlee.org/emacs/command-frequency.html


;;** scratch
;;*** append scratch buffer into a file automatically
;;https://github.com/wakaran/scratch-log
(idle-require 'scratch-log)

;; another implementation: How to Make Emacs' Scratch Buffer Persistent Across Sessions 
;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html

;;*** go to scratch buffer
(defun goto-scratch-buffer-on-botton-window ()
  (interactive)
  (require 'windmove)
  (let ( (win (selected-window)) )
    (while (windmove-find-other-window 'down nil win)
      (setq win (windmove-find-other-window 'down nil win)))
    (when win
      (select-window win)
      (switch-to-buffer "*scratch*"))))

;;(global-set-key (kbd "<f11> s") 'goto-scratch-buffer-on-botton-window)

;;*** multi-scratch
;;TODO: multi-scratch

;;*** scratch-ext.el
;;TODO: scratch-ext.el


;;** "some aliases"
(defalias 'fl 'find-library)
(defalias 'll 'load-library)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

