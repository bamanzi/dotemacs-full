;;* CEDET (and ECB

(defun find-dir-in-load-path-and-add-it (dirname recursively)
  "Try to find a dir named DIRNAME along all the paths in `load-path', and add it into `load-path'."
  (mapc #'(lambda(path)
            (let ( (full-path (expand-file-name dirname path)) )
              (when (file-directory-p full-path)
                (message "Adding %s to load-path. recursively=%s" full-path recursively)
                (let ( (default-directory full-path) )
                  (if recursively
                      (normal-top-level-add-subdirs-to-load-path)
                    (add-to-list 'load-path full-path))
                    ))))
        load-path)
  t
  )

;;** CEDET

(defun config-cedet-from-sf.net ()
  "Configure CEDET for Emacs <= 23.1 (no built-in cedet available."
  ;; See cedet/common/cedet.info for configuration details.
  ;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
  ;; CEDET component (including EIEIO) gets activated by another 
  ;; package (Gnus, auth-source, ...).
;;  (load-library "cedet") //FIXME!!!
)

(defun init-cedet-from-sf.net ()
  (interactive)
  
;;; Enable EDE (Project Management) features
  (global-ede-mode 1)

  ;; Enable EDE for a pre-existing C++ project
  ;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;;; Enabling Semantic (code-parsing, smart completion) features
;;; Select one of the following:

  ;; * This enables the database and idle reparse engines
  (semantic-load-enable-minimum-features)

  ;; * This enables some tools useful for coding, such as summary mode
  ;;   imenu support, and the semantic navigator
  (semantic-load-enable-code-helpers)

  ;; * This enables even more coding tools such as intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  ;; (semantic-load-enable-gaudy-code-helpers)

  ;; * This enables the use of Exuberent ctags if you have it installed.
  ;;   If you use C++ templates or boost, you should NOT enable it.
  ;; (semantic-load-enable-all-exuberent-ctags-support)
  ;;   Or, use one of these two types of support.
  ;;   Add support for new languges only via ctags.
  ;; (semantic-load-enable-primary-exuberent-ctags-support)
  ;;   Add support for using ctags as a backup parser.
  ;; (semantic-load-enable-secondary-exuberent-ctags-support)

;;; Misc
  ;; Enable SRecode (Template management) minor-mode.
  ;; (global-srecode-minor-mode 1)

  ;; tag folding
  (global-semantic-tag-folding t)
  )

(defun config-cedet-built-in ()
  "Configure CEDET for Emacs >= 23.2."
  (setq semantic-default-submodes
        '(
          global-semanticdb-minor-mode          ;; - Maintain tag database.
          global-semantic-idle-scheduler-mode   ;; - Reparse buffer when idle.
          ;; global-semantic-idle-summary-mode     ;; - Show summary of tag at point.
          ;; global-semantic-idle-completions-mode ;; - Show completions when idle.
          global-semantic-decoration-mode       ;; - Additional tag decorations.
          global-semantic-highlight-func-mode   ;; - Highlight the current tag.
          ;; global-semantic-stickyfunc-mode       ;; - Show current fun in header line.
          ;; global-semantic-mru-bookmark-mode     ;; - Provide `switch-to-buffer'-like
                                                ;;;  keybinding for tag names.
          ))
  )

(defvar bmz/always-use-cedet-from-sf.net nil
  "Always use CEDET package from http://cedet.sf.net.")

(if (string< emacs-version "23.2")
    (if (find-dir-in-load-path-and-add-it "cedet-1.0" t)
        (config-cedet-from-sf.net))
  (if (and bmz/always-use-cedet-from-sf.net
           (find-dir-in-load-path-and-add-it "cedet-snapshot" t))
      (config-cedet-from-sf.net)
    (config-cedet-built-in)))


;;FIXME: not work?
(defun bmz/restore-imenu-index-function ()
  "Restore `imenu-create-index-function' for current major-mode.
	
Note this would reset to `imenu-default-create-index-function',
but some modes have its own implementation."
  (interactive)
  (let* ((func-name (cond
                     ((eq major-mode 'python-mode)
                      "'python-imenu-create-index")
                     ((eq major-mode 'js2-mode)
                      "'python-imenu-create-index")
                     ((eq major-mode 'espresso-mode)
                      "'espresso--imenu-create-index")
                     (t
                      "'imenu-default-create-index-function")))
         (mode-expr (concat "(setq-mode-local " (symbol-name major-mode)
                            " imenu-create-index-function "
                            func-name
                            ")"))
         (buffer-expr (concat "(setq imenu-create-index-function " func-name  ")")))
    (message "Eval: %s" mode-expr)
    (eval mode-expr)
    (message "Eval: %s" buffer-expr)
    (eval buffer-expr)
    ))

(defun bmz/restore-imenu-index-function ()
  "Restore `imenu-create-index-function' for current major-mode.

Note this would reset to `imenu-default-create-index-function', but some modes
have its own implementation."
  (interactive)
  (let* ((func-name (cond
                    ((eq major-mode 'python-mode)
                     'python-imenu-create-index)
                    ((eq major-mode 'js2-mode)
                     'js2-mode-create-imenu-index)
                    ((eq major-mode 'espresso-mode)
                     'espresso--imenu-create-index)
                    (t
                     'imenu-default-create-index-function))))
    (eval `(setq-mode-local ,major-mode imenu-create-index-function
                     func-name))
    (setq imenu-create-index-function func-name))) 

;;** ECB

(if (string< emacs-version "23.2")
    (find-dir-in-load-path-and-add-it "ecb-2.40" nil)
  (if (find-dir-in-load-path-and-add-it "ecb-snap" nil)
      t
    (if bmz/always-use-cedet-from-sf.net
        (message "NOTE: ecb-2.40 can't be used with emacs' built-in cedet."))))

;; workaround for emacs >= 23.2
(unless (string< emacs-version "23.2")
  (setq stack-trace-on-error nil)
  )

;; If you want to load the complete ECB at (X)Emacs-loadtime
;; ( Advantage: All ECB-options available after loading ECB.
;; Disadvantage: Increasing loadtime2):
;;(require 'ecb)

;; If you want to load the ECB first after starting it by ecb-activate
;; ( Advantage: Fast loading.
;; Disadvantage: ECB- and semantic-options first available after starting ECB):
(idle-require 'ecb-autoloads)
(autoload 'ecb-activate "ecb" "Activates ECB and creates the special buffers for the choosen layout." t)

;; ECB is now ready for use and can be activated by calling
;; M-x ecb-activate or ecb-minor-mode


(setq ecb-tip-of-the-day nil)
(defadvice ecb-show-tip-of-the-day (around surpress-dialog)
  (interactive)
  (let ( (window-system nil)
         (ecb-tip-of-the-day t)
         )
    ad-do-it))
(ad-activate 'ecb-show-tip-of-the-day)

;; to support imenu on `methods' window
;; DOC: (info "(ecb) Non-semantic sources")
(setq speedbar-use-imenu-flag nil)

(eval-after-load "ecb"
  `(progn
     (setq ecb-toggle-layout-sequence '("left9" "left14"))
     
     ;; (add-to-list 'ecb-compilation-buffer-names
     ;;              '("*sdcv*"))
   
     ;;(setq ecb-mode-line-display-window-number nil)
     ;;(defadvice ecb-window-in-window-list-number (after ecb-window-number-compatible-with-window-numbering-mode)
     ;;  (1+ ad-return-value))
     ;;(ad-activate 'ecb-window-in-window-list-number)

     ;;redefine `ecb-window-in-window-list-number', to make it compatible with `window-numbering-mode'
     (defun ecb-window-in-window-list-number (win-list &optional window)
       (let ((win-number (ecb-position win-list (or window (selected-window)))))
         (if win-number (+ win-number 2) nil)))
     ))


      
