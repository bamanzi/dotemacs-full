;;* Org-mode

(setq org-CUA-compatible t)

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-support-shift-select 'always

      org-return-follows-link nil             ;; RET on link woule open it (use C-j if you want newline)
      org-src-fontify-natively t          ;; syntax highlighting the source code
      
      org-use-sub-superscripts nil        ;; don't use `_' for subscript
      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil             ;; no ToC in export
      org-export-with-author-info nil     ;; no author info in export
      org-export-with-creator-info nil    ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css
      org-export-html-inline-images nil   ;; supress inline images
      )

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c q") 'org-capture)


;;** buffer-local faces for org-mode
(defun font-exists-p (font)
  "Test if FONT is available."
   (if (null (list-fonts (font-spec :family font)))
              ;; 2008-02-26 function of the new font backend (Emacs 23),
              ;; instead of `x-list-fonts'
       nil
     t))

;;Use some specific monospace font, so that cells in orgtbl would align well
;; when Chinese/English are mixed
(defun org-mode-init-face ()
  (message "Customize face `variable-pitch' to change fonts for `org-mode'.")
  (if (font-exists-p "Inconsolata")
      (set-face-attribute 'variable-pitch nil
                          :family  "Inconsolata"
                          :foundry "outline"
                          :font    "-*-Inconsolata-normal-normal-normal-*-15-*-*-*-m-*-iso10646-1")
;;                          :fontset "-*-Inconsolata-normal-normal-normal-*-15-*-*-*-m-*-fontset-*")
    (if (font-exists-p "Consolas")
        (set-face-attribute 'variable-pitch nil
                          :family  "Consolas"
                          :foundry "Consolas"
                          :font    "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1")
      (message "NOTE: No suitable font found for org-mode. ")))
  
  (set (make-variable-buffer-local 'line-spacing) 2) 
  (buffer-face-mode t)

  (bmz/init-org-level-faces (selected-frame))
  )

(defun bmz/init-org-level-faces (frame)
  (interactive (list (selected-frame)))
  (set-face-attribute 'org-level-1 frame :height 1.5 :bold t)
  (set-face-attribute 'org-level-2 frame :height 1.3 :bold t)
  (set-face-attribute 'org-level-3 frame :height 1.1))

;;FIXME: not work?
(add-hook 'org-mode-hook 'org-mode-init-face)
(add-hook 'after-make-frame-functions 'bmz/init-org-level-faces)

;;** export org-mode to markdown format
(eval-after-load "org"
  `(progn
     (when (require 'org-export-generic nil t)
         ;; https://github.com/alexhenning/ORGMODE-Markdown
       (org-set-generic-type
        "Markdown"
        '(:file-suffix ".markdown"
                       :key-binding ?M
                       :title-format "%s\n"
                       :title-suffix ?=
                       :body-header-section-numbers t
                       :body-header-section-number-format "%s) "
                       :body-section-header-prefix ("\n## " "\n### " "\n#### " "\n##### " "\n###### ")
                       :body-section-header-format "%s"
                       :body-section-header-suffix "\n"
                       :todo-keywords-export t
                       :body-line-format " %s\n"
                       :body-tags-export t
                       :body-tags-prefix " <tags>"
                       :body-tags-suffix "</tags>\n"
                       ;;:body-section-prefix "<secprefix>\n"
                       ;;:body-section-suffix "</secsuffix>\n"
                       :body-line-export-preformated t
                       :body-line-fixed-prefix "<pre>\n"
                       :body-line-fixed-suffix "\n</pre>\n"
                       :body-line-fixed-format "%s\n"
                       :body-list-prefix "\n"
                       :body-list-suffix "\n"
                       :body-list-format " * %s\n"
                       ;;:body-number-list-prefix "<ol>\n"
                       ;;:body-number-list-suffix "</ol>\n"
                       ;;:body-number-list-format "<li>%s</li>\n"
                       ;;:body-number-list-leave-number t
                       :body-list-checkbox-todo "[_] "
                       :body-list-checkbox-todo-end ""
                       :body-list-checkbox-done "[X] "
                       :body-list-checkbox-done-end ""
                       :body-line-format "%s"
                       :body-line-wrap 75
                       :body-text-prefix ""
                       :body-text-suffix ""
                       ))
       )))

;;** misc
;;FIXME: '<s' template
(defun org-quote-region (begin end)
  (interactive "r")
  (if (not (and transient-mark-mode mark-active))
      (error "You should make a region")
    (let* ( (choices '("SRC" "EXAMPLE" "QUOTE" "HTML" "VERSE" "COMMENT" "LATEX"
                    "*" "_" "/" "=" "~"
                    ))
            (choice (ido-completing-read "With:" choices)) )
      (if (member choice '("*" "_" "/" "=" "~"))
          (progn
            (goto-char end)
            (insert-string choice)
            (goto-char begin)
            (insert-string choice))
        (progn
          (goto-char end)
          (insert-string (concat "\n#+END_" choice "\n"))
          (goto-char begin)
          (insert-string (concat "#+BEGIN_" choice "\n")))))))

;;** markdown mode
(autoload 'markdown-mode  "markdown-mode"
  "Major mode for editing Markdown files." t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(eval-after-load "markdown-mode"
  `(progn
     (set-face-attribute 'markdown-header-face-1 nil :inherit 'org-level-1)
     (set-face-attribute 'markdown-header-face-2 nil :inherit 'org-level-2)
     (set-face-attribute 'markdown-header-face-3 nil :inherit 'org-level-3)
     (set-face-attribute 'markdown-header-face-4 nil :inherit 'org-level-4)
     (set-face-attribute 'markdown-header-face-5 nil :inherit 'org-level-5)
     (set-face-attribute 'markdown-header-face-6 nil :inherit 'org-level-6)

     (add-hook 'markdown-mode-hook 'buffer-face-mode) 
     ))



