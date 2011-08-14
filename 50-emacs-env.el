(global-set-key (kbd "<C-f10> g") 'customize-group)
(global-set-key (kbd "<C-f10> v") 'customize-variable)
(global-set-key (kbd "<C-f10> f") 'customize-face)
(global-set-key (kbd "<C-f10> t") 'customize-themes)


(global-set-key (kbd "<f12> l l") 'load-library)
(global-set-key (kbd "<f12> l t") 'load-theme)


(defun load-and-execute (library)
  "load a library 'foobar' and execute the command with same name (foobar or foobar-mode"
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



;;;_ S(@* "eshell")


;;;_ S(@* "dired")

;;;_ S(@* "ediff")






