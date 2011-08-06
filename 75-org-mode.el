;;(@* "org-mode")

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
