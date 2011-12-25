
(defun insert-function-autoload-spec (function)
  "Insert the first line of documentation of a function.

Useful when writing autoload spec."
  (interactive
   (let ((fn (function-called-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read (if fn
				    (format "Describe function (default %s): " fn)
				  "Describe function: ")
				obarray 'fboundp t nil nil
				(and fn (symbol-name fn))))
     (list (if (equal val "")
	       fn (intern val)))))
  (if (null function)
      (message "You didn't specify a function")
    (insert-string (format " \"%s\"\n  \"%s\" t)"
                           (replace-regexp-in-string ".elc?$" "" (file-name-nondirectory (symbol-file function 'defun)))
                           (or (eldoc-docstring-first-line (documentation function t))
                               "Undocumented.")    ))))

(defalias 'ifas 'insert-function-autoload-spec)

(defun mode-line-install-element (element &optional position)
  "Install an ELEMENT to mode-line.

POSITION could be an integer or another element (such as `mode-line-buffer-identification').
New ELEMENT would be installed *after* POSITION."
  (let ((mode-line (default-value 'mode-line-format))
        (res)
	(position (or position 1)))
    (while (and position mode-line)
      (push (car mode-line) res)
      (if (integerp position)
	  (progn
	    (setq position (- position 1))
	    (if (eq position 0)
		(setq position nil)))
	(if (equal (car mode-line) position)
	    (setq position nil)))
      (pop mode-line))
    (push element res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res))
    (force-mode-line-update t))
    )


(defun mode-line-uninstall-element (element)
  "Remove an ELEMENT from the mode-line.

ELEMENT could be the same value of an existing element,
or the car of it, if that element is a cons cell.

e.g. 
 (mode-line-uninstall-element 
 '(which-func-mode
  ("" which-func-format
   #(" " 0 1
     (help-echo ...))))
or just:
  (mode-line-uninstall-element 'which-func-mode)
"
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (if (consp item)
                    (equal (car item) element)
                  (equal item element))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))


(provide 'bmz-misc)

