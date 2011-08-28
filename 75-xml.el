
;;;_. for `sgml-pretty-print'
(autoload 'sgml-pretty-print "sgml-mode" "Simple-minded pretty printer for SGML" t)


;;;_.  Showing XPath in modeline
;; http://www.emacswiki.org/emacs/NxmlMode
(defun nxml-get-where ()
  "Get the hierarchy of XML elements the point is on as a path."
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (condition-case nil
                   (progn
                     (nxml-backward-up-element) ; always returns nil
                     t)       
                 (error nil))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (mapconcat 'identity path "/")))))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (message "/%s" (nxml-get-where)))  

(defun which-func-for-xml ()
  (if (memq major-mode '(nxml-mode))
      (nxml-get-where)))

(add-hook 'which-func-functions 'which-func-for-xml)
(add-to-list 'which-func-modes 'nxml-mode)


;;;_. goto match tag
(defun xml-goto-match-tag ()
  (interactive)
  (require 'sgml-mode)
  (let ( (symbol (thing-at-point 'symbol)) )
    (if (not (and symbol (string= "<" (substring symbol 0 1))))
        (message "Cursor should be place on an opening or closing tag")
      (progn
       (unless (string= "<" (thing-at-point 'char))
         (re-search-backward "<"))
       (forward-char 1)
       (if (string= "/" (thing-at-point 'char))  ;;now on a closing tag
           (progn
             (sgml-skip-tag-backward 1)
             (forward-char 1)
             (point))
         (progn   ;;now on an opening tag
           (let ( (tagname (sgml-parse-tag-name)) )
             (sgml-beginning-of-tag)
             (backward-char 2)
             (sgml-skip-tag-forward 1)
             (backward-sexp 1)

             (if (looking-at (concat "</" tagname))
                 (forward-char 2)
               (message "This tag has no ending part.")))))))))

(eval-after-load "nxml"
  '(define-key nxml-mode-map (kbd "M-g %") 'xml-goto-match-tag))


