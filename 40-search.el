;;** search (within buffer)

;;make the keybinding more natural
(eval-after-load "isearch"
  `(progn
     (define-key isearch-mode-map (kbd "C-y")     'isearch-yank-kill) ;;before emacs-24: `isearch-yank-line'
     (define-key isearch-mode-map (kbd "M-y")     'isearch-yank-pop)  ;;before emacs-24: `isearch-yank-kill'
     (define-key isearch-mode-map (kbd "M-s C-e") 'isearch-yank-line)
     (define-key isearch-mode-map (kbd "M-s M-f") 'isearch-yank-word)
     ))


;;(define-key global-map (kbd "<C-f3>") 'isearch-repeat-forward)
;;(define-key global-map (kbd "<S-f3>") 'isearch-repeat-backward)


;;** search selection
(defun bmz/search-selection-forward ()
  (interactive)
  (let ( (symbol (bmz/get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-forward)
  ))

(defun bmz/search-selection-backward ()
  (interactive)
  (let ( (symbol (bmz/get-symbol-selected-or-current)) )
    (deactivate-mark)
    (setq isearch-string symbol)
    (call-interactively 'isearch-repeat-backward)
  ))

(define-key global-map (kbd "<C-f3>")   'bmz/search-selection-forward)
(define-key global-map (kbd "<S-f3>")   'bmz/search-selection-backward)

;;** search current symbol
(autoload 'highlight-symbol-next  "highlight-symbol"
  "Jump to the next location of the symbol at point within the function." t)
(autoload 'highlight-symbol-prev  "highlight-symbol"
  "Jump to the previous location of the symbol at point within the function." t)

(define-key search-map (kbd "*") 'highlight-symbol-next)
(define-key search-map (kbd "#") 'highlight-symbol-prev)

;;** occur
(defun bmz/occur-at-point (nlines)
  (interactive "P")
  (occur (format "%s" (bmz/get-symbol-selected-or-current)) nlines))

(defun bmz/multi-occur-at-point (nlines)
  (interactive "P")
  ;;FIXME: use multi-occur?
  (multi-occur nil (format "%s" (bmz/get-symbol-selected-or-current)) nlines))


;;*** multi-occur in same mode buffers
;; stolen from http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun bmz/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (format "%s" (bmz/get-symbol-selected-or-current))))
  

;;** grep
(defun bmz/grep-symbol-at-point-same-ext()
  (interactive)
  (grep (format "grep -nH %s *.%s %s"
		(bmz/get-symbol-selected-or-current)
        (file-name-extension buffer-file-name)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

(defun bmz/grep-symbol-at-point()
  (interactive)
  (grep (format "grep -nH %s *.* %s"
		(bmz/get-symbol-selected-or-current)
		"--exclude \"#*.*\" --exclude \"*.*~\""
		)))

;;** ack: a better grep  http://betterthangrep.com/
;;;  - it would ignore .svn, CVS etc by default
;;;  - it would ignore binary files, core dumps, backup files by default
;;;  - to limit search in some file types, you can easily use `--type=perl'
;;;    rather than combining grep with `find'
;;;
(autoload 'ack "ack" "Use ack where you might usually use grep." t)
(autoload 'ack-mode "ack" "Use ack where you might usually use grep." nil)

(defun bmz/ack-at-point (typep recursively)
  (require 'ack)
  (let ( (command (concat (if typep (ack-build-command)
                            ack-command)
                          (if recursively " -r "
                              " ")
                          (bmz/get-symbol-selected-or-current))) )
    (compilation-start command 'ack-mode)))

(defun bmz/ack-at-point-in-same-type-files (arg)  
  "Use `ack' to search current symbol in same type files.

If ARG given, search recursively."
  (interactive "P")
  (bmz/ack-at-point 'same-type arg))

(defun bmz/ack-at-point-in-all-files (arg)
    "Use `ack' to search current symbol in all files.

If ARG given, search recursively."
  (interactive "P")
  (bmz/ack-at-point nil arg))

;;** hidesearch
(autoload 'hidesearch "hidesearch"
  "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines"
  "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

(defadvice hide-non-matching-lines (after highlight-pattern
                                          (search-text) activate)
  (let ((faces '(hi-yellow hi-pink hi-green hi-blue
                           hi-black-b hi-blue-b hi-green-b hi-red-b hi-black-hb)))
    (highlight-regexp search-text (car (nth (random (length faces)))))))


;;FIXME: anything-occur is better?

;;** ace-jump-mode
(autoload 'ace-jump-line-mode  "ace-jump-mode"
  "AceJump line mode." t)
(autoload 'ace-jump-char-mode "ace-jump-mode"
  "AceJump char mode" t)

(define-key goto-map "l"  'ace-jump-line-mode)
(define-key goto-map " "  'ace-jump-mode)

