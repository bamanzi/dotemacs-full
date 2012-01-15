;;* Query Documents
(if (eq window-system 'windows-nt)
    (load "keyword-help" 'noerror))

;;** `info'
;;*** `info-lookup-symbol'

;;*** anything-info
(defun anything-info-emacs ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-emacs
         anything-c-source-info-elisp
        ;; anything-c-source-info-emacs-lisp-intro
         )))


;;** devhelp
;; Emacs integration by Richard Hult <richard@imendio.com>
;;

(defun devhelp-word-at-point ()
  "Searches for the current word in Devhelp"
  (interactive)
  (start-process-shell-command "devhelp" nil "devhelp" "-s" (current-word))
  (set-process-query-on-exit-flag (get-process "devhelp") nil)
  )
(defun devhelp-assistant-word-at-point ()
  "Searches for the current work in the Devhelp assistant"
  (interactive)
  (setq w (current-word))
  (start-process-shell-command "devhelp" nil "devhelp" "-a" w)
  (set-process-query-on-exit-flag (get-process "devhelp") nil)
  )

(defvar devhelp-timer nil)
(defun devhelp-disable-assistant ()
  (message "Devhelp assistant disabled")
  (cancel-timer devhelp-timer)
  (setq devhelp-timer nil)
)
(defun devhelp-enable-assistant ()
  (message "Devhelp assistant enabled")
  (setq devhelp-timer (run-with-idle-timer 0.6 t 'devhelp-assistant-word-at-point))
)
(defun devhelp-toggle-automatic-assistant ()
  "Toggles automatic Devhelp assistant on and off"
  (interactive)
  (if devhelp-timer (devhelp-disable-assistant) (devhelp-enable-assistant))
)

;; Examples:
;;
;; Bind F7 to start devhelp and search for the word at the point.
;;(global-set-key [f11] 'devhelp-word-at-point)
;;
;; Bind F6 to enable the automatic assistant.
;; (global-set-key [f6] 'devhelp-toggle-automatic-assistant)
;;
;; Bind F6 to search with the assistant window.
;; (global-set-key [f6] 'devhelp-assistant-word-at-point)


;;** Emacs: Perl PHP Dictionary Wikipedia Google … Reference lookup -
;; http://xahlee.org/emacs/emacs_lookup_ref.html

(defun lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches you to your browser."
 (interactive)
 (let (myWord myUrl)
   (setq myWord
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "_" myWord))
  (setq myUrl (concat "http://en.wikipedia.org/wiki/" myWord))
  (browse-url myUrl)
   ))
