
(defun anything-info-awk ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-gawk )))

(eval-after-load "cc-mode"
  `(define-key awk-mode-map (kbd "<M-f1>") 'anything-info-awk))

(defun anything-info-sh-script ()
  (interactive)
  (anything
   :input (thing-at-point 'symbol)
   :prompt "Info about: "
   :candidate-number-limit 10
   :sources
   '( anything-c-source-info-bash
      anything-c-source-info-zsh
      anything-c-source-info-coreutils
      anything-c-source-info-sh-utils
      anything-c-source-info-fileutils
      anything-c-source-info-textutils
      anything-c-source-man-pages
      )))

(eval-after-load "sh-script"
  '(progn
     (define-key sh-mode-map      (kbd "M-s <f1>")  'anything-info-sh-script)
     ))
