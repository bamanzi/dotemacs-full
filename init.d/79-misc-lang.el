;;** awk
(defun anything-info-awk ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-gawk )))

(eval-after-load "cc-mode"
  `(define-key awk-mode-map (kbd "<M-f1>") 'anything-info-awk))

;;** shell script
(defun anything-info-shell-script ()
  (interactive)
    (anything
      :prompt "Info about: "
      :candidate-number-limit 10
      :sources
      '( anything-c-source-info-bash
;;         anything-c-source-info-zsh
;;         anything-c-source-info-sh-utils
         anything-c-source-info-coreutils
;;         anything-c-source-info-textutils
;;         anything-c-source-info-fileutils
         anything-c-source-man-pages
         )))
;;
