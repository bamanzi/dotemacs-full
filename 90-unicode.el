
;;** auto-detect file encoding
;;(idle-require 'fenc)  ;;only for different Chinese encodings
;;(idle-require 'unicad)   ;; an elisp port of Mozilla Universal Charset Auto Detector

;;** input special char
;;*** The simplest way to input Chinese punctuation:
;;    (set-input-method 'chinese-punct)
;; then when you type '9', you have choices: ９　⒐　⒚　⑼ ⒆ ⑨ ㈨ Ⅸ
;;      when you type '[', you have choices: ［　〔　〖　【　｛

;;** some utils
;;*** Xah Lee's unicode-browser-mode
(autoload 'xub-mode "xub-mode" "Load xub-mode for browsing Unicode." t)
(defalias 'unicode-browser 'xub-mode)

;;** misc

(global-unset-key (kbd "C-v"))
(global-set-key (kbd "C-v 、") "/")
(global-set-key (kbd "C-v －") "-")


