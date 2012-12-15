;;* language tools: dictionary, translations
;;** DICT protocol
;;*** dictionary.el (recommended)
(setq dictionary-server "localhost")
;;TIP: to searc in specific dictionary, use: C-u M-x dictionary-search
;;(setq dictionary-default-dictionary "xdict")
;;(setq dictionary-default-dictionary "stardict")
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)

;;*** dictem.el
;; external utility `dict' client needed
(setq dictem-server "dict.org")
(autoload 'dictem-run-search "dictem"
  "Asks a user about database name, search strategy and query," t)
(autoload 'dictem-run-match "dictem"
  "Asks a user about database name, search strategy and query," t)
(autoload 'dictem-run-define "dictem"
  "Asks a user about database name and query," t)

(progn
  (define-key search-map (kbd "M-d s") 'dictem-run-search)
  (define-key search-map (kbd "M-d m") 'dictem-run-match)
  (define-key search-map (kbd "M-d d") 'dictem-run-define)
  )

(defun dictem-mode-outline ()
  (setq outline-regexp "^From.* \\[.*\\]:")
  (outline-minor-mode 1)
  (highlight-lines-matching-regexp "^From.*" 'outline-1))

(eval-after-load "dictem"
  `(progn
     (dictem-initialize)
     (add-hook 'dictem-mode-hook 'dictem-mode-outline)))


;;*** dict.el (external program `dict' needed)
;;;  (NOTE: it's hard to use it on windows)
(setq dict-servers '("localhost" "dict.org"))
;;(setq dict-enable-key-bindings t)
;;(setq dict-databases '("gcide" "pydict"))
(autoload 'dict "dict" "Lookup a WORD on dict.org." t)


;;** stardict

;;*** sdcv-mode.el
(autoload 'sdcv-search "sdcv-mode" nil t)

;;*** sdcv.el
;;NOTE: use the one I fixed: https://bitbucket.org/bamanzi/site-lisp/fixed/
(autoload 'sdcv-search-detail "sdcv"
  "Search WORD through the `command-line' tool sdcv." t)
(define-key search-map "D"  'sdcv-search-detail)

(defun sdcv-search-word-at-pt-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (require 'sdcv)
  ;;(setq sdcv-dictionary-simple-list '("XDICT英汉辞典" "XDICT汉英辞典"))
  (call-interactively 'sdcv-search-pointer+))

(global-set-key (kbd "<C-down-mouse-1>") 'sdcv-search-word-at-pt-mouse)
(define-key search-map "d"  'sdcv-search-word-at-pt-mouse)


;;** web dictionary
;;from: http://xahlee.org/emacs/emacs_lookup_ref.html
(defun lookup-word-definition (service-url)
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
 (interactive "sService URL:")
 (let (myWord myUrl)
   (setq myWord
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "%20" myWord))
  ;;(setq myUrl (concat "http://www.answers.com/main/ntquery?s=" myWord))
  ;;(setq myUrl (concat "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=" myWord))
  (setq myUrl (format service-url myWord))

  (if (fboundp 'w3m-browse-url)
      (w3m-browse-url myUrl) ;; if you want to browse using w3m
    (browse-url myUrl))
   ))

;; List of Online Dictionaries
;; http://xahlee.org/PageTwo_dir/Vocabulary_dir/dictionary_tools.html

;;dict.org provides detailed explanation
(defun lookup-word-definition-dict.org ()
  (interactive)
  (lookup-word-definition "http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=%s"))

;;wiktionary provides brief explanation
(defun lookup-word-definition-wiktionary-en ()
  (interactive)
  (lookup-word-definition "http://en.wiktionary.org/wiki/%s"))

(defun lookup-word-definition-wiktionary-zh ()
  (interactive)
  (lookup-word-definition "http://zh.wiktionary.org/wiki/%s"))

;;** spell
;;

;;** speek/synthesizer

;;** translation
;;TODO: babel.el
(defun google-translate (tolang)
  "Translate current word's to another language with Google Translate service.

If a region is active (a phrase), lookup that phrase."
 (let (myWord myUrl)
   (setq myWord
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myWord (replace-regexp-in-string " " "%20" myWord))
  
  (setq myUrl (concat "http://translate.google.com/m?hl=zh-CN&sl=auto&tl=" tolang
                      "&ie=UTF-8&q=" myWord))
  
  (browse-url myUrl)
  ;; (w3m-browse-url myUrl) ;; if you want to browse using w3m
   ))
