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
(setq dictem-server "localhost")
(autoload 'dictem-run-search "dictem" nil t)
(autoload 'dictem-run-match  "dictem" nil t) 

(eval-after-load "dictem"	  `(dictem-initialize))

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
;;FIXME: problem


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
